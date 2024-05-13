#-------------------------------------------------------------------------------
# Ranking simulation over the second half of the league
#-------------------------------------------------------------------------------
# Let's assume to have data from first half of the league
# 1) Full simulation: predict results and incorporate predicted goals in dataset
#                     update priors with them and go on simulating
#                     -> Not tractable: model predicts goal diffs, not exact goals
# 2) Make preds and assign points to the teams basing on the predicted results
#    To predict next matchday I'll use the real results to update my knowledge
#    -> This approach allows to exploit the models learnt in prev online learning
#-------------------------------------------------------------------------------
# Libraries and utils
library(tidyverse)
library(dplyr)
source("utils/my_skellam.R")
# Import and prepare data
SerieA_2324<- read.csv(file="data/season_2324/SerieA_2324.csv")
SerieA_2324<- SerieA_2324[,c("HomeTeam","AwayTeam","FTHG","FTAG")]
teams<- unique(SerieA_2324$HomeTeam)
teams<- str_replace_all(teams, " ", "")
n_teams<- length(teams)
SerieA_2324$ht= unlist(sapply(1:nrow(SerieA_2324),function (g) which(teams==SerieA_2324$HomeTeam[g])))
SerieA_2324$at= unlist(sapply(1:nrow(SerieA_2324),function (g) which(teams==SerieA_2324$AwayTeam[g])))

# Rankings after the first half of the league
teams_pts <- data.frame(Team=teams,Pts=rep(0,n_teams),Lower=rep(0,n_teams),Upper=rep(0,n_teams))
for (t in 1:n_teams){
  n_wins= SerieA_2324[1:190,] %>% filter(((ht==t) &(FTHG>FTAG)) | ((at==t) & (FTHG<FTAG)))%>% nrow() 
  n_draws= SerieA_2324[1:190,] %>% filter(((ht==t) | (at==t)) & (FTHG == FTAG))%>% nrow() 
  teams_pts$Pts[t]= 3*n_wins+n_draws
}
# Since in the first half we are not estimating anything, Lower=Upper=Pts
teams_pts$Lower<- teams_pts$Pts
teams_pts$Upper<- teams_pts$Pts

# Iteration over the 2nd half
for (m in 20:35){
  cat(paste0("...Simulation of matchday n. ",m,"...\n"))
  #-------------------------------------------------
  # (1) Get the current matchday to predict
  test_set=SerieA_2324[(10*m -9):(10*m),]
  test_set=na.omit(test_set) #just to manage postponed matches in last batch
  #-------------------------------------------------
  # (2) Load the most recent model and some of its stuff
  cat("...Loading the model and retrieving useful info...\n")
  load(paste0("estimated_models/online_models/matchday",m-1,"/KN_matchday",m-1,".rds"))
  posterior<- as.array(KN_model)
  n_iters=dim(posterior)[1]
  n_chains=dim(posterior)[2]
  mu = posterior[,,"mu"]
  home=posterior[,,"home_advantage"]
  p = posterior[,,"p"]
  #-------------------------------------------------
  # (3) Make predictions
  cat("...Computing predictions and updating rankings...\n")
  for(m in 1:nrow(test_set)){
    ht=test_set$ht[m]
    at=test_set$at[m]
    attH=posterior[,,paste0("att[",ht,"]")]
    defH=posterior[,,paste0("def[",ht,"]")]
    attA=posterior[,,paste0("att[",at,"]")]
    defA=posterior[,,paste0("def[",at,"]")]
    theta_H = exp(mu+home+attH+defA)
    theta_A = exp(mu+attA+defH)
    GD<- mapply(my_rzeroinflatedskellam, 1,theta_H, theta_A,p)
    
    # Now lower and upper are from the p.o.v of the Home team
    GD_mean<- GD %>% mean() %>% round()
    GD_lower<- GD %>% quantile(probs =0.025) %>% round()
    GD_upper<- GD %>% quantile(probs =0.975)  %>% round()
    #-------------------------------------------------
    # (4) Update the points
    # Mean estimate
    if(GD_mean>0){
      teams_pts$Pts[ht] = teams_pts$Pts[ht]+3
    }
    else if(GD_mean<0){
      teams_pts$Pts[at] = teams_pts$Pts[at]+3
    }
    else{
      teams_pts$Pts[ht] = teams_pts$Pts[ht]+1
      teams_pts$Pts[at] = teams_pts$Pts[at]+1
    }
    # Upper/Lower estimates (think about omitting them)
    if(GD_lower>0){
      teams_pts$Lower[ht] = teams_pts$Lower[ht]+3
    }
    else if(GD_lower<0){
      teams_pts$Upper[at] = teams_pts$Upper[at]+3
    }
    else{
      teams_pts$Lower[ht] = teams_pts$Lower[ht]+1
      teams_pts$Upper[at] = teams_pts$Lower[at]+1
    }
    
    if(GD_upper<0){ 
      teams_pts$Lower[at] = teams_pts$Lower[at]+3
    }
    else if(GD_upper>0){
      teams_pts$Upper[ht] = teams_pts$Upper[ht]+3
    }
    else{
      teams_pts$Upper[ht] = teams_pts$Upper[ht]+1
      teams_pts$Lower[at] = teams_pts$Lower[at]+1
    }
    #-------------------------------------------------
  }
  cat("-------------------------------------------------\n")
}
View(teams_pts)
