# Let's assume to have data from first half of the league
# Since my model predicts only the goal difference, it would be impossible to "fully"-simulate the 2nd half
# In fact, in a full simulation I would predict results and incorporate the predicted results in a dataset
# What I can do is to make predictions and assign points to the table basing on the predicted results
# Then at next matchday I will update my knowledge about parameters with the real results and simulate again
#--->Fortunately i can exploit the models i already fitted ;)
library(tidyverse)
library(dplyr)
source("utils/my_skellam.R")
#-------------------------------------------------------------------------------
# IDEA 1: use already fitted model and just assign points basing on predictions
#-------------------------------------------------------------------------------
SerieA_2324<- read.csv(file="data/season_2324/SerieA_2324.csv")
SerieA_2324<- SerieA_2324[,c("HomeTeam","AwayTeam","FTHG","FTAG")]
teams<- unique(SerieA_2324$HomeTeam)
teams<- str_replace_all(teams, " ", "")
n_teams<- length(teams)
SerieA_2324$ht= unlist(sapply(1:nrow(SerieA_2324),function (g) which(teams==SerieA_2324$HomeTeam[g])))
SerieA_2324$at= unlist(sapply(1:nrow(SerieA_2324),function (g) which(teams==SerieA_2324$AwayTeam[g])))

# Ranking df
teams_pts <- data.frame(Team=teams,Pts=rep(0,n_teams))
for (t in 1:n_teams){
  n_wins= SerieA_2324[1:190,] %>% filter(((ht==t) &(FTHG>FTAG)) | ((at==t) & (FTHG<FTAG)))%>% nrow() 
  n_draws= SerieA_2324[1:190,] %>% filter(((ht==t) | (at==t)) & (FTHG == FTAG))%>% nrow() 
  teams_pts$Pts[t]= 3*n_wins+n_draws
}

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
    GD<- mapply(my_rzeroinflatedskellam, 1,theta_H, theta_A,p) %>% mean() %>% round()
    #-------------------------------------------------
    # (4) Update the points
    if(GD>0){
      teams_pts$Pts[ht] = teams_pts$Pts[ht]+3
    }
    else if(GD<0){
      teams_pts$Pts[at] = teams_pts$Pts[at]+3
    }
    else{
      teams_pts$Pts[ht] = teams_pts$Pts[ht]+1
      teams_pts$Pts[at] = teams_pts$Pts[at]+1
    }
    #-------------------------------------------------
  }
  cat("-------------------------------------------------\n")
}
View(teams_pts)
