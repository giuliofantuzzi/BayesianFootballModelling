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
library(ggplot2)
library(bayesplot)
source("utils/my_skellam.R")
# Import and prepare data
SerieA_2324<- read.csv(file="data/season_2324/SerieA_2324.csv")
SerieA_2324<- SerieA_2324[,c("HomeTeam","AwayTeam","FTHG","FTAG")]
teams<- unique(SerieA_2324$HomeTeam)
teams<- str_replace_all(teams, " ", "")
n_teams<- length(teams)
SerieA_2324$ht= unlist(sapply(1:nrow(SerieA_2324),function (g) which(teams==SerieA_2324$HomeTeam[g])))
SerieA_2324$at= unlist(sapply(1:nrow(SerieA_2324),function (g) which(teams==SerieA_2324$AwayTeam[g])))
n_chains=4
n_iters=11000
n_warmup<- 1000
# Rankings after the first half of the league 
# Note: this df is s.t. each column is associated to a team
#       nrows is n_chains*(n_iters-n_warmup) to be friendly with the MCMC posteriors
teams_pts <- data.frame(matrix(NA,nrow = n_chains*(n_iters-n_warmup),ncol=n_teams))
colnames(teams_pts)=teams
for (t in 1:n_teams){
  n_wins= SerieA_2324[1:190,] %>% filter(((ht==t) &(FTHG>FTAG)) | ((at==t) & (FTHG<FTAG)))%>% nrow() 
  n_draws= SerieA_2324[1:190,] %>% filter(((ht==t) | (at==t)) & (FTHG == FTAG))%>% nrow() 
  teams_pts[,t]= 3*n_wins+n_draws
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
  for(mm in 1:nrow(test_set)){
    ht=test_set$ht[mm]
    at=test_set$at[mm]
    attH=posterior[,,paste0("att[",ht,"]")]
    defH=posterior[,,paste0("def[",ht,"]")]
    attA=posterior[,,paste0("att[",at,"]")]
    defA=posterior[,,paste0("def[",at,"]")]
    theta_H = exp(mu+home+attH+defA)
    theta_A = exp(mu+attA+defH)
    GD<- mapply(my_rzeroinflatedskellam, 1,theta_H, theta_A,p)
    
    # (4) Calculate points according to predicted goal diffs
    pts_ht = ifelse(GD > 0, 3, ifelse(GD < 0, 0, 1))
    pts_at = ifelse(GD < 0, 3, ifelse(GD > 0, 0, 1))
    # (5) Sum them to the previous points
    teams_pts[,ht] = teams_pts[,ht]+pts_ht
    teams_pts[,at] = teams_pts[,at]+pts_at
    #-------------------------------------------------
  }
  cat("-------------------------------------------------\n")
}
# Now teams_pts has a distribution of final points for each team
final_table<- data.frame(Team= teams,
                         Pts_mean=NA,
                         Pts_lower=NA,
                         Pts_upper=NA)
for(t in 1:n_teams){
  final_table$Pts_mean[t]<- teams_pts[,t] %>% mean() %>% round()
  final_table$Pts_lower[t]<- teams_pts[,t] %>% quantile(probs = 0.025) %>% round()
  final_table$Pts_upper[t]<- teams_pts[,t] %>% quantile(probs = 0.975) %>% round()
}

# To view directly the dataframe
View(final_table)
# Plot of the points distribution for each team
color_scheme_set("brightblue")
mcmc_intervals(teams_pts)+
  labs(title="Ranking points at the end of the season",
       x = "Points",
       y= "Teams")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",size=18),
        axis.title.x = element_text(size=12, face="bold", colour = "black"),
        axis.text.x = element_text(size=10, face="plain", colour = "black"),
        axis.title.y = element_text(size=12, face="bold", colour = "black"),
        axis.text.y = element_text(size=10, face="plain", colour = "black"),
  )
