rm(list = ls())
#-------------------------------------------------------------------------------
# Libraries and utils
#-------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(bayesplot)
source("utils/my_skellam.R")
#-------------------------------------------------------------------------------
# Global settings and variables
#-------------------------------------------------------------------------------
N_CHAINS=4
N_ITERS=11000
N_WARMUP=1000
DATA_DIR= "data/"
STAN_DIR= "stan/"
SEASON="2122"
MODELS_DIR= paste0("estimated_models/season_",SEASON,"/online_models/")
#-------------------------------------------------------------------------------
# Data import and preparation
#-------------------------------------------------------------------------------
SerieA_data<- read.csv(file= paste0(DATA_DIR,"season_",SEASON,"/SerieA_",SEASON,".csv"))
SerieA_data<- SerieA_data[,c("HomeTeam","AwayTeam","FTHG","FTAG")]
teams<- unique(SerieA_data$HomeTeam)
n_games<- nrow(SerieA_data)
n_teams<- length(teams)
n_matchdays= ceiling(n_games/((n_teams)/2))
ht= unlist(sapply(1:n_games,function (g) which(teams==SerieA_data$HomeTeam[g])))
at= unlist(sapply(1:n_games,function (g) which(teams==SerieA_data$AwayTeam[g])))
SerieA_data$ht= ht
SerieA_data$at= at
n_matchdays= ceiling(n_games/((n_teams)/2))
teams<- str_replace_all(teams, " ", "")
#-------------------------------------------------------------------------------
# Rankings Simulation
#-------------------------------------------------------------------------------
# Rankings after the first half of the league 
# Note: this df is s.t. each column is associated to a team
#       nrows is N_CHAINS*(N_ITERS-N_WARMUP) to be friendly with the MCMC posteriors
teams_pts <- data.frame(matrix(NA,nrow = N_CHAINS*(N_ITERS-N_WARMUP),ncol=n_teams))
colnames(teams_pts)=teams
for (t in 1:n_teams){
  n_wins= SerieA_data[1:190,] %>% filter(((ht==t) &(FTHG>FTAG)) | ((at==t) & (FTHG<FTAG)))%>% nrow() 
  n_draws= SerieA_data[1:190,] %>% filter(((ht==t) | (at==t)) & (FTHG == FTAG))%>% nrow() 
  teams_pts[,t]= 3*n_wins+n_draws
}

# Iteration over the 2nd half
for (m in 20:n_matchdays){
  cat(paste0("...Simulation of matchday n. ",m,"...\n"))
  #-------------------------------------------------
  # (1) Get the current matchday to predict
  test_set=SerieA_data[(10*m -9):(10*m),]
  test_set=na.omit(test_set) #just to manage postponed matches in last batch
  #-------------------------------------------------
  # (2) Load the most recent model and some of its stuff
  cat("...Loading the model and retrieving useful info...\n")
  load(paste0(MODELS_DIR,"matchday",m-1,"/KN_matchday",m-1,".rds"))
  posterior<- as.array(KN_model)
  N_ITERS=dim(posterior)[1]
  N_CHAINS=dim(posterior)[2]
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
# Lazy passages to re-order columns basing on their mean
team_means <- teams_pts %>%
  summarise_all(mean) %>%
  round() %>%
  gather() %>%
  arrange(desc(value)) %>%
  pull(key)
# Reorder columns based on mean points
teams_pts <- teams_pts[, team_means]
# Compute the real points in order to add them in the mcmc_plot
real_points= data.frame(
  Team=teams,
  Pts=NA
)
for (t in 1:n_teams){
  n_wins= SerieA_data %>% filter(((ht==t) &(FTHG>FTAG)) | ((at==t) & (FTHG<FTAG)))%>% nrow() 
  n_draws= SerieA_data %>% filter(((ht==t) | (at==t)) & (FTHG == FTAG))%>% nrow()
  real_points$Pts[t]= 3*n_wins+n_draws
}

# Mcmc plot of the points distribution
mcmc_intervals(teams_pts)+
  labs(title="Estimated final rankings",
       x = "Points",
       y= "Teams")+
  geom_point(
    data = real_points,
    aes(x = Pts, y = Team), # Map color to a variable
    size = 3.5,
    shape=21,
    color="black",
    fill="indianred2"
  )+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",size=18),
        axis.title.x = element_text(size=12, face="bold", colour = "black"),
        axis.text.x = element_text(size=10, face="plain", colour = "black"),
        axis.title.y = element_text(size=12, face="bold", colour = "black"),
        axis.text.y = element_text(size=10, face="plain", colour = "black"),
  )
