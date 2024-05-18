rm(list = ls())
#-------------------------------------------------------------------------------
# Libraries and utils
#-------------------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(rstan)
library(ggplot2)
library(patchwork)

#-------------------------------------------------------------------------------
# Global settings and variables
#-------------------------------------------------------------------------------
options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)
rstan_options(threads_per_chain = 2)
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
teams<- str_replace_all(teams, " ", "")
#-------------------------------------------------------------------------------
# Estimation of the models over time (but with an online approach)
#-------------------------------------------------------------------------------
# Create the folder to store models
if(!file.exists(MODELS_DIR)){
  dir.create(MODELS_DIR,recursive = T)
}
# (1) Base step: fit the first model after 1st half of the league
base_training=SerieA_data[1:190,c("HomeTeam","AwayTeam","FTHG","FTAG")]
stan_paramters = list(
  n_teams=n_teams,
  n_games=nrow(base_training),
  home_team= ht[1:nrow(base_training)],
  away_team= at[1:nrow(base_training)],
  goal_difference = base_training$FTHG-base_training$FTAG,
  prev_att_means=rep(0,20),
  prev_def_means=rep(0,20),
  prev_mu_mean=0,
  prev_home_advantage_mean=0,
  prev_att_sd=rep(10,20),
  prev_def_sd=rep(10,20),
  prev_mu_sd=10,
  prev_home_advantage_sd=10
)

KN_model <- stan(file = paste0(STAN_DIR,"online.stan"),
                 data = stan_paramters,
                 chains = N_CHAINS,
                 iter = N_ITERS,
                 warmup = N_WARMUP,
                 seed = 16
)
dir.create(paste0(MODELS_DIR,"matchday19/"))
save(KN_model,file=paste0(MODELS_DIR,"matchday19/KN_matchday19.rds"))

#...............................................................................
# Note: a totally equivalent way would be
# base_training=SerieA_data[1:190,c("HomeTeam","AwayTeam","FTHG","FTAG")]
# stan_paramters = list(
#   n_teams=n_teams,
#   n_games=nrow(base_training),
#   home_team= ht[1:nrow(base_training)],
#   away_team= at[1:nrow(base_training)],
#   goal_difference = base_training$FTHG-base_training$FTAG
# )
# 
# KN_model <- stan(file = paste0(STAN_DIR,"karlis-ntzoufras.stan"),
#                  data = stan_paramters,
#                  chains = N_CHAINS,
#                  iter = N_ITERS,
#                  warmup = N_WARMUP,
#                  seed = 16
# )
#...............................................................................


# (2) Online learning loop
for(i in 20:n_matchdays){
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  cat("...Parameters estimation after matchday n.",i,"...\n")
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  #---------------------------------------------------------------------------
  # (1) Training and test set for the current matchday
  cat("...Preparing the training set...\n")
  training<- SerieA_data[1:(10*i),c("HomeTeam","AwayTeam","FTHG","FTAG")]
  training<- na.omit(training) #just to manage if some matches were postponed...
  #---------------------------------------------------------------------------
  # (2) Retrieving the previous estimates
  cat("...Retrieving previous prior information...\n")
  load(paste0(MODELS_DIR,"/matchday",i-1,"/KN_matchday",i-1,".rds"))
  prev_att_means= unlist(sapply(1:n_teams,function (t) mean(as.array(KN_model)[,,paste0("att[",t,"]")]))) #qui da mettere n_teams-1
  prev_def_means= unlist(sapply(1:n_teams,function (t) mean(as.array(KN_model)[,,paste0("def[",t,"]")]))) #qui da mettere n_teams-1
  prev_mu_mean= mean(as.array(KN_model)[,,"mu"])
  prev_home_advantage_mean= mean(as.array(KN_model)[,,"home_advantage"])
  prev_att_sd= unlist(sapply(1:n_teams,function (t) sd(as.array(KN_model)[,,paste0("att[",t,"]")]))) #qui da mettere n_teams-1
  prev_def_sd= unlist(sapply(1:n_teams,function (t) sd(as.array(KN_model)[,,paste0("def[",t,"]")]))) #qui da mettere n_teams-1
  prev_mu_sd= sd(as.array(KN_model)[,,"mu"])
  prev_home_advantage_sd= sd(as.array(KN_model)[,,"home_advantage"])
  #---------------------------------------------------------------------------
  # (3) Prepare the parameters for stan
  stan_paramters = list(
    n_teams=n_teams,
    n_games=nrow(training),
    home_team= ht[1:nrow(training)],
    away_team= at[1:nrow(training)],
    goal_difference = training$FTHG - training$FTAG,
    prev_att_means=prev_att_means,
    prev_def_means=prev_def_means,
    prev_mu_mean=prev_mu_mean,
    prev_home_advantage_mean=prev_home_advantage_mean,
    prev_att_sd=prev_att_sd,
    prev_def_sd=prev_def_sd,
    prev_mu_sd=prev_mu_sd,
    prev_home_advantage_sd=prev_home_advantage_sd
    )
  # (4) Fit the model
  cat("...Fitting the model...\n")
  KN_model <- stan(file = paste0(STAN_DIR,"online.stan"),
                   data = stan_paramters,
                   chains = N_CHAINS,
                   iter = N_ITERS,
                   warmup = N_WARMUP,
                   seed = 16
  )
  # (5) Save the model
  dir.create(paste0(MODELS_DIR,"matchday",i,"/"))
  save(KN_model,file=paste0(MODELS_DIR,"matchday",i,"/KN_matchday",i,".rds"))
}
