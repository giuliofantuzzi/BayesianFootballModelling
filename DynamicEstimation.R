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
MODELS_DIR= paste0("estimated_models/season_",SEASON,"/models/")
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
# Estimation of the models over time
#-------------------------------------------------------------------------------
if(!file.exists(MODELS_DIR)){
  dir.create(MODELS_DIR,recursive = T)
}

for(m in 19:n_matchdays){
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  cat("Parameters estimation after matchday n.",i,"...\n")
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  #---------------------------------------------------------------------------
  # (1) Training and test set for the current matchday
  training<- SerieA_data[1:(10*m),c("HomeTeam","AwayTeam","FTHG","FTAG")]
  training<- na.omit(training)
  # (2) Prepare the parameters for stan
  stan_paramters = list(
    n_teams=n_teams,
    n_games=nrow(training),
    home_team= ht[1:nrow(training)],
    away_team= at[1:nrow(training)],
    goal_difference = SerieA_data$FTHG[1:nrow(training)]-SerieA_data$FTAG[1:nrow(training)]
  )
  # (3) Fit the model
  KN_model <- stan(file = paste0(STAN_DIR,"karlis-ntzoufras.stan"),
                   data = stan_paramters,
                   chains = N_CHAINS,
                   iter = N_ITERS,
                   warmup = N_WARMUP,
                   seed = 16
  )
  # (4) Save the model
  dir.create(paste0(MODELS_DIR,"matchday",m))
  save(KN_model,file=paste0(MODELS_DIR,"matchday",m,"/KN_matchday",m,".rds"))
}
