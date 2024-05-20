rm(list = ls())
#-------------------------------------------------------------------------------
# Libraries and utils
#-------------------------------------------------------------------------------
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyverse)
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
MATCHDAY_TO_PREDICT=38

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
teams<- str_replace_all(teams, " ", "")
SerieA_data$GD= SerieA_data$FTHG - SerieA_data$FTAG

#-------------------------------------------------------------------------------
# Simulations
#-------------------------------------------------------------------------------
GD_df<- data.frame(matrix(NA,nrow = N_CHAINS*(N_ITERS-N_WARMUP),ncol=190))

for (m in 20:n_matchdays){
  cat(paste0("...Simulation of matchday n. ",m,"...\n"))
  #-------------------------------------------------
  # (1) Get the current matchday to predict
  test_set=SerieA_data[(10*m -9):(10*m),]
  #-------------------------------------------------
  # (2) Load the most recent model and some of its stuff
  cat("...Loading the model and retrieving useful info...\n")
  load(paste0(MODELS_DIR,"matchday",m-1,"/KN_matchday",m-1,".rds"))
  posterior<- as.array(KN_model)
  mu = posterior[,,"mu"]
  home=posterior[,,"home_advantage"]
  p = posterior[,,"p"]
  #-------------------------------------------------
  # (3) Make predictions
  cat("...Generating quantities...\n")
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
    # (4) Add simulated values to GD_df
    GD_df[,10*(m-20)+mm]<- GD
    colnames(GD_df)[10*(m-20)+mm]<-paste0(teams[ht],"-vs-",teams[at])
  }
  cat("-------------------------------------------------\n")
}

# Ci sta 7 anni!!!
y=SerieA_data$GD[191:200]
ppc_dens_overlay(y,as.matrix(GD_df[1:400,1:10]))
