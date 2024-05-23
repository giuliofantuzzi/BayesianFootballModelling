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
ONLINE_MODELS_DIR= paste0("estimated_models/season_",SEASON,"/online_models/")
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
GD_df<- data.frame(matrix(NA,nrow = N_CHAINS*(N_ITERS-N_WARMUP),ncol=n_games))
#load the final model (end of season)
load(paste0(ONLINE_MODELS_DIR,"matchday",n_matchdays,"/KN_matchday",n_matchdays,".rds"))
posterior<- as.array(KN_model)
mu = posterior[,,"mu"]
home=posterior[,,"home_advantage"]
p = posterior[,,"p"]
for (m in 1:n_games){
  cat(paste0("...Working on match ",m,"/380...\n"))
  ht=SerieA_data$ht[m]
  at=SerieA_data$at[m]
  attH=posterior[,,paste0("att[",ht,"]")]
  defH=posterior[,,paste0("def[",ht,"]")]
  attA=posterior[,,paste0("att[",at,"]")]
  defA=posterior[,,paste0("def[",at,"]")]
  theta_H = exp(mu+home+attH+defA)
  theta_A = exp(mu+attA+defH)
  GD<- mapply(my_rzeroinflatedskellam, 1,theta_H, theta_A,p)
  # (4) Add simulated values to GD_df
  GD_df[,m-190]<- GD
  colnames(GD_df)[m-190]<-paste0(teams[ht],"-vs-",teams[at])
}

y=SerieA_data$GD
ppc_dens_overlay(y,as.matrix(GD_df[1:4000,])) + ggtitle("Posterior checks of Goal Differences")
ppc_intervals(y,as.matrix(GD_df[1:40000,]),prob_outer = 0.95)
ppc_ecdf_overlay(y,as.matrix(GD_df[1:10000,]))
ppc_stat(y,as.matrix(GD_df[1:40000,]),stat = "mean",bins = 20)
prop_zero <- function(x) mean(x == 0)
ppc_stat(y,as.matrix(GD_df[1:40000,]),stat = "prop_zero",bins = 20)
ppc_stat_2d(y,as.matrix(GD_df[1:40000,]),stat = c("mean","sd"))
 