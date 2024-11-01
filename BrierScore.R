rm(list = ls())
#-------------------------------------------------------------------------------
# Libraries and utils
#-------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyverse)
source("utils/HDA_probabilities.R")
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
MODELS_DIR= paste0("estimated_models/season_",SEASON,"/offline_models/")
#-------------------------------------------------------------------------------
# Data import and preparation
#-------------------------------------------------------------------------------
SerieA_data<- read.csv(file= paste0(DATA_DIR,"season_",SEASON,"/SerieA_",SEASON,".csv"))
SerieA_data<- SerieA_data[,c("HomeTeam","AwayTeam","FTHG","FTAG","FTR")]
teams<- unique(SerieA_data$HomeTeam)
n_games<- nrow(SerieA_data)
n_teams<- length(teams)
n_matchdays= ceiling(n_games/((n_teams)/2))
SerieA_data$ht= unlist(sapply(1:n_games,function (g) which(teams==SerieA_data$HomeTeam[g])))
SerieA_data$at= unlist(sapply(1:n_games,function (g) which(teams==SerieA_data$AwayTeam[g])))
teams<- str_replace_all(teams, " ", "")

#-------------------------------------------------------------------------------
# Compute Brier Score matchday by matchday
#-------------------------------------------------------------------------------
# Online Model
BS_ts<- data.frame(matchday=20:n_matchdays,
                   BS_offline=NA,
                   BS_online=NA)
for(m in 20:n_matchdays){
  cat(paste0("Computing Brier Score for matchday",m,"\n"))
  test_set=SerieA_data[(10*m -9):(10*m),]
  test_set=na.omit(test_set)
  load(paste0(MODELS_DIR,"/matchday",m-1,"/KN_matchday",m-1,".rds"))
  
  test_set$deltaH= rep(0,nrow(test_set))
  test_set$deltaD= rep(0,nrow(test_set))
  test_set$deltaA= rep(0,nrow(test_set))
  
  for(mm in 1:nrow(test_set)){
    ht=test_set$ht[mm]
    at=test_set$at[mm]
    # Get probabilities H-D-A 
    HDA_probs= HDA_probabilities(ht,at,KN_model)/100
    # Get the real result
    FTR= test_set[mm,"FTR"]
    # Given the real result, update Real result dummies (deltaH,deltaD,deltaA)
    test_set[mm,paste("delta",FTR,sep="")]=1
  }
  BS_ts$BS_offline[m-19] = sum((HDA_probs - as.numeric(test_set[mm,c("deltaH","deltaD","deltaA")]))^2)/nrow(test_set)
}


for(m in 20:n_matchdays){
  cat(paste0("Computing Brier Score for matchday",m," (online model)\n"))
  test_set=SerieA_data[(10*m -9):(10*m),]
  test_set=na.omit(test_set)
  load(paste0(ONLINE_MODELS_DIR,"/matchday",m-1,"/KN_matchday",m-1,".rds"))
  
  test_set$deltaH= rep(0,nrow(test_set))
  test_set$deltaD= rep(0,nrow(test_set))
  test_set$deltaA= rep(0,nrow(test_set))
  
  for(mm in 1:nrow(test_set)){
    ht=test_set$ht[mm]
    at=test_set$at[mm]
    # Get probabilities H-D-A 
    HDA_probs= HDA_probabilities(ht,at,KN_model)/100
    # Get the real result
    FTR= test_set[mm,"FTR"]
    # Given the real result, update Real result dummies (deltaH,deltaD,deltaA)
    test_set[mm,paste("delta",FTR,sep="")]=1
  }
  BS_ts$BS_online[m-19] = sum((HDA_probs - as.numeric(test_set[mm,c("deltaH","deltaD","deltaA")]))^2)/nrow(test_set)
}


BS_ts %>% 
  pivot_longer(cols=c(BS_offline,BS_online),
               names_to = "BS",
               values_to = "Value") %>%
  ggplot(aes(x=matchday,y=Value,color=BS))+
  geom_line(linewidth=1)+
  ggtitle("Brier score over time for the 2 models")
  
