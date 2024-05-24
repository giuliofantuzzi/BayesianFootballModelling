rm(list = ls())
#-------------------------------------------------------------------------------
# Libraries and utils
#-------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyverse)
library(cvms)
source("utils/my_skellam.R")
source("utils/plot_confusionmatrix.R")
#-------------------------------------------------------------------------------
# Global settings and variables
#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
# ???
#-------------------------------------------------------------------------------
SerieA_data$GD= SerieA_data$FTHG - SerieA_data$FTAG
SerieA_data$PredictedGD=NA
for (m in 20:n_matchdays){
  cat(paste0("...Simulation of matchday n. ",m,"...\n"))
  #-------------------------------------------------
  # (1) Get the current matchday to predict
  test_set=SerieA_data[(10*m -9):(10*m),]
  #-------------------------------------------------
  # (2) Load the most recent model and some of its stuff
  cat("...Loading the model and retrieving useful info...\n")
  load(paste0(ONLINE_MODELS_DIR,"matchday",m-1,"/KN_matchday",m-1,".rds"))
  posterior<- as.array(KN_model)
  mu = posterior[,,"mu"]
  home=posterior[,,"home_advantage"]
  p = posterior[,,"p"]
  #-------------------------------------------------
  # (3) Make predictions
  cat("...Computing predictions and updating rankings...\n")
  PredictedGD<- vector(mode="numeric",length = nrow(test_set))
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
    
    PredictedGD[mm]<- GD %>% mean() %>% round()
  }
  # (4) Add predicted GD to the dataset
  SerieA_data$PredictedGD[(10*m -9):(10*m)]<- PredictedGD
  cat("-------------------------------------------------\n")
}

ConfusionMatrix_df<- SerieA_data %>% slice(191:n_games) %>% select(GD,PredictedGD)

ConfusionMatrix_df<- ConfusionMatrix_df %>% mutate(
   Actual = ifelse(GD > 0, "H", ifelse(GD < 0, "A", "D")),
   Predicted = ifelse(PredictedGD > 0, "H", ifelse(PredictedGD < 0, "A", "D"))
 )
# 
# 
 CM <- confusion_matrix(
   targets = factor(ConfusionMatrix_df$Actual, levels = c("H", "D", "A")),
   predictions = factor(ConfusionMatrix_df$Predicted, levels = c("H", "D", "A"))
 )


plot_confusionmatrix(CM, "Karlis-Ntzoufras (Offline version)")
 
