#-------------------------------------------------------------------------------
# Libraries and utils
#-------------------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(rstan)
library(ggplot2)
library(patchwork)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
rstan_options(threads_per_chain = 2)

#-------------------------------------------------------------------------------
# Data import and preparation
#-------------------------------------------------------------------------------
SerieA_2324<- read.csv(file="data/season_2324/SerieA_2324.csv")
SerieA_2324<- SerieA_2324[,c("HomeTeam","AwayTeam","FTHG","FTAG")]
n_games<- nrow(SerieA_2324)
teams<- unique(SerieA_2324$HomeTeam)
ht= unlist(sapply(1:n_games,function (g) which(teams==SerieA_2324$HomeTeam[g])))
at= unlist(sapply(1:n_games,function (g) which(teams==SerieA_2324$AwayTeam[g])))
teams<- str_replace_all(teams, " ", "")
n_teams<- length(teams)


#-------------------------------------------------------------------------------
# Estimation of the models over time
#-------------------------------------------------------------------------------
n_chains<-4
n_iters<- 11000
n_warmup<- 1000

dir.create("estimated_models/models")
for(i in 19:36){
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  cat("Parameters estimation after matchday n.",i,"...\n")
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  #---------------------------------------------------------------------------
  # (1) Training and test set for the current matchday
  training<- SerieA_2324[1:(10*i),c("HomeTeam","AwayTeam","FTHG","FTAG")]
  training<- na.omit(training)
  # (2) Prepare the parameters for stan
  stan_paramters = list(
    n_teams=n_teams,
    n_games=nrow(training),
    home_team= ht[1:nrow(training)],
    away_team= at[1:nrow(training)],
    goal_difference = SerieA_2324$FTHG[1:nrow(training)]-SerieA_2324$FTAG[1:nrow(training)]
  )
  # (3) Fit the model
  KN_model <- stan(file = 'stan/karlis-ntzoufras.stan',
                   data = stan_paramters,
                   chains = n_chains,
                   iter = n_iters,
                   warmup = n_warmup,
                   seed = 16
  )
  # (4) Save the model
  dir.create(paste0("estimated_models/models/matchday",i))
  save(KN_model,file=paste0("estimated_models/models/matchday",i,"/KN_matchday",i,".rds"))
}

#-------------------------------------------------------------------------------
# Get all att-def parameters over matchdays
#-------------------------------------------------------------------------------
source("utils/get_all_teams_data.R")
ts_df<- get_all_teams_data(teams_list=teams,start=19,end=36,models_dir_path ="estimated_models/models")

#-------------------------------------------------------------------------------
# Plot the timeseries for each team
#-------------------------------------------------------------------------------
source("utils/plot_parameters_ts.R")

plot_list <- list()

plot_list <- lapply(teams, function(t) {
  plot_parameters_ts(team = t,complete_df = ts_df,start = 19,end = 36)
  }
)

(plot_list[[1]] | plot_list[[2]] |plot_list[[3]] | plot_list[[4]]) /
  (plot_list[[5]] | plot_list[[6]] |plot_list[[7]] | plot_list[[8]]) /
  (plot_list[[9]] | plot_list[[10]] |plot_list[[11]] | plot_list[[12]]) /
  (plot_list[[13]] | plot_list[[14]] |plot_list[[15]] | plot_list[[16]]) /
  (plot_list[[17]] | plot_list[[18]] |plot_list[[19]] | plot_list[[20]])



