rm(list = ls())
#-------------------------------------------------------------------------------
# Libraries and utils
#-------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(bayesplot)
library(DescTools)
source("utils/my_skellam.R")
source("utils/final_pts_distribution.R")
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
OFFLINE_MODELS_DIR= paste0("estimated_models/season_",SEASON,"/offline_models/")
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

# 1) Offline models
offline_pts_distribution<- final_pts_distribution(teams,start=20,end=38,
                                                  n_chains=N_CHAINS,n_iters=N_ITERS,n_warmup=N_WARMUP,
                                                  models_dir =OFFLINE_MODELS_DIR)
write.csv(offline_pts_distribution,"report/final_pts_offline.csv",row.names = FALSE)
offline_final_table<- data.frame(Team= teams,
                         Pts_mean=NA,
                         Pts_mode=NA,
                         Pts_lower.95=NA,
                         Pts_lower.50=NA,
                         Pts_upper.50=NA,
                         Pts_upper.95=NA)
for(t in 1:n_teams){
  offline_final_table$Pts_mean[t]<- offline_pts_distribution[,t] %>% mean() %>% round()
  offline_final_table$Pts_mode[t]<- offline_pts_distribution[,t] %>% DescTools::Mode() %>% Mean() %>% round() #We manage the multimodal case by taking their mean
  offline_final_table$Pts_lower.95[t]<- offline_pts_distribution[,t] %>% quantile(probs = 0.025) %>% round()
  offline_final_table$Pts_lower.50[t]<- offline_pts_distribution[,t] %>% quantile(probs = 0.25) %>% round()
  offline_final_table$Pts_upper.50[t]<- offline_pts_distribution[,t] %>% quantile(probs = 0.75) %>% round()
  offline_final_table$Pts_upper.95[t]<- offline_pts_distribution[,t] %>% quantile(probs = 0.975) %>% round()
}

# 2) Online models
online_pts_distribution<- final_pts_distribution(teams,start=20,end=38,
                                                 n_chains=N_CHAINS,n_iters=N_ITERS,n_warmup=N_WARMUP,
                                                 models_dir =ONLINE_MODELS_DIR)
write.csv(online_pts_distribution,"report/final_pts_online.csv",row.names = FALSE)
online_final_table<- data.frame(Team= teams,
                                 Pts_mean=NA,
                                 Pts_mode=NA,
                                 Pts_lower.95=NA,
                                 Pts_lower.50=NA,
                                 Pts_upper.50=NA,
                                 Pts_upper.95=NA)
for(t in 1:n_teams){
  online_final_table$Pts_mean[t]<- online_pts_distribution[,t] %>% mean() %>% round()
  online_final_table$Pts_mode[t]<- online_pts_distribution[,t] %>% DescTools::Mode() %>% Mean() %>% round() #We manage the multimodal case by taking their mean
  online_final_table$Pts_lower.95[t]<- online_pts_distribution[,t] %>% quantile(probs = 0.025) %>% round()
  online_final_table$Pts_lower.50[t]<- online_pts_distribution[,t] %>% quantile(probs = 0.25) %>% round()
  online_final_table$Pts_upper.50[t]<- online_pts_distribution[,t] %>% quantile(probs = 0.75) %>% round()
  online_final_table$Pts_upper.95[t]<- online_pts_distribution[,t] %>% quantile(probs = 0.975) %>% round()
}

# To view directly the dataframe
View(offline_final_table %>% arrange(desc(Pts_mode)))
View(online_final_table %>% arrange(desc(Pts_mode)))



# Plot of the points distribution for each team
color_scheme_set("brightblue")
# Lazy passages to re-order columns basing on their mean
ordered_teams<- online_pts_distribution %>%
  summarise_all(mean) %>%
  round() %>%
  gather() %>%
  arrange(desc(value)) %>%
  pull(key)
# Reorder columns based on mean points
offline_pts_distribution <- offline_pts_distribution[, ordered_teams]
online_pts_distribution <- online_pts_distribution[, ordered_teams]
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
online_pts_distribution %>% 
  mcmc_intervals()+
    labs(title="Estimated final rankings",
         x = "Points",
         y= "Teams")+
    geom_point(
      data = real_points,
      aes(x = Pts, y = Team), 
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
