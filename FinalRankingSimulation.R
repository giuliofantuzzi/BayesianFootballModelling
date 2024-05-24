rm(list = ls())
#-------------------------------------------------------------------------------
# Libraries and utils
#-------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(bayesplot)
library(DescTools)
library(knitr)
library(kableExtra)
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
final_pts_offline<- final_pts_distribution(teams,start=20,end=38,
                                           n_chains=N_CHAINS,
                                           n_iters=N_ITERS,
                                           n_warmup=N_WARMUP,
                                           models_dir =OFFLINE_MODELS_DIR)

final_pts_online<- final_pts_distribution(teams,start=20,end=38,
                                          n_chains=N_CHAINS,
                                          n_iters=N_ITERS,
                                          n_warmup=N_WARMUP,
                                          models_dir =ONLINE_MODELS_DIR)


write.csv(final_pts_offline,"report/final_pts_offline.csv",row.names = FALSE)
write.csv(final_pts_online,"report/final_pts_online.csv",row.names = FALSE)


offline_rankings<- data.frame(Team= teams,
                              Pts_mean=NA,
                              Pts_mode=NA,
                              Pts_lower.95=NA,
                              Pts_lower.50=NA,
                              Pts_upper.50=NA,
                              Pts_upper.95=NA)

online_rankings<- data.frame(Team= teams,
                             Pts_mean=NA,
                             Pts_mode=NA,
                             Pts_lower.95=NA,
                             Pts_lower.50=NA,
                             Pts_upper.50=NA,
                             Pts_upper.95=NA)

for(t in 1:n_teams){
  
  # Update rankings for offline model
  offline_rankings$Pts_mean[t]<- final_pts_offline[,t] %>% mean() %>% round()
  offline_rankings$Pts_mode[t]<- final_pts_offline[,t] %>% DescTools::Mode() %>% mean() %>% round() #mean just to manage multimodal case
  offline_rankings$Pts_lower.95[t]<- final_pts_offline[,t] %>% quantile(probs = 0.025) %>% round()
  offline_rankings$Pts_lower.50[t]<- final_pts_offline[,t] %>% quantile(probs = 0.25) %>% round()
  offline_rankings$Pts_upper.50[t]<- final_pts_offline[,t] %>% quantile(probs = 0.75) %>% round()
  offline_rankings$Pts_upper.95[t]<- final_pts_offline[,t] %>% quantile(probs = 0.975) %>% round()
  
  # Update rankings for online model
  online_rankings$Pts_mean[t]<- final_pts_online[,t] %>% mean() %>% round()
  online_rankings$Pts_mode[t]<- final_pts_online[,t] %>% DescTools::Mode() %>% mean() %>% round() #mean just to manage multimodal case
  online_rankings$Pts_lower.95[t]<- final_pts_online[,t] %>% quantile(probs = 0.025) %>% round()
  online_rankings$Pts_lower.50[t]<- final_pts_online[,t] %>% quantile(probs = 0.25) %>% round()
  online_rankings$Pts_upper.50[t]<- final_pts_online[,t] %>% quantile(probs = 0.75) %>% round()
  online_rankings$Pts_upper.95[t]<- final_pts_online[,t] %>% quantile(probs = 0.975) %>% round()
}


# Simulated Rankings at the end of the season
table_offline<- offline_rankings %>% 
  arrange(desc(Pts_mode)) %>%
  mutate(Position = row_number()) %>%
  select(Position,Team,Pts_mean,Pts_mode,Pts_lower.95,Pts_lower.50,Pts_upper.50,Pts_upper.95) %>%
  kbl(caption = "Serie A 2021-2022 simulated rankings (offline models)", align = 'c') %>%
  kable_styling(font_size = 12, full_width = F) %>%
  row_spec(0, background = "#FFFFFF", color = "#000000",bold=T) %>%
  # Champions League
  row_spec(1:4, background = "#8AD1F3") %>% 
  # Europa League
  row_spec(5:6, background = "#EEA256") %>%
  # Conference League
  row_spec(7, background = "#E1D625") %>%  
  # Relegation
  row_spec((nrow(offline_rankings)-2):nrow(offline_rankings), background = "#FF9E94") %>%
  row_spec(1:nrow(offline_rankings), extra_css = "border-top: 1px solid black;")


table_online<- online_rankings %>% 
  arrange(desc(Pts_mode)) %>%
  mutate(Position = row_number()) %>%
  select(Position,Team,Pts_mean,Pts_mode,Pts_lower.95,Pts_lower.50,Pts_upper.50,Pts_upper.95) %>%
  kbl(caption = "Serie A 2021-2022 simulated rankings (online models)", align = 'c') %>%
  kable_styling(font_size = 12, full_width = F) %>%
  row_spec(0, background = "#FFFFFF", color = "#000000",bold=T) %>%
  # Champions League
  row_spec(1:4, background = "#8AD1F3") %>% 
  # Europa League
  row_spec(5:6, background = "#EEA256") %>%
  # Conference League
  row_spec(7, background = "#E1D625") %>%  
  # Relegation
  row_spec((nrow(online_rankings)-2):nrow(online_rankings), background = "#FF9E94") %>%
  row_spec(1:nrow(online_rankings), extra_css = "border-top: 1px solid black;")


# Plot of the points distribution for each team
color_scheme_set("brightblue")
# Lazy passages to re-order columns basing on their mean
ordered_teams<- final_pts_offline %>%
  summarise_all(mean) %>%
  round() %>%
  gather() %>%
  arrange(desc(value)) %>%
  pull(key)
# Reorder columns based on mean points
final_pts_offline <- final_pts_offline[, ordered_teams]
final_pts_online <- final_pts_online[, ordered_teams]


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
final_pts_offline_plot<- final_pts_offline %>% 
  mcmc_intervals()+
  labs(title="Estimated final rankings (offline model)",
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

final_pts_online_plot<- final_pts_online %>% 
  mcmc_intervals()+
  labs(title="Estimated final rankings (online model)",
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

