rm(list = ls())
#-------------------------------------------------------------------------------
# Libraries and utils
#-------------------------------------------------------------------------------
library(ggplot2)
library(ggrepel)
library(ggimage)
library(dplyr)
library(tidyverse)
library(bayesplot)
source("utils/get_all_teams_data.R")
source("utils/plot_parameters_ts.R")
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
OFFLINE_MODELS_DIR= paste0("estimated_models/season_",SEASON,"/models/")

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
# Load the model
#-------------------------------------------------------------------------------
load(paste0(ONLINE_MODELS_DIR,"matchday",n_matchdays,"/KN_matchday",n_matchdays,".rds"))
par_names<-  names(KN_model)
useful_par_names<- par_names[!(grepl("raw", par_names))]
print(KN_model,par=useful_par_names)
posterior<- as.array(KN_model)

# Helper quantities to parse parameters/parameters names in bayesplots
att_params <- useful_par_names[grepl("att", useful_par_names)]
def_params <- useful_par_names[grepl("def", useful_par_names)]
att_labels <- setNames(teams, paste0("att[", 1:20, "]"))
def_labels <- setNames(teams, paste0("def[", 1:20, "]"))

#-------------------------------------------------------------------------------
# Plots
#-------------------------------------------------------------------------------

# Attack strenght coefficients
color_scheme_set("blue")
mcmc_intervals(posterior,pars=att_params)+
  scale_y_discrete(labels=att_labels)+
  ggtitle("Attack strenght coefficients (posterior)")

# Defence weakness coefficients
color_scheme_set("red")
mcmc_intervals(posterior,pars=def_params)+
  scale_y_discrete(labels=def_labels)+
  ggtitle("Defence weakness coefficients (posterior)")

# marginal posterior (it accounts for all the chains together)
color_scheme_set("blue")
mcmc_areas(posterior,pars= att_params)+
  scale_y_discrete(labels=att_labels)+
  ggtitle("MCMC areas for attack coefficients")

color_scheme_set("red")
mcmc_areas(posterior,pars=def_params)+
  scale_y_discrete(labels=def_labels)+
  ggtitle("MCMC areas for defence coefficients")

# density overlay (we see the chains)
color_scheme_set("blue")
dens_overlay_att=mcmc_dens_overlay(posterior,pars=att_params)
# Here we need to do something different to re-name the parameters in the plot!
dens_overlay_att[[1]]$Parameter = rep(att_labels,each=N_CHAINS*(N_ITERS-N_WARMUP))
dens_overlay_att


color_scheme_set("red")
dens_overlay_def=mcmc_dens_overlay(posterior,pars=def_params)
# Here we need to do something different to re-name the parameters in the plot!
dens_overlay_def[[1]]$Parameter = rep(def_labels,each=N_CHAINS*(N_ITERS-N_WARMUP))
dens_overlay_def


# Plots with logos

# Create scatterplot_df dataframe
scatterplot_df <- data.frame(
  "Team" = teams,
  "Att" = NA,
  "Def" = NA,
  "Logo" = NA
)

scatterplot_df$Logo <- c(
  "https://upload.wikimedia.org/wikipedia/commons/0/05/FC_Internazionale_Milano_2021.svg",
  "https://upload.wikimedia.org/wikipedia/en/9/92/Hellas_Verona_FC_logo_%282020%29.svg",
  "https://upload.wikimedia.org/wikipedia/en/e/e9/Empoli_F.C._logo_%282021%29.png",
  "https://upload.wikimedia.org/wikipedia/en/2/2e/Torino_FC_Logo.svg",
  "https://upload.wikimedia.org/wikipedia/commons/5/5b/Bologna_F.C._1909_logo.svg",
  "https://upload.wikimedia.org/wikipedia/en/c/ce/Udinese_Calcio_logo.svg",
  "https://upload.wikimedia.org/wikipedia/commons/2/2d/SSC_Neapel.svg",
  "https://upload.wikimedia.org/wikipedia/en/f/f7/AS_Roma_logo_%282017%29.svg",
  "https://upload.wikimedia.org/wikipedia/en/6/61/Cagliari_Calcio_1920.svg",
  "https://upload.wikimedia.org/wikipedia/en/d/d2/U.C._Sampdoria_logo.svg",
  "https://upload.wikimedia.org/wikipedia/en/6/66/AtalantaBC.svg",
  "https://upload.wikimedia.org/wikipedia/en/c/ce/S.S._Lazio_badge.svg",
  "https://upload.wikimedia.org/wikipedia/commons/f/f2/2022_ACF_Fiorentina_logo.svg",
  "https://upload.wikimedia.org/wikipedia/commons/b/bc/Juventus_FC_2017_icon_%28black%29.svg",
  "https://upload.wikimedia.org/wikipedia/en/2/25/Genoa_C.F.C._logo.png",
  "https://upload.wikimedia.org/wikipedia/en/1/1c/US_Sassuolo_Calcio_logo.svg",
  "https://upload.wikimedia.org/wikipedia/commons/d/d0/Logo_of_AC_Milan.svg",
  "https://upload.wikimedia.org/wikipedia/en/8/85/US_Salernitana_1919_logo.svg",
  "https://upload.wikimedia.org/wikipedia/it/c/cf/Spezia_Calcio_logo_2023.svg",
  "https://upload.wikimedia.org/wikipedia/commons/7/74/2022_Venezia_FC_logo.svg"
  # "https://upload.wikimedia.org/wikipedia/en/0/0b/Frosinone_Calcio_logo.svg",
  # "https://upload.wikimedia.org/wikipedia/en/8/85/Us_lecce.svg",
  # "https://upload.wikimedia.org/wikipedia/en/a/a7/AC_Monza_logo_%282021%29.svg",
  )
  
  

# Calculate mean values for Att and Def
for (t in 1:length(teams)) {
  scatterplot_df$Att[t] <- mean(posterior[,,paste0("att[",t,"]")])
  scatterplot_df$Def[t] <- -mean(posterior[,,paste0("def[",t,"]")])
}

# Plot
ggplot(scatterplot_df,aes(Att, Def)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey35") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey35") +
  geom_image(aes(image = Logo), size = 0.06) + 
  ylim(-1,1.5)+
  labs(x= "Attack strength",
       y= "Defense strength",
       title = paste0("Teams's attack and defense abilities after matchday",
                      n_matchdays," (Serie A ,",SEASON,")"))+
  theme_linedraw()+
  theme(axis.title.x = element_text(size=12, face="bold", colour = "black"),
        axis.text.x = element_text(size=10, face="bold", colour = "black"),
        axis.title.y = element_text(size=12, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"),
        plot.title = element_text(hjust = 0.5,face = "bold")
        )


# Coefficients time series
offline_ts_df<- get_all_teams_data(teams_list=teams,start=19,end=38,models_dir_path =OFFLINE_MODELS_DIR)

offline_plot_list <- list()
offline_plot_list <- lapply(teams, function(t) {
  plot_parameters_ts(team = t,complete_df = offline_ts_df,start = 19,end = 38)
  }
)

(offline_plot_list[[1]] | offline_plot_list[[2]] |offline_plot_list[[3]] | offline_plot_list[[4]]) /
  (offline_plot_list[[5]] | offline_plot_list[[6]] |offline_plot_list[[7]] | offline_plot_list[[8]]) /
  (offline_plot_list[[9]] | offline_plot_list[[10]] |offline_plot_list[[11]] | offline_plot_list[[12]]) /
  (offline_plot_list[[13]] | offline_plot_list[[14]] |offline_plot_list[[15]] | offline_plot_list[[16]]) /
  (offline_plot_list[[17]] | offline_plot_list[[18]] |offline_plot_list[[19]] | offline_plot_list[[20]])


online_ts_df<- get_all_teams_data(teams_list=teams,start=19,end=38,models_dir_path =ONLINE_MODELS_DIR)

online_plot_list <- list()
online_plot_list <- lapply(teams, function(t) {
  plot_parameters_ts(team = t,complete_df = ts_df,start = 19,end = 38)
}
)

(online_plot_list[[1]] | online_plot_list[[2]] |online_plot_list[[3]] | online_plot_list[[4]]) /
  (online_plot_list[[5]] | online_plot_list[[6]] |online_plot_list[[7]] | online_plot_list[[8]]) /
  (online_plot_list[[9]] | online_plot_list[[10]] |online_plot_list[[11]] | online_plot_list[[12]]) /
  (online_plot_list[[13]] | online_plot_list[[14]] |online_plot_list[[15]] | online_plot_list[[16]]) /
  (online_plot_list[[17]] | online_plot_list[[18]] |online_plot_list[[19]] | online_plot_list[[20]])


