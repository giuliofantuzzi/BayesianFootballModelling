#-------------------------------------------------------------------------------
# Libraries and lazy options
#-------------------------------------------------------------------------------
library(ggplot2)
library(ggrepel)
library(ggimage)
library(dplyr)
library(tidyverse)
library(rstan)
library(bayesplot)

options(mc.cores = parallel::detectCores())
rstan_options(threads_per_chain = 2)

#-------------------------------------------------------------------------------
# Data loading and preparation
#-------------------------------------------------------------------------------
SerieA_2324<- read.csv(file="data/season_2324/SerieA_2324.csv")
SerieA_2324<- SerieA_2324[,c("HomeTeam","AwayTeam","FTHG","FTAG")]

n_games<- nrow(SerieA_2324)
teams<- unique(SerieA_2324$HomeTeam)

ht= unlist(sapply(1:n_games,function (g) which(teams==SerieA_2324$HomeTeam[g])))
at= unlist(sapply(1:n_games,function (g) which(teams==SerieA_2324$AwayTeam[g])))

# Remove blank space in teams (needed for bayesplots)
teams<- str_replace_all(teams, " ", "")
n_teams<- length(teams)

stan_SerieA_2324 = list(
  n_teams=n_teams,
  n_games=n_games,
  home_team= ht[1:n_games],
  away_team= at[1:n_games],
  goal_difference = SerieA_2324$FTHG[1:n_games]-SerieA_2324$FTAG[1:n_games]
)

#-------------------------------------------------------------------------------
# Model
#-------------------------------------------------------------------------------
n_chains<-4
n_iters<- 11000
n_warmup<- 1000
KN_model <- stan(file = 'stan/karlis-ntzoufras.stan',
                 data = stan_SerieA_2324,
                 chains = n_chains,
                 iter = n_iters,
                 warmup = n_warmup,
                 seed = 16
)
#save(KN_model, file = "stan/KN_serieA.Rdata")
load(file = "stan/KN_serieA.Rdata")
par_names<-  rownames(summary(KN_model)$summary)
useful_par_names<- par_names[!(grepl("raw", par_names))]
print(KN_model,par=useful_par_names)
posterior<- as.array(KN_model)

#-------------------------------------------------------------------------------
# Plots
#-------------------------------------------------------------------------------

# Helper quantities to parse parameters/parameters names in bayesplots
att_params <- useful_par_names[grepl("att", useful_par_names)]
def_params <- useful_par_names[grepl("def", useful_par_names)]
att_labels <- setNames(teams, paste0("att[", 1:20, "]"))
def_labels <- setNames(teams, paste0("def[", 1:20, "]"))

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
dens_overlay_att[[1]]$Parameter = rep(att_labels,each=n_chains*(n_iters-n_warmup))
dens_overlay_att


color_scheme_set("red")
dens_overlay_def=mcmc_dens_overlay(posterior,pars=def_params)
# Here we need to do something different to re-name the parameters in the plot!
dens_overlay_def[[1]]$Parameter = rep(def_labels,each=n_chains*(n_iters-n_warmup))
dens_overlay_def


# Plots with logos

#library(magick)

# Create scatterplot_df dataframe
scatterplot_df <- data.frame(
  "Team" = teams,
  "Att" = NA,
  "Def" = NA,
  "Logo" = NA
)

scatterplot_df$Logo <- c(
  "https://upload.wikimedia.org/wikipedia/en/e/e9/Empoli_F.C._logo_%282021%29.png",
  "https://upload.wikimedia.org/wikipedia/en/0/0b/Frosinone_Calcio_logo.svg",
  "https://upload.wikimedia.org/wikipedia/en/2/25/Genoa_C.F.C._logo.png",
  "https://upload.wikimedia.org/wikipedia/commons/0/05/FC_Internazionale_Milano_2021.svg",
  "https://upload.wikimedia.org/wikipedia/en/f/f7/AS_Roma_logo_%282017%29.svg",
  "https://upload.wikimedia.org/wikipedia/en/1/1c/US_Sassuolo_Calcio_logo.svg",
  "https://upload.wikimedia.org/wikipedia/en/8/85/Us_lecce.svg",
  "https://upload.wikimedia.org/wikipedia/en/c/ce/Udinese_Calcio_logo.svg",
  "https://upload.wikimedia.org/wikipedia/en/2/2e/Torino_FC_Logo.svg",
  "https://upload.wikimedia.org/wikipedia/commons/5/5b/Bologna_F.C._1909_logo.svg",
  "https://upload.wikimedia.org/wikipedia/en/a/a7/AC_Monza_logo_%282021%29.svg",
  "https://upload.wikimedia.org/wikipedia/commons/d/d0/Logo_of_AC_Milan.svg",
  "https://upload.wikimedia.org/wikipedia/en/9/92/Hellas_Verona_FC_logo_%282020%29.svg",
  "https://upload.wikimedia.org/wikipedia/commons/f/f2/2022_ACF_Fiorentina_logo.svg",
  "https://upload.wikimedia.org/wikipedia/commons/b/bc/Juventus_FC_2017_icon_%28black%29.svg",
  "https://upload.wikimedia.org/wikipedia/en/c/ce/S.S._Lazio_badge.svg",
  "https://upload.wikimedia.org/wikipedia/commons/2/2d/SSC_Neapel.svg",
  "https://upload.wikimedia.org/wikipedia/en/8/85/US_Salernitana_1919_logo.svg",
  "https://upload.wikimedia.org/wikipedia/en/6/61/Cagliari_Calcio_1920.svg",
  "https://upload.wikimedia.org/wikipedia/en/6/66/AtalantaBC.svg"
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
  geom_image(aes(image = Logo), size = 0.08) + 
  xlab("Attack strength") + 
  ylab("Defense strength") + 
  ggtitle("Teams's attack and defense abilities (Serie A 2023-2024)")+
  theme_linedraw()+
  theme(axis.title.x = element_text(size=12, face="bold", colour = "black"),
        axis.text.x = element_text(size=10, face="bold", colour = "black"),
        axis.title.y = element_text(size=12, face="bold", colour = "black"),
        axis.text.y = element_text(size=10, face="bold", colour = "black"),
        plot.title = element_text(hjust = 0.5,face = "bold")
        )


