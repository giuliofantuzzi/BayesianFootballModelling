#-------------------------------------------------------------------------------
# Libraries and lazy options
#-------------------------------------------------------------------------------
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyverse)
library(rstan)
library(bayesplot)

options(mc.cores = parallel::detectCores())
rstan_options(threads_per_chain = 2)

#-------------------------------------------------------------------------------
# Data loading and preparation
#-------------------------------------------------------------------------------
PremierLeague_0607<- read.csv(file="data/season_0607/PremierLeague_0607.csv")
PremierLeague_0607<- PremierLeague_0607[,c("HomeTeam","AwayTeam","FTHG","FTAG")]

n_games<- nrow(PremierLeague_0607)
teams<- unique(PremierLeague_0607$HomeTeam)

ht= unlist(sapply(1:n_games,function (g) which(teams==PremierLeague_0607$HomeTeam[g])))
at= unlist(sapply(1:n_games,function (g) which(teams==PremierLeague_0607$AwayTeam[g])))

# Remove blank space in teams (needed for bayesplots)
teams<- str_replace_all(teams, " ", "")
n_teams<- length(teams)

stan_PremierLeague_0607 = list(
  n_teams=n_teams,
  n_games=n_games,
  home_team= ht[1:n_games],
  away_team= at[1:n_games],
  goal_difference = PremierLeague_0607$FTHG[1:n_games]-PremierLeague_0607$FTAG[1:n_games]
)

#-------------------------------------------------------------------------------
# Model
#-------------------------------------------------------------------------------
n_chains<-4
n_iters<- 11000
n_warmup<- 1000
KN_model <- stan(file = 'stan/karlis-ntzoufras.stan',
                 data = stan_PremierLeague_0607,
                 chains = n_chains,
                 iter = n_iters,
                 warmup = n_warmup
                 )

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
  xlim(-5,5)+
  ggtitle("MCMC areas for attack coefficients")

color_scheme_set("red")
mcmc_areas(posterior,pars=def_params)+
  scale_y_discrete(labels=def_labels)+
  xlim(-5,5)+
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



#-------------------------------------------------------------------------------
# Predictions
#-------------------------------------------------------------------------------

# In stan there wasn't a skellam_rng
# The R base rskellam are "wrong"
source("my_skellam.R")

test_set<- PremierLeague_0607[(nrow(PremierLeague_0607)-9):nrow(PremierLeague_0607),]
ht= unlist(sapply(1:nrow(test_set),function (g) which(teams==test_set$HomeTeam[g])))
at= unlist(sapply(1:nrow(test_set),function (g) which(teams==test_set$AwayTeam[g])))

mu = posterior[,,"mu"]
home=posterior[,,"home_advantage"]
p = posterior[,,"p"]


GD_df<- data.frame(matrix(NA,nrow = n_chains*(n_iters-n_warmup),ncol=nrow(test_set)))

for(m in 1:nrow(test_set)){
  attH=posterior[,,paste0("att[",ht[m],"]")]
  defH=posterior[,,paste0("def[",ht[m],"]")]
  attA=posterior[,,paste0("att[",at[m],"]")]
  defA=posterior[,,paste0("def[",at[m],"]")]
  theta_H = exp(mu+home+attH+defA)
  theta_A = exp(mu+attA+defH)
  result <- mapply(my_rzeroinflatedskellam, 1,theta_H, theta_A,p)
  #cat("Estimated GD for match",teams[ht[m]],"vs",teams[at[m]],"-->",median(result),"\n")
  GD_df[,m]<- result
  colnames(GD_df)[m]<-paste0(teams[ht[m]],"-vs-",teams[at[m]])
}


color_scheme_set("green")
mcmc_intervals(GD_df)+
  ggtitle("Goal difference estimates for Test Matches")+
  vline_at(0,linetype="dotted")
