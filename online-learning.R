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
# Estimation of the models over time (but with an online approach)
#-------------------------------------------------------------------------------
n_chains<-4
n_iters<- 11000
n_warmup<- 1000

# I already fitted model after the 1 half of the league in another script
# In case you don't, just fit it before the cycle
# (e-g. passing vectors of 0 means and 10 sd as default --- or using the other stan model)


for(i in 20:35){
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  cat("...Parameters estimation after matchday n.",i,"...\n")
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  #---------------------------------------------------------------------------
  # (1) Training and test set for the current matchday
  cat("...Preparing the training set...\n")
  training<- SerieA_2324[1:(10*i),c("HomeTeam","AwayTeam","FTHG","FTAG")]
  training<- na.omit(training) #just to manage if some matches were postponed...
  #---------------------------------------------------------------------------
  # (2) Retrieving the previous estimates
  cat("...Retrieving previous prior information...\n")
  load(paste0("stan/online_models/matchday",i-1,"/KN_matchday",i-1,".rds"))
  prev_att_means= unlist(sapply(1:n_teams,function (t) mean(as.array(KN_model)[,,paste0("att[",t,"]")])))
  prev_def_means= unlist(sapply(1:n_teams,function (t) mean(as.array(KN_model)[,,paste0("def[",t,"]")])))
  prev_mu_mean= mean(as.array(KN_model)[,,"mu"])
  prev_home_advantage_mean= mean(as.array(KN_model)[,,"home_advantage"])
  prev_att_sd= unlist(sapply(1:n_teams,function (t) sd(as.array(KN_model)[,,paste0("att[",t,"]")])))
  prev_def_sd= unlist(sapply(1:n_teams,function (t) sd(as.array(KN_model)[,,paste0("def[",t,"]")])))
  prev_mu_sd= sd(as.array(KN_model)[,,"mu"])
  prev_home_advantage_sd= sd(as.array(KN_model)[,,"home_advantage"])
  #---------------------------------------------------------------------------
  # (3) Prepare the parameters for stan
  stan_paramters = list(
    n_teams=n_teams,
    n_games=nrow(training),
    home_team= ht[1:nrow(training)],
    away_team= at[1:nrow(training)],
    goal_difference = SerieA_2324$FTHG[1:nrow(training)]-SerieA_2324$FTAG[1:nrow(training)],
    prev_att_means=prev_att_means,
    prev_def_means=prev_def_means,
    prev_mu_mean=prev_mu_mean,
    prev_home_advantage_mean=prev_home_advantage_mean,
    prev_att_sd=prev_att_sd,
    prev_def_sd=prev_def_sd,
    prev_mu_sd=prev_mu_sd,
    prev_home_advantage_sd=prev_home_advantage_sd
    )
  # (4) Fit the model
  cat("...Fitting the model...\n")
  KN_model <- stan(file = 'stan/online.stan',
                   data = stan_paramters,
                   chains = n_chains,
                   iter = n_iters,
                   warmup = n_warmup,
                   seed = 16
  )
  # (5) Save the model
  dir.create(paste0("stan/online_models/matchday",i))
  save(KN_model,file=paste0("stan/online_models/matchday",i,"/KN_matchday",i,".rds"))
}




# #--- A glimpse to what changed
# test_set<- data.frame(HomeTeam=c("Frosinone","Napoli","Milan","Lazio","Genoa","Verona","Juventus","Atalanta","Lecce","Fiorentina"),
#                       AwayTeam=c("Inter","Bologna","Cagliari","Empoli","Sassuolo","Torino","Salernitana","Roma","Udinese","Monza")
# )
# 
# 
# ht= unlist(sapply(1:nrow(test_set),function (g) which(teams==test_set$HomeTeam[g])))
# at= unlist(sapply(1:nrow(test_set),function (g) which(teams==test_set$AwayTeam[g])))
# 
# #-------------------------------------------------------------------------------
# # Load the fitted stan model and make predictions
# #-------------------------------------------------------------------------------
# # Load the model trained up to matchday 35
# load(file = "stan/online_models/matchday35/KN_matchday35.rds")
# source("utils/my_skellam.R")
# # Recall some paramters
# n_chains<-4
# n_iters<- 11000
# n_warmup<- 1000
# 
# posterior<- as.array(KN_model)
# mu = posterior[,,"mu"]
# home=posterior[,,"home_advantage"]
# p = posterior[,,"p"]
# 
# GD_df<- data.frame(matrix(NA,nrow = n_chains*(n_iters-n_warmup),ncol=nrow(test_set)))
# 
# for(m in 1:nrow(test_set)){
#   attH=posterior[,,paste0("att[",ht[m],"]")]
#   defH=posterior[,,paste0("def[",ht[m],"]")]
#   attA=posterior[,,paste0("att[",at[m],"]")]
#   defA=posterior[,,paste0("def[",at[m],"]")]
#   theta_H = exp(mu+home+attH+defA)
#   theta_A = exp(mu+attA+defH)
#   result <- mapply(my_rzeroinflatedskellam, 1,theta_H, theta_A,p)
#   #cat("Estimated GD for match",teams[ht[m]],"vs",teams[at[m]],"-->",median(result),"\n")
#   GD_df[,m]<- result
#   colnames(GD_df)[m]<-paste0(teams[ht[m]],"-vs-",teams[at[m]])
# }
# 
# library(ggplot2)
# library(bayesplot)
# color_scheme_set("green")
# mcmc_intervals(GD_df)+
#   vline_at(0,linetype="dotted")+
#   labs(title="Goal difference estimates for matchday n. 36",
#        x = "Goal difference",
#        y= "Match")+
#   scale_x_continuous(breaks = -5:5) +
#   theme(axis.title.x = element_text(size=12, face="bold", colour = "black"),
#         axis.text.x = element_text(size=10, face="plain", colour = "black"),
#         axis.title.y = element_text(size=12, face="bold", colour = "black"),
#         axis.text.y = element_text(size=10, face="plain", colour = "black"),
#         plot.title = element_text(hjust = 0.5,face = "bold")
#   )
# 
# # H-D-A probabilities
# test_set$Pr_H<-NA
# test_set$Pr_D<-NA
# test_set$Pr_A<-NA
# 
# for(m in 1:nrow(test_set)){
#   #match_name<- paste0(teams[ht[m]],"-vs-",teams[at[m]])
#   GD_pred<- GD_df[,m]
#   test_set[m,"Pr_H"]<- round(mean(GD_pred > 0)*100,digits = 2)
#   test_set[m,"Pr_D"]<- round(mean(GD_pred == 0)*100,digits = 2)
#   test_set[m,"Pr_A"]<- round(mean(GD_pred < 0)*100,digits = 2)
# }
# 
# 
# 
# 
# # Parameters over time
# 
# get_all_teams_data <- function(teams_list, start = 19, end = 35) {
#   all_teams_data <- data.frame(Team=character(),
#                                Matchday=integer(),
#                                Mean=double(),
#                                Sd=double(),
#                                Lower=double(),
#                                Upper=double(),
#                                Type=character())
#   for (m in start:end) {
#     load(paste0("stan/models/matchday", m, "/KN_matchday", m, ".rds"))
#     posterior <- as.array(KN_model)
#     for (t in teams_list) {
#       team_idx <- match(t, teams_list)
#       att <- posterior[, , paste0("att[", team_idx, "]")]
#       def <- posterior[, , paste0("def[", team_idx, "]")]
#       team_data <- data.frame(
#         "Team" = rep(t,2),
#         "Matchday" =rep(m,2),
#         "Mean" = c(mean(att), mean(def)),
#         "Sd" =  c(sd(att), sd(def)),
#         "Lower" = c(quantile(att,0.025), quantile(def,0.025)),
#         "Upper" = c(quantile(att,0.975), quantile(def,0.975)),
#         "Type" = c("att", "def")
#       )
#       all_teams_data <- rbind(all_teams_data, team_data)
#     }
#   }
#   return(all_teams_data)
# }
# 
# 
# #-------------------------------------------------------------------------------
# # Plot the timeseries for each team
# #-------------------------------------------------------------------------------
# 
# ts_df<- get_all_teams_data(teams_list=teams,start=19,end=35)
# 
# plot_parameters_ts<- function(team,complete_df,start=19,end=35){
#   plot<- complete_df%>% filter(Team==team & Matchday >= start & Matchday <= end) %>%
#     ggplot( aes(x = Matchday, y = Mean, ymin = Lower, ymax = Upper, fill = Type))+
#     geom_line(aes(color = Type), linewidth = 1) +
#     geom_point()+
#     geom_ribbon(aes(fill = Type), alpha = 0.20) +
#     labs(title = paste0(team," abilities over time"), x = "Matchday", y = "Coefficient") +
#     scale_color_manual(values = c("blue", "red")) +  # Specify colors for attack and defence
#     scale_fill_manual(values = c("blue", "red")) +  # Hide the legend for fill
#     theme_minimal()+
#     theme(#axis.title.x = element_text(size=12, face="bold", colour = "black"),
#       #axis.text.x = element_text(size=10, face="bold", colour = "black"),
#       #axis.title.y = element_text(size=12, face="bold", colour = "black"),
#       #axis.text.y = element_text(size=10, face="bold", colour = "black"),
#       plot.title = element_text(hjust = 0.5,face = "bold")
#     )
#   return(plot)
# }
# 
# 
# plot_list <- list()
# 
# plot_list <- lapply(teams, function(t) {
#   plot_parameters_ts(team = t,complete_df = ts_df,start = 19,end = 35)
# }
# )
# 
# (plot_list[[1]] | plot_list[[2]] |plot_list[[3]] | plot_list[[4]]) /
#   (plot_list[[5]] | plot_list[[6]] |plot_list[[7]] | plot_list[[8]]) /
#   (plot_list[[9]] | plot_list[[10]] |plot_list[[11]] | plot_list[[12]]) /
#   (plot_list[[13]] | plot_list[[14]] |plot_list[[15]] | plot_list[[16]]) /
#   (plot_list[[17]] | plot_list[[18]] |plot_list[[19]] | plot_list[[20]])
# 
# 
# 
