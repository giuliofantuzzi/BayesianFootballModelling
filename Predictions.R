#-------------------------------------------------------------------------------
# Libraries and utils
#-------------------------------------------------------------------------------
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyverse)
library(rstan)
library(bayesplot)
# In stan there wasn't a skellam_rng and the R base rskellam are "wrong"
source("utils/my_skellam.R")
# Decide if learn the online model or not
ONLINE_MODEL = TRUE
MATCHDAY_N   = 37
#-------------------------------------------------------------------------------
# Prepare the test-set (the unknown matchday 36)
#-------------------------------------------------------------------------------
SerieA_2324<- read.csv(file="data/season_2324/SerieA_2324.csv")
teams<- unique(SerieA_2324$HomeTeam)
teams<- str_replace_all(teams, " ", "")
n_teams<- length(teams)
test_set<- data.frame(HomeTeam=c("Fiorentina","Lecce","Torino","Sassuolo","Udinese","Monza","Inter","Roma","Salernitana","Bologna"),
                      AwayTeam=c("Napoli","Atalanta","Milan","Cagliari","Empoli","Frosinone","Lazio","Genoa","Verona","Juventus")
                      )


ht= unlist(sapply(1:nrow(test_set),function (g) which(teams==test_set$HomeTeam[g])))
at= unlist(sapply(1:nrow(test_set),function (g) which(teams==test_set$AwayTeam[g])))

#-------------------------------------------------------------------------------
# Load the fitted stan model and make predictions
#-------------------------------------------------------------------------------
# Load the model trained up to matchday 35
if(ONLINE_MODEL){
  load(file = paste0("estimated_models/online_models/matchday",MATCHDAY_N-1,"/KN_matchday",MATCHDAY_N-1,".rds"))
}else{
  load(file = paste0("estimated_models/models/matchday",MATCHDAY_N-1,"/KN_matchday",MATCHDAY_N-1,".rds"))
}

# Recall some paramters
n_chains<-4
n_iters<- 11000
n_warmup<- 1000

posterior<- as.array(KN_model)
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
  vline_at(0,linetype="dotted")+
  labs(title="Goal difference estimates for matchday n. 36",
       x = "Goal difference",
       y= "Match")+
  scale_x_continuous(breaks = -5:5) +
  theme(axis.title.x = element_text(size=12, face="bold", colour = "black"),
        axis.text.x = element_text(size=10, face="plain", colour = "black"),
        axis.title.y = element_text(size=12, face="bold", colour = "black"),
        axis.text.y = element_text(size=10, face="plain", colour = "black"),
        plot.title = element_text(hjust = 0.5,face = "bold")
  )

# H-D-A probabilities
test_set$Pr_H<-NA
test_set$Pr_D<-NA
test_set$Pr_A<-NA

for(m in 1:nrow(test_set)){
  #match_name<- paste0(teams[ht[m]],"-vs-",teams[at[m]])
  GD_pred<- GD_df[,m]
  test_set[m,"Pr_H"]<- round(mean(GD_pred > 0)*100,digits = 2)
  test_set[m,"Pr_D"]<- round(mean(GD_pred == 0)*100,digits = 2)
  test_set[m,"Pr_A"]<- round(mean(GD_pred < 0)*100,digits = 2)
}
