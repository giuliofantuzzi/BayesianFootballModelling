rm(list = ls())
#-------------------------------------------------------------------------------
# Libraries and utils
#-------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyverse)
source("utils/my_skellam.R")
#-------------------------------------------------------------------------------
# Global settings and variables
#-------------------------------------------------------------------------------
DATA_DIR= "data/"
SEASON="2122"
#-------------------------------------------------------------------------------
# Data loading and preparation
#-------------------------------------------------------------------------------
SerieA_data<- read.csv(file= paste0(DATA_DIR,"season_",SEASON,"/SerieA_",SEASON,".csv"))
SerieA_data<- SerieA_data %>% select(c("HomeTeam","AwayTeam","FTHG","FTAG"))
SerieA_data<- SerieA_data %>% mutate(GD=FTHG-FTAG)
SerieA_data<- SerieA_data %>% mutate(FinalResult=ifelse(GD>0,"HomeWin",ifelse(GD<0,"AwayWin","Draw")))
#-------------------------------------------------------------------------------
# Plots
#-------------------------------------------------------------------------------

# Plot the goal difference and demonstrate skellam is good choice
l1<- mean(SerieA_data$FTHG)
l2<- mean(SerieA_data$FTAG)
p=0.015
plot(table(SerieA_data$GD)/length(SerieA_data$GD),xlab = "Goal Difference",ylab = "Density")
x <- min(SerieA_data$GD):max(SerieA_data$GD)
lines(x+0.1, my_dskellam(x,l1,l2), type = 'h', col = '#641E16', lwd = 2, lty = 2)
lines(x+0.2, my_dzeroinflatedskellam(x,l1,l2,0.02), type = 'h', col = '#145A32', lwd = 2, lty = 2)
legend("topright", legend = c("Empirical densities", "Skellam densities","ZI-Skellam densities"), lty = c(1,2), 
       col = c("black", "#641E16","#145A32"))
title("Validity check of my skellam rng")

#-------------------------------------------------------------------------------

# Barplot of Home-Draw-Away
SerieA_data %>%
  count(FinalResult) %>%
  ggplot(aes(x = reorder(FinalResult, -n), y = n/nrow(SerieA_data), fill = FinalResult)) + 
  geom_bar(stat = "identity",col="black") +
  theme_minimal() + 
  labs(x = "", y = "Frequency", title = "Type of Victory",fill = "Match Outcome") +
  scale_fill_manual(values = c("HomeWin" = "#2A6426",
                               "AwayWin" = "#B0F1AC",
                               "Draw" = "#4FB448"))+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"),
        axis.title.y = element_text(size=12, face="bold", colour = "black"),
        axis.text.y = element_text(size=10, face="plain", colour = "black"),
        plot.title = element_text(hjust = 0.5,face = "bold"),
        legend.title = element_text(face="bold")
  )

#-------------------------------------------------------------------------------

# Total Goals scored and conceeded by team
home_scored <- aggregate(SerieA_data$FTHG, list(SerieA_data$HomeTeam), FUN = sum) 
away_scored <- aggregate(SerieA_data$FTAG, list(SerieA_data$AwayTeam), FUN = sum)
home_conceeded <- aggregate(SerieA_data$FTAG, list(SerieA_data$HomeTeam), FUN = sum) 
away_conceeded <- aggregate(SerieA_data$FTHG, list(SerieA_data$AwayTeam), FUN = sum)

colnames(home_scored) <- c("team", "home_scored")
colnames(away_scored) <- c("team", "away_scored")
colnames(home_conceeded) <- c("team", "home_conceeded")
colnames(away_conceeded) <- c("team", "away_conceeded")

goals_scored <- merge(home_scored, away_scored, by = "team")
goals_scored$tot_goals <- goals_scored$home_scored + goals_scored$away_scored

goals_conceeded <- merge(home_conceeded, away_conceeded, by = "team")
goals_conceeded$tot_goals <- goals_conceeded$home_conceeded + goals_conceeded$away_conceeded


goals_scored %>%
  mutate(team = fct_reorder(team, tot_goals))  %>%
  ggplot(aes(x = team, y = tot_goals)) +
  geom_bar(stat = "identity", col="black",fill = "green4", alpha = 0.6, width = 0.8) +
  geom_text(aes(label = tot_goals),hjust=-0.3,size = 4,color = "black") +
  coord_flip() +
  theme_minimal() +
  labs(x="Team",y = 'Number of Goals', title = 'Number of goals scored by team')+
  theme(axis.text.x = element_text(size=10, face="plain", colour = "black"),
        axis.title.x = element_text(size=12, face="bold", colour = "black"),
        axis.title.y = element_text(size=12, face="bold", colour = "black"),
        axis.text.y = element_text(size=10, face="plain", colour = "black"),
        plot.title = element_text(hjust = 0.5,face = "bold")
  )

goals_conceeded %>%
  mutate(team = fct_reorder(team, tot_goals))  %>%
  ggplot(aes(x = team, y = tot_goals)) +
  geom_bar(stat = "identity", col="black",fill = "red3", alpha = 0.6, width = 0.8) +
  geom_text(aes(label = tot_goals),hjust=-0.3,size = 4,color = "black") +
  coord_flip() +
  theme_minimal() +
  labs(x="Team",y = 'Number of Goals', title = 'Number of goals conceeded by team')+
  theme(axis.text.x = element_text(size=10, face="plain", colour = "black"),
        axis.title.x = element_text(size=12, face="bold", colour = "black"),
        axis.title.y = element_text(size=12, face="bold", colour = "black"),
        axis.text.y = element_text(size=10, face="plain", colour = "black"),
        plot.title = element_text(hjust = 0.5,face = "bold")
  )


# Goals scored and conceeded by team Home vs Away
goals_scored %>% 
  select(c(team,home_scored,away_scored)) %>%
  pivot_longer(cols = c(home_scored, away_scored), 
               names_to = "Stadium", 
               values_to = "Goals") %>%
  ggplot(aes(x = team, y = Goals, fill = Stadium)) +
  geom_bar(stat = "identity", position = "dodge",width = 0.5,col="black") +
  scale_fill_manual(values = c("home_scored" = "#145A32", "away_scored" = "#52BE80"),
                    labels = c("home_scored" = "Home", "away_scored" = "Away"))+
  coord_flip()+
  labs(title = "Goals scored by team (Home vs Away)", 
       x = "Team", 
       y = "Goals")+
  theme_minimal()+
  theme(axis.text.x = element_text(size=10, face="plain", colour = "black"),
        axis.title.x = element_text(size=12, face="bold", colour = "black"),
        axis.title.y = element_text(size=12, face="bold", colour = "black"),
        axis.text.y = element_text(size=10, face="plain", colour = "black"),
        plot.title = element_text(hjust = 0.5,face = "bold")
  )

# Goals conceeded Home vs Away
goals_conceeded %>% 
  select(c(team,home_conceeded,away_conceeded)) %>%
  pivot_longer(cols = c(home_conceeded, away_conceeded), 
               names_to = "Stadium", 
               values_to = "Goals") %>%
  ggplot(aes(x = team, y = Goals, fill = Stadium)) +
  geom_bar(stat = "identity", position = "dodge",width = 0.5,col="black") +
  scale_fill_manual(values = c("home_conceeded" = "#641E16", "away_conceeded" = "#EC7063"),
                    labels = c("home_conceeded" = "Home", "away_conceeded" = "Away"))+
  coord_flip()+
  labs(title = "Goals conceeded by team (Home vs Away)", 
       x = "Team", 
       y = "Goals")+
  theme_minimal()+
  theme(axis.text.x = element_text(size=10, face="plain", colour = "black"),
        axis.title.x = element_text(size=12, face="bold", colour = "black"),
        axis.title.y = element_text(size=12, face="bold", colour = "black"),
        axis.text.y = element_text(size=10, face="plain", colour = "black"),
        plot.title = element_text(hjust = 0.5,face = "bold")
  )

#-------------------------------------------------------------------------------
# Retrieve Final Rankings
#-------------------------------------------------------------------------------

# Assign points to each match outcome
SerieA_data <- SerieA_data %>%
  mutate(HomePoints = ifelse(FinalResult == "HomeWin", 3, ifelse(FinalResult == "Draw", 1, 0)),
         AwayPoints = ifelse(FinalResult == "AwayWin", 3, ifelse(FinalResult == "Draw", 1, 0)))

# Aggregate points for each team
total_points <- SerieA_data %>%
  group_by(Team = HomeTeam) %>%
  summarise(TotalPoints = sum(HomePoints)) %>%
  bind_rows(
    SerieA_data %>%
      group_by(Team = AwayTeam) %>%
      summarise(TotalPoints = sum(AwayPoints))
  ) %>%
  group_by(Team) %>%
  summarise(TotalPoints = sum(TotalPoints)) %>%
  arrange(desc(TotalPoints))
View(total_points)


