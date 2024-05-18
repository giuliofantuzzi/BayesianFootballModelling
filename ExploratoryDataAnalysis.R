library(ggplot2)
library(dplyr)
library(tidyverse)
#-------------------------------------------------------------------------------
# Data loading and preparation
#-------------------------------------------------------------------------------
SerieA_2324<- read.csv(file="data/season_2324/SerieA_2324.csv")
SerieA_2324<- SerieA_2324 %>% select(c("HomeTeam","AwayTeam","FTHG","FTAG"))
SerieA_2324<- SerieA_2324 %>% mutate(GD=FTHG-FTAG)
SerieA_2324<- SerieA_2324 %>% mutate(FinalResult=ifelse(GD>0,"HomeWin",ifelse(GD<0,"AwayWin","Draw")))



#-------------------------------------------------------------------------------
# Barplot of Home-Draw-Away
SerieA_2324 %>%
  count(FinalResult) %>%
  ggplot(aes(x = reorder(FinalResult, -n), y = n/nrow(SerieA_2324), fill = FinalResult)) + 
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

#-------------------------------------------------------------------------------
# Assign points to each match outcome
SerieA_2324 <- SerieA_2324 %>%
  mutate(HomePoints = ifelse(FinalResult == "HomeWin", 3, ifelse(FinalResult == "Draw", 1, 0)),
         AwayPoints = ifelse(FinalResult == "AwayWin", 3, ifelse(FinalResult == "Draw", 1, 0)))

# Aggregate points for each team
total_points <- SerieA_2324 %>%
  group_by(Team = HomeTeam) %>%
  summarise(TotalPoints = sum(HomePoints)) %>%
  bind_rows(
    SerieA_2324 %>%
      group_by(Team = AwayTeam) %>%
      summarise(TotalPoints = sum(AwayPoints))
  ) %>%
  group_by(Team) %>%
  summarise(TotalPoints = sum(TotalPoints)) %>%
  arrange(desc(TotalPoints))
View(total_points)
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Total Goals scored and conceeded by team
home_scored <- aggregate(SerieA_2324$FTHG, list(SerieA_2324$HomeTeam), FUN = sum) 
away_scored <- aggregate(SerieA_2324$FTAG, list(SerieA_2324$AwayTeam), FUN = sum)
home_conceeded <- aggregate(SerieA_2324$FTAG, list(SerieA_2324$HomeTeam), FUN = sum) 
away_conceeded <- aggregate(SerieA_2324$FTHG, list(SerieA_2324$AwayTeam), FUN = sum)

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
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
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
