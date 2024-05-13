library(ggplot2)
library(dplyr)
library(tidyverse)

plot_parameters_ts<- function(team,complete_df,start=19,end=35){
  plot<- complete_df%>% filter(Team==team & Matchday >= start & Matchday <= end) %>%
    ggplot( aes(x = Matchday, y = Mean, ymin = Lower, ymax = Upper, fill = Type))+
    geom_line(aes(color = Type), linewidth = 1) +
    geom_point()+
    geom_ribbon(aes(fill = Type), alpha = 0.20) +
    labs(title = paste0(team," abilities over time"), x = "Matchday", y = "Coefficient") +
    scale_color_manual(values = c("blue", "red")) +  # Specify colors for attack and defence
    scale_fill_manual(values = c("blue", "red")) +  # Hide the legend for fill
    theme_minimal()+
    theme(#axis.title.x = element_text(size=12, face="bold", colour = "black"),
      #axis.text.x = element_text(size=10, face="bold", colour = "black"),
      #axis.title.y = element_text(size=12, face="bold", colour = "black"),
      #axis.text.y = element_text(size=10, face="bold", colour = "black"),
      plot.title = element_text(hjust = 0.5,face = "bold")
    )
  return(plot)
}