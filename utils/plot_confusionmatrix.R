plot_confusionmatrix <- function(confusionmatrix, modelname){
  # Convert confusion matrix into df
  df <- data.frame(confusionmatrix$`Confusion Matrix`[[1]])
  
  # Reorder the levels of the 'Target' and 'Prediction' columns
  df$Target <- factor(df$Target, levels = c("H", "D", "A"))
  df$Prediction <- factor(df$Prediction, levels = c("A", "D", "H"))
  
  plot <- ggplot(df, aes(x = Target, y = Prediction, fill = N)) +
    geom_tile(color = "black", lwd = 0.4) +
    geom_text(aes(label = N), color = "black", size = 4) +
    xlab("Actual values") +
    ylab("Predicted values") +
    ggtitle(modelname) +
    scale_fill_gradient(low = "white", high = "#0EBAE6") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 11),
      legend.position = "none"
    )
  return(plot)
}