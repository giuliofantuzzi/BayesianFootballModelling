HDA_probabilities<- function(ht,at,model){
  probs<- vector(mode="numeric",length=3)

  posterior<- as.array(model)
  mu = posterior[,,"mu"]
  home=posterior[,,"home_advantage"]
  p = posterior[,,"p"]
  attH=posterior[,,paste0("att[",ht,"]")]
  defH=posterior[,,paste0("def[",ht,"]")]
  attA=posterior[,,paste0("att[",at,"]")]
  defA=posterior[,,paste0("def[",at,"]")]
  theta_H = exp(mu+home+attH+defA)
  theta_A = exp(mu+attA+defH)
  GD_pred<- mapply(my_rzeroinflatedskellam, 1,theta_H, theta_A,p)
  
  probs[1]<- round(mean(GD_pred > 0)*100,digits = 2)
  probs[2]<- round(mean(GD_pred == 0)*100,digits = 2)
  probs[3]<- round(mean(GD_pred < 0)*100,digits = 2)
  
  return(probs)
}