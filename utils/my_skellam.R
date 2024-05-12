my_dskellam <- function(x, lambda1, lambda2) {
  x=round(x) # to manage the case x is not integer
  return(exp(-(lambda1+lambda2))*(lambda1/lambda2)^{x/2}*besselI(x=2*sqrt(lambda1*lambda2),nu = abs(x)))
}

my_dzeroinflatedskellam <- function(x, lambda1, lambda2,p) {
  x=round(x) # to manage the case x is not integer
  prob=(1-p)*exp(-(lambda1+lambda2))*(lambda1/lambda2)^{x/2}*besselI(x=2*sqrt(lambda1*lambda2),nu = abs(x))
  if(x==0){
    prob=prob+p
  }
  return(prob)
}

# my_rskellam<- function(n,lambda1,lambda2){
#   samples<-numeric(0)
#   while (length(samples)<n) {
#     x_star<- rnorm(n=1,mean =(lambda1-lambda2),sd=sqrt(lambda1+lambda2))
#     acc_prob<- my_dskellam(x_star,lambda1,lambda2)/dnorm(x_star,mean =(lambda1-lambda2),sd=sqrt(lambda1+lambda2))
#     if (runif(1) < acc_prob) {
#       samples <- c(samples, round(x_star))
#     }
#   }
#   return(samples)
# }

# my_rzeroinflatedskellam<- function(n,lambda1,lambda2,p){
#   samples<-numeric(0)
#   while (length(samples)<n) {
#     x_star<- rnorm(n=1,mean =(lambda1-lambda2),sd=sqrt(lambda1+lambda2))
#     acc_prob<- my_dzeroinflatedskellam(x_star,lambda1,lambda2,p)/dnorm(x_star,mean =(lambda1-lambda2),sd=sqrt(lambda1+lambda2))
#     if (runif(1) < acc_prob) {
#       samples <- c(samples, round(x_star))
#     }
#   }
#   return(samples)
# }
library(skellam)
# NB: rskallam internally computes 2 rpois, and I don't want this
#     but still I can exploit the function qskellam without re-inventing the wheel
my_rskellam<- function(n,lambda1,lambda2){
  # Inverse sampling
  q= runif(n,min=0,max=1)
  return(qskellam(q,lambda1,lambda2))
}

# Test that it works:
# simulated_data= my_rskellam(10000,4,2)
# plot(table(simulated_data)/length(simulated_data),xlab = "values",ylab="density")
# # Overlay theoretical PMF plot
# success <- min(simulated_data):max(simulated_data)
# lines(success+0.1, dskellam(success,4,2), type = 'h', col = 'red', lwd = 2, lty = 2)
# legend("topright", legend = c("Empirical densities", "Theoretical densities"), lty = c(1,2), 
#        col = c("black", "red"), bty = "n")
# title("Validity check of my skellam rng")

my_rzeroinflatedskellam<- function(n,lambda1,lambda2,p){
  samples<- vector(mode="numeric",length = n)
  for(i in 1:n){
    # I generate 0 with probability p
    # Or a random number from a (not zero-inflated) skellam with prob (1-p)
    if(runif(1)>p){
      samples[i]<- my_rskellam(1,lambda1,lambda2)
    }
    #else not needed since there are already zeros!
  }
  return(samples)
}



