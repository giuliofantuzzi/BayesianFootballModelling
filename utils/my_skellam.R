library(skellam)
my_dskellam <- function(x, lambda1, lambda2) {
  x=round(x) # to manage the case x is not integer
  return(exp(-(lambda1+lambda2))*(lambda1/lambda2)^{x/2}*besselI(x=2*sqrt(lambda1*lambda2),nu = abs(x)))
}

my_dzeroinflatedskellam <- function(x, lambda1, lambda2, p) {
  # Ensure x is a vector of integers
  x <- round(x)
  # Calculate the probabilities for each element in x, lambda1, lambda2, and p
  prob <- (1 - p) * exp(-(lambda1 + lambda2)) * (lambda1 / lambda2)^(x / 2) * besselI(x = 2 * sqrt(lambda1 * lambda2), nu = abs(x))
  # If x is zero, add p to the probability
  prob[x == 0] <- prob[x == 0] + p
  return(prob)
}

my_rskellam<- function(n,lambda1,lambda2){
  # Inverse sampling
  q= runif(n,min=0,max=1)
  return(qskellam(q,lambda1,lambda2))
}

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
