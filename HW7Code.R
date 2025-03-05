################################################################################
# Homework 7
# Caroline Devine
################################################################################

################################################################################
# Problem 1
################################################################################

# Write a pois.prob function. 

pois.prob <- function(x, lambda, type){
  # inputs are x, lambda (only parameter of Poison Distribution, and 
  #the probability we want to compute
  # lambda = the mean number of events
  
  # Use dpois and ppois to conditionally return the correct probability
  
  if (type == "=="){
    # P(X = x) -> pmf
    output = dpois(x, lambda)
  }else if (type == "!="){
    # P(X != x) -> complement rule
    output = 1 - (dpois(x, lambda))
  }else if (type == "<"){
    # P(X < x) -> uses cdf
    output = ppois((x-1), lambda)
  }else if (type == "<="){
    # P(X <= x) -> cdf
    output = ppois(x, lambda)
  }else if (type == ">"){
    # P(X > x) -> cdf + complement rule
    output = 1 - (ppois(x, lambda))
  }else if (type == ">="){
    # P(X >= x) -> cdf + complement rule
    output = 1 - (ppois((x-1), lambda))
  } else {
    output = "Not correct type"
  }
  return(output)
  
}

# Example Run Through
# Question: P( X = x) where x = 0, lambda = 2 following Poison Distribution
pois.prob(0,2,"==")
# Correct, outputs 0.1353 and that is e^-2 which is the answer
#when calculated analytically

################################################################################
# Problem 2
################################################################################

