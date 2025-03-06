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
  } 
  return(output)
  
}

# Example Run Through
# Question: P( X = x) where x = 0, lambda = 2 following Poison Distribution
pois.prob(x = 0, lambda = 2,"==")
# Correct, outputs 0.1353 and that is e^-2 which is the answer
#when calculated analytically

################################################################################
# Problem 2
################################################################################

# Write a beta.prob function. 
beta.prob <- function(x, alpha, beta, type){
  # alpha = success parameter
  # beta = failure parameter
  # support = [0,1] inclusive
  # Beta Distribution is continuous, not discrete
  
  
  # Use dbeta and pbeta to conditionally return the correct probability
 
  if (type == "=="){
    # P(X = x) -> pdf
    output = dbeta(x, alpha, beta)
  }else if (type == "!="){
    # P(X != x) -> pdf, complement rule
    output = 1 - (dbeta(x, alpha, beta))
  }else if (type == "<"){
    # P(X < x) -> uses cdf.      -> same as <=
    output = pbeta(x, alpha, beta)
  }else if (type == "<="){
    # P(X <= x) -> cdf , same as < due to continuous
    output = pbeta(x, alpha, beta)
  }else if (type == ">"){
    # P(X > x) -> cdf + complement rule
    output = 1 - (pbeta(x, alpha, beta))
  }else if (type == ">="){
    # P(X >= x) -> cdf + complement rule, same as >
    output = 1 - (pbeta(x, alpha, beta))
  }
  return(output)
  
}

# Example Run Through
beta.prob(x = 0.4, alpha = 2, beta = 5, ">=")
