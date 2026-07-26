model {
  # Likelihood
  for(i in 1:Nplots){
    # The number of dead trees (Y) is binomially distributed given the 
    # probability of death (theta) and total trees (N) in plot i.
    Y[i] ~ dbin(theta, N[i])
  }
  # Prior for the scalar parameter theta (probability of tree dying)
  # A Beta(1,1) is a conjugate prior for a Binomial likelihood and acts 
  # as a uniform prior bounding the probability between 0 and 1.
  theta ~ dbeta(1, 1)

}