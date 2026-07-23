model{
  for(i in 1:N){
    # Likelihood for annual water use:
    Y[i] ~ dnorm(XXX, tau)
    # Replicated data:
    XXX
    # Regression model with coefficients that vary by vegetation type,
    # include main effects of MAP and MAT
    mu[i] = b0[veg[i]] + b[1,veg[i]]*MAT[i] + XXX
  }

  # Conjugate, hierarchical priors for the vegetation-type-specific
  # coefficients:
  for(v in 1:Nveg){
    for(k in 1:3){
      # Climate effects:
      XXXX
    }
    # Intercept:
    b0[v] ~ dnorm(mu.b0, tau.b0)
  }
  
  # Conjugate, relatively non-informative priors for population-level 
  # (root node) parameters:
  for(k in 1:3){
    # Overall climate effects:
    mu.b[k] ~ XXXX
    # Precision (compute standard deviation) describing variability
    # among vegetation types wrt to their climate effects:
    tau.b[k] ~ XXXX
    # Standard deviations for coefficients:
    sig.b[k] = 1/sqrt(tau.b[k])
  }
  # Overall intercept:
  mu.b0 ~ XXXX
  # Precision (and standard deviation) describing variability among
  # vegetation types wrt to their intercept (baseline water loss):
  tau.b0 ~ XXXX
  # Standard deviation for intercept:
  sig.b0 = 1/sqrt(tau.b0)
  
  # Residual precision (and standard deviation) prior:
  tau ~ XXXX
  # Standard deviation residual variance:
  sig = 1/sqrt(tau)
  

 
}