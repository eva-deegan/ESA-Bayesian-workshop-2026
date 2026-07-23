model{
  for(i in 1:N){
    # Likelihood for annual water use:
    Y[i] ~ dnorm(mu[i], tau)
    Yrep[i] ~ dnorm(mu[i], tau)
    # Regression model with coefficients that vary by vegetation type,
    # include main effects of MAP and MAT, their quadratic terms, and
    # their 2-way interaction:
    mu[i] = b0[veg[i]] + b[1,veg[i]]*MAT[i] + b[2,veg[i]]*MAP[i] +
      b[3,veg[i]]*pow(MAT[i],2) + XXXX +
      b[5,veg[i]]*MAT[i]*MAP[i] + eps[metID[i]] + XXXX]
  }
  
  # Zero-centered hierarchical prior for the study effects with sum-to-zero
  # constraint:
  for(i in XXXX){
    XXXX
  }
  # Sum-to-zero for study effects:
  XXXX

  # Zero-centered hierarchical prior for the method effects with post-sweeping
  # to compute identifiable method random effects:
  for(m in XXXX){
    eps[m] ~ dnorm(0,tau.eps)
    # Post-sweeping to compute identifiable method random effect:
    XXXX
  }
  XXXX
  
  # Conjugate, hierarchical priors for the vegetation-type-specific
  # coefficients:
  for(v in 1:Nveg){
    for(k in 1:5){
      # Climate effects:
      b[k,v] ~ dnorm(mu.b[k], tau.b[k])
    }
    # Intercept:
    b0[v] ~ dnorm(mu.b0, tau.b0)
    # Identifiable intercept:
    XXXX
  }
  
  # Conjugate, relatively non-informative priors for population-level 
  # (root node) parameters:
  for(k in 1:5){
    # Overall climate effects:
    mu.b[k] ~ dnorm(0,0.0001)
    # Precision (compute standard deviation) describing variability
    # among vegetation types wrt to their climate effects:
    tau.b[k] ~ dgamma(0.01, 0.01)
    # Standard deviations for coefficients:
    sig.b[k] = 1/sqrt(tau.b[k])
  }
  # Overall intercept:
  mu.b0 ~ dnorm(0,0.0001)
  # Identifiable overall intercept:
  XXXX
  
  # Precision (and standard deviation) describing variability among
  # vegetation types wrt to their intercept (baseline water loss):
  tau.b0 ~ dgamma(0.01, 0.01)
  # Standard deviation for intercept:
  sig.b0 = 1/sqrt(tau.b0)
  
  # Residual precision (and standard deviation) prior:
  tau ~ dgamma(0.01, 0.01)
  # Standard deviation residual variance:
  sig = 1/sqrt(tau)
  
  # Conjugate relatively non-informative gamma priors for the random effects
  # precision terms:
  tau.gam ~ dgamma(0.1, 0.1)
  tau.eps ~ dgamma(0.1, 0.1)
  # Compute random effect standard deviations:
  sig.gam = 1/sqrt(tau.gam)
  sig.eps = 1/sqrt(tau.eps)
  
  # Compute Bayesian R2:
  var.pred = pow(sd(mu[]),2)
  var.resid = sig*sig
  R2 = var.pred/(var.pred + var.resid)
}