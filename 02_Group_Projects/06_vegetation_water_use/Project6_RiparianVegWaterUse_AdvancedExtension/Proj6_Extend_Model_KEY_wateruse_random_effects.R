model{
  for(i in 1:N){
    # Likelihood for annual water use:
    Y[i] ~ dnorm(mu[i], tau)
    Yrep[i] ~ dnorm(mu[i], tau)
    # Regression model with coefficients that vary by vegetation type,
    # include main effects of MAP and MAT, their quadratic terms, and
    # their 2-way interaction:
    mu[i] = b0[veg[i]] + b[1,veg[i]]*MAT[i] + b[2,veg[i]]*MAP[i] +
      b[3,veg[i]]*pow(MAT[i],2) + b[4,veg[i]]*pow(MAP[i],2) +
      b[5,veg[i]]*MAT[i]*MAP[i] + eps[metID[i]] + gam[stdID[i]]
  }
  
  # Zero-centered hierarchical prior for the study effects with sum-to-zero
  # constraint:
  for(i in 2:Nstd){
    gam[i] ~ dnorm(0,tau.gam)
  }
  # Sum-to-zero for study effects:
  gam[1] = -sum(gam[2:Nstd])

  # Zero-centered hierarchical prior for the method effects with post-sweeping
  # to compute identifiable method random effects:
  for(m in 1:Nmet){
    eps[m] ~ dnorm(0,tau.eps)
    # 2b) Post-sweeping to compute identifiable method random effect:
    eps.star[m] = eps[m] - mean.eps
  }
  mean.eps = mean(eps[])
  
  # Conjugate, hierarchical priors for the vegetation-type-specific
  # coefficients:
  for(v in 1:Nveg){
    for(k in 1:5){
      # Climate effects:
      b[k,v] ~ dnorm(mu.b[k], tau.b[k])
    }
    # Intercept:
    b0[v] ~ dnorm(mu.b0, tau.b0)
    # 2c) Identifiable intercept:
    b0.star[v] = b0[v] + mean.eps
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
  mu.b0.star = mu.b0 + mean.eps
  
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