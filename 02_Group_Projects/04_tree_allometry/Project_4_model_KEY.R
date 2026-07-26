model{
  for(i in 1:N){
    # Likelihood for height data with nonlinear mean model:
    H[i] ~ dnorm(mu[i],tau)
    mu[i] <- Hmax*(1-exp(-phi*muR[i]/Hmax))
    
    # Classical error-in-variables model for repeated observations of radius:
    for(r in 1:3){
      R[i,r] ~ dnorm(muR[i], tauR)
    }
    # Relatively non-informative prior for true, latent radii:
    muR[i] ~ dnorm(0,0.00001)
  }
  # Relatively non-informative prior for allometry parameters:
  Hmax ~ dnorm(0,0.00001)
  phi ~ dnorm(0,0.00001)
  
  # Relatively noninformative priors for standard deviations:
  sig ~ dunif(0,1000)
  sigR ~ dunif(0,1000)
  # Compute precisions:
  tau = pow(sig,-2)
  tauR = pow(sigR,-2)
}
