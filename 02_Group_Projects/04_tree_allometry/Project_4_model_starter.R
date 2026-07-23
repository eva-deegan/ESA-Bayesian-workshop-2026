model{
  for(i in 1:N){
    # Likelihood for height data with nonlinear mean model:
    H[i] ~ dnorm(XXX,tau)
    mu[i] = XXX
    
    # Classical error-in-variables model for repeated observations of radius:
    for(r in 1:3){
      R[i,r] ~ XXX
    }
    # Relatively non-informative prior for true, latent radii:
    muR[i] ~ XXX
  }
  # Relatively non-informative prior for allometry parameters:
  Hmax ~ dnorm(XXX)
  phi ~ XXX
  
  # Relatively noninformative uniform priors for standard deviations:
  sig ~ dunif(XXX)
  sigR ~ XXX
  # Compute precisions:
  tau = XXX
  tauR = XXX
}
