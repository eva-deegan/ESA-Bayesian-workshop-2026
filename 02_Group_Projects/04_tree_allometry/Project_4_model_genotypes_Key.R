model{
    for(i in 1:N){
    # Likelihood for height data with nonlinear mean model:
    H[i] ~ dnorm(mu[i],tau)
    mu[i] <- Hmax[genotype[i]]*(1-exp(-phi[genotype[i]]*muR[i]/Hmax[genotype[i]]))

    # Classical error-in-variables model for repeated observations of radius:
    for(r in 1:3){
      R[i,r] ~ dnorm(muR[i], tauR)
    }
    # Relatively non-informative prior for true, latent radii:
    muR[i] ~ dnorm(0,0.00001)
  }
  for(g in 1:Ng){
    # Hierarchical priors for genotype parameters:
    Hmax[g] ~ dnorm(mu.Hmax,tau.Hmax)
    phi[g] ~ dnorm(mu.phi,tau.phi)
  }
  
  # Relatively non-informative, conjugate priors for population mean parameters
  mu.Hmax ~ dnorm(0,0.00001)
  mu.phi ~ dnorm(0,0.00001)
  # Relatively noninformative uniform priors for standard deviations:
  sig ~ dunif(0,1000)
  sigR ~ dunif(0,1000)
  sig.Hmax ~ dunif(0,1000)
  sig.phi ~ dunif(0,1000)
  # Compute precisions:
  tau = pow(sig,-2)
  tauR = pow(sigR,-2)  
  tau.Hmax = pow(sig.Hmax,-2)  
  tau.phi = pow(sig.phi,-2)
}