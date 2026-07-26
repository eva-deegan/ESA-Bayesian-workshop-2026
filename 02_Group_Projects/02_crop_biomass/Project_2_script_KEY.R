#Project 2 script - SOLUTION: Crop production example
rm(list=ls())
setwd("C:/Users/esd96/OneDrive - Northern Arizona University/professional development/ESA_2026/ESA-Bayesian-workshop-2026/02_Group_Problems/02_crop_biomass/")
library(rjags)
library(ggplot2)
library(MCMCvis)
# Read-in the 2 different biomass datasets:
v1 = read.csv("Lab_04_Biomass_data_v1.csv")
v2 = read.csv("Lab_04_Biomass_data_v2.csv")
str(v1)
head(v1)
str(v2)
head(v2)


####################### Balanced data ####################### 

# Write JAGS model code for the balanced data (data v1)
text.v1 = "
model{
  for(k in 1:2){
    for(i in 1:30){
      # Likelihood for biomass data:
      B[i, k] ~ dnorm(mu[k], tau)
    }
    # Prior for treatment-level means:
    mu[k] ~ dnorm(0, 0.001)
  }
  # Prior for common precision:
  tau ~ dgamma(0.01, 0.01)
  # Compute standard deviation:
  sig  =  1/sqrt(tau)
  # Compute difference between treatment means:
  diff  =  mu[1]-mu[2]
}
"

# Create data list
dat1  =  list(B=as.matrix(v1))

# Create initials:

#Find means of data columns
#apply(v1, 2, mean)
colMeans(v1)
#compute variance of biomass across all obs and both treatment levels
1 / var(unlist(v1[, 1:2]))
#get initials based on the sample summary stats above
inits1 =list(mu = c(100,160), tau = 0.0005)

# Run jags.model
mod.v1 = textConnection(text.v1)
# Initialize jags model
jm1 = jags.model(mod.v1, n.chains=3,data=dat1,inits=inits1)

# Set iterations and update model with coda.samples:
n.iter=5000
coda1 = coda.samples(jm1,variable.names=c("mu", "tau", "sig", "diff"),
                     n.iter=n.iter)

# Plot traces and density to explore convergence and mixing
MCMCtrace(coda1, 
          iter = n.iter, 
          file = "mcmc_plots.pdf")
# compute posterior statistics and convergence stats
out1 = MCMCsummary(window(coda1,start=start(coda1)+100))


####################### Unbalanced data ####################### 

# create data list based on v2 data (unbalance)
dat2  =  list("B"=as.vector(v2[,1]), "Trt"=as.vector(v2[,2]))
dat2 <- list(
  B   = dat2$B$B,
  Trt = dat2$Trt$Trt
)

# Write JAGS model code for the unbalanced data (data v2)
text.v2 = "
model{
  for(i in 1:50){
    # Likelihood for biomass data:
    B[i] ~ dnorm(mu[Trt[i]], tau)
  }
  for(k in 1:2){
    # Prior for treatment-level means:
    mu[k] ~ dnorm(0, 0.001)
  }
  # Prior for commaon precision:
  tau ~ dgamma(0.01, 0.01)
  # Compute standard deviation:
  sig = 1/sqrt(tau)
  # Compute difference between treatment means:
  diff =mu[1]-mu[2]
}
"

#same initials as above
inits1

# Initialize with jags.model:
mod.v2 = textConnection(text.v2)
jm2 = jags.model(mod.v2, n.chains=3, data=dat2,inits=inits1)

# Update with coda.samples: 
n.iter=5000
coda2 = coda.samples(jm2,variable.names=c("mu", "tau", "sig", "diff"),
                     n.iter=n.iter)

# Plot traces and density to explore convergence and mixing
MCMCtrace(coda2, 
          iter = n.iter, 
          file = "mcmc_plots.pdf")
# compute posterior statistics and convergence stats
out2 = MCMCsummary(window(coda2,start=start(coda2)+100))

out1
out2
