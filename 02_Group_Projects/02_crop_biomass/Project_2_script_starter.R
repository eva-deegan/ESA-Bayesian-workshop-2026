#Project 2 script - Crop production example, fill in XXX
rm(list=ls())
setwd(XXX)
library(rjags)
library(tidyverse)
library(MCMCvis)
# Problem 1: Read-in the 2 different biomass datasets:
v1<-read.csv("Lab_04_Biomass_data_v1.csv")
v2<-read.csv("Lab_04_Biomass_data_v2.csv")
str(v1)
head(v1)
str(v2)
head(v2)
#what is the difference between datasets v1 and v2?


####################### Balanced data ####################### 

# Write JAGS model code for the balanced data (data v1)
text.v1 = "
model{
  for(k in 1:2){
    for(i in 1:30){
      # Likelihood for biomass data:
      B[XXX] ~ dnorm(XXX, XXX)
    }
    # Prior for treatment-level means:
    mu[k] ~ XXX
  }
  # Prior for common precision:
  tau ~ XXX
  # Compute standard deviation:
  sig = XXX
  # Compute difference between treatment means:
  diff = XXX
}
"

#create data list
dat1 <- list(B=as.matrix(v1))

#create initials:

#Find means of data columns
#apply(v1, 2, mean)
colMeans(v1)
#compute variance of biomass across all obs and both treatment levels
1 / var(unlist(v1[, 1:2]))
#get initials based on the sample summary stats above
inits1<- list(mu = c(XXX,XXX), tau = XXX)

#run jags.model
mod.v1 = textConnection(text.v1)
#initialize jags model
jm1 <- jags.model(mod.v1, n.chains=3,data=dat1,inits=inits1)

#set iterations and update model with coda.samples:
n.iter=5000
coda1 = coda.samples(jm1,variable.names=XXX,
                     n.iter=n.iter)

#plot posterior densities
MCMCtrace(coda1, 
          iter = n.iter, 
          file = "mcmc_plots.pdf")
#find means and quantiles
out1 = MCMCsummary(window(coda1,start=start(coda1)+100))

####################### Unbalanced data ####################### 

# create data list based on v2 data (unbalance)
dat2 <- list("B"=as.vector(v2[,1]), "Trt"=as.vector(v2[,2]))
dat2 <- list(
  B   = dat2$B$B,
  Trt = dat2$Trt$Trt
)

# Write JAGS model code for the unbalanced data (data v2)
text.v2 = "
model{
  for(i in 1:50){
    # Likelihood for biomass data:
    B[XXX] ~ dnorm(XXX, XXX)
  }
  for(k in 1:2){
    # Prior for treatment-level means:
    mu[k] ~ XXX
  }
  # Prior for commaon precision:
  tau ~ XXX
  # Compute standard deviation:
  sig <- XXX
  # Compute difference between treatment means:
  diff <- XXX
}
"

#same initials as above
inits1

#Initialize with jags.model:
mod.v2 = textConnection(text.v2)
jm2 <- jags.model(mod.v2, n.chains=3, data=dat2,inits=inits1)

#Update with coda.samples: 
n.iter=5000
coda2 = coda.samples(jm2,variable.names=XXX,
                     n.iter=n.iter)

# 3e: plot traces and density
MCMCtrace(coda2, 
          iter = n.iter, 
          file = "mcmc_plots.pdf")
#find means and quantiles
out2 = MCMCsummary(window(coda2,start=start(coda2)+100))

out1
out2


