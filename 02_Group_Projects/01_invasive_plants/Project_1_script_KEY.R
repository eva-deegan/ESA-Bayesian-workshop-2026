#Project 1 script - SOLUTION: Poisson-snowfence example

rm(list=ls())
setwd(XXX)
library(rjags)
library(tidyverse)
library(MCMCvis)

#read in data
snow = read.csv("Snowfence_data.csv")

# look at data
str(snow)
# sample size
n = nrow(snow)
# define data list for JAGS model
datalist = list(y= snow$Number_invasives, x= snow$Footprint, n=n)

#Prior A is gamma(0.001, 0.001)
#prior B is gamma(10,10)
#Prior C is gamma(65.8,32.5)

# Code the Bayesian model using prior A --------------------------------------
text.modA <- "
model{
  for(i in 1:n){
    #Poisson likelihood for number of invasive plants counted for
    #snowfence i, which has footprint area x
    y[i] ~ dpois(mu[i])
    mu[i] = x[i]*theta
  }
  # Prior A: relatively non-informative prior for theta:
  theta ~ dgamma(0.001,0.001)
}
"
modA = textConnection(text.modA)


# Initialize jags model using jags.model for 3 chains:
jm_modA<-jags.model(modA, n.chains=3, data=datalist)
# update jags model with coda.samples for 5000 iterations
n.iter=5000
coda_modA = coda.samples(jm_modA,variable.names=c("theta"),
                         n.iter=n.iter)

# Check for convergence and mixing
MCMCtrace(coda_modA,iter=n.iter,file="mcmc_plots.pdf")

# Compute posterior statistics, check for convergence and effective sample size.
# Apply window function and start to remove potential burn-in:
outA = MCMCsummary(window(coda_modA,start=start(coda_modA)+100))


# Let's make the model code more general such that we can provide
# values for the gamma-prior hyperparameters with the datalist:
# Code the Bayesian model that can be used for priors A, B, and C ---------------------------------------------------------------------------------------
text.mod <- "
model{
  for(i in 1:n){
    #Poisson likelihood for number of invasive plants counted for
    #snowfence i, which has footprint area x
    y[i] ~ dpois(mu[i])
    mu[i] = x[i]*theta
  }
  # Prior A: relatively non-informative prior for theta:
  theta ~ dgamma(a,b)
}
"
# Run model with prior B; -----------------------------------------------------
#first update data to include values for hyperparameters
datalist$a = 10
datalist$b = 10
# Redefine model object:
modAll = textConnection(text.mod)
# Initialize model with prior B:
jm_modB= jags.model(modAll,n.chains=3, data=datalist)
# Update model with coda.samples:
coda_modB = coda.samples(jm_modB,variable.names=c("theta"),
                         n.iter=n.iter)
# Compute posterior statistics, check for convergence and effective sample size.
# Apply window function and start to remove potential burn-in:
outB = MCMCsummary(window(coda_modB,start=start(coda_modB)+100))

# combine posterior stats for models A and B:
sum_tab_all = rbind(outA, outB)
row.names(sum_tab_all)<-c("priorA", "priorB")

# Run model with prior C; ------------------------------------------------------
# Update dat:
datalist$a = 65.8
datalist$b = 32.5
# Refine model:
modAll = textConnection(text.mod)
# Initialize model based on prior C:
jm_modC = jags.model(modAll, n.chains=3, data=datalist)
# Update model with coda.samples:
coda_modC = coda.samples(jm_modC,variable.names=c("theta"),
                         n.iter=n.iter)
# Compute posterior statistics, check for convergence and effective sample size.
# Apply window function and start to remove potential burn-in:
outC = MCMCsummary(window(coda_modC,start=start(coda_modC)+100))

# combine posterior stats for all 3 models:
sum_tab_all = rbind(outA, outB, outC)
row.names(sum_tab_all) = c("priorA", "priorB", "priorC")
sum_tab_all= round(sum_tab_all, 4)


# Looking at posterior density distribution --------------------------------------------------------------------------------------------------------
#overlays the densities from each plot
all_p = cbind.data.frame(coda_modA[[1]][,1],coda_modB[[1]][,1], coda_modC[[1]][,1]) 
names(all_p) = c("With prior A", "With prior B", "With prior C")
library(reshape2)
x= melt(all_p)
str(x)
ggplot(x,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + theme_classic()




