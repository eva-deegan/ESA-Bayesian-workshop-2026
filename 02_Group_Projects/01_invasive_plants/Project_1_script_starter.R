#Project 1 script - Poisson-snowfence example, fill in XXX

rm(list=ls())
setwd(XXX)
library(rjags)
library(ggplot2)

#read in data
snow = read.csv("Snowfence_data.csv")

# look at data
XXX
# sample size
n = nrow(snow)
# define data list for JAGS model
datalist = list(y=XXX, x=XXX, n=XXX)

#Prior A is gamma(0.001, 0.001)
#prior B is gamma(10,10)
#Prior C is gamma(65.8,32.5)

# Code the Bayesian model using prior A ------------------------------------------------------------------------------------------
text.modA <- "
model{
  for(i in 1:n){
    #Poisson likelihood for number of invasive plants counted for
    #snowfence i, which has footprint area x
    y[i] ~ dpois(XXX)
    mu[i] <- XXX
  }
  # Prior A: relatively non-informative prior for theta:
  theta ~ dgamma(XXX,XXX)
}
"
modA = textConnection(text.modA)

n.adapt=500
jm_modA<-jags.model(modA, n.chains=1, data=datalist, n.adapt = n.adapt)
# update jags model with coda.samples for 5000 iterations
n.iter=XXX
coda_modA = coda.samples(jm_modA,variable.names=XXX,
                         n.iter=n.iter)
# Check for convergence and mixing
MCMCtrace(coda_modA,iter=n.iter,file="mcmc_plots.pdf")

# Compute posterior statistics, check for convergence and effective sample size.
# Apply window function and start to remove burn-in:
outA = MCMCsummary(window(coda_modA,start=start(coda_modA)+100))


# Let's make the model code more general such that we can provide
# values for the gamma-prior hyperparameters with the datalist:
# Code the Bayesian model that can be used for priors A, B, and C ---------------------------------------------------------------------------------------
text.mod <- "
model{
  for(i in 1:n){
    #Poisson likelihood for number of invasive plants counted for
    #snowfence i, which has footprint area x
    y[i] ~ dpois(XXX)
    mu[i] <- XXX
  }
  # Prior A: relatively non-informative prior for theta:
  theta ~ dgamma(a,b)
}
"
# Run model with prior B; --------------------------------------------------------------------------------------------------------
#first update data to include values for hyperparametes
#prior B is gamma(10,10)
datalist$a = XXX
datalist$b = XXX
modAll = textConnection(text.mod)
jm_modB<-jags.model(modAll,n.chains=3, data=datalist, n.adapt = n.adapt)
coda_modB = coda.samples(jm_modB,variable.names=XXX,
                         n.iter=n.iter)
# Compute posterior statistics, check for convergence and effective sample size.
# Apply window function and start to remove burn-in:
outB = MCMCsummary(window(coda_modB,start=start(coda_modB)+100))


sum_tab_all <-rbind(outA, outB)
row.names(sum_tab_all)<-c("priorA", "priorB")

# Run model with prior C; --------------------------------------------------------------------------------------------------------
# Prior C is gamma(65.8,32.5)
datalist$a = XXX
datalist$b = XXX
modAll = textConnection(text.mod)
jm_modC <-jags.model(modAll, n.chains=3, data=datalist, n.adapt = n.adapt)
coda_modC = coda.samples(jm_modC,variable.names=XXX,
                         n.iter=n.iter)
# Compute posterior statistics, check for convergence and effective sample size.
# Apply window function and start to remove burn-in:
outC = MCMCsummary(window(coda_modC,start=start(coda_modC)+100))


sum_tab_all <-rbind(outA, outB, outC)
row.names(sum_tab_all)<-c("priorA", "priorB", "priorC")

sum_tab_all= round(sum_tab_all, 4)

# simulate values of theta from each prior using JAGS: --------------------------------------------------------------------------------------------------------
# Code simple model for simulating from priors:
text.priors  = "
model{
  # Simple code for simulating from different gamma priors:
  theta_a ~ dgamma(0.001, 0.001)
  theta_b ~ dgamma(10,10)
  theta_c ~ dgamma(65.8,32.5)
}
"
modPriors = textConnection(text.priors)
jm_priors<-jags.model(modPriors, n.chains=3, n.adapt = n.adapt)
coda_priors = coda.samples(jm_priors,variable.names=c("theta_a", "theta_b", "theta_c"),
                           n.iter=n.iter)
#Prior summary statistics
priors_out = MCMCsummary(window(coda_priors,start=start(coda_priors)+100))


# Looking at posterior density distribution --------------------------------------------------------------------------------------------------------
#alternative code which overlays the 
#densities from each plot (need to install ggplot2 package)
#ggplot2 requries an input of a dataframe
all_p<-cbind.data.frame(coda_modA[[1]][,1],coda_modB[[1]][,1], coda_modC[[1]][,1]) 
names(all_p)<-c("With prior A", "With prior B", "With prior C")
library(reshape2)
x<-melt(all_p)
str(x)
ggplot(x,aes(x=value, fill=variable)) + geom_density(alpha=0.25)


# Plotting to see how the prior impacts the posterior distribution
# With prior A
all_A<-cbind.data.frame(coda_modA[[1]][,1],coda_priors[[1]][,1]) 
names(all_A)<-c("Posterior","Prior A")
x_A<-melt(all_A)
str(x_A)
ggplot(x_A, aes(x = value, fill = variable)) + 
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 0.1) + 
  coord_cartesian(xlim = c(0, 4), ylim = c(0, 100))

#With prior B
all_B<-cbind.data.frame(coda_modB[[1]][,1],coda_priors[[1]][,2]) 
names(all_B)<-c("Posterior", "Prior B")
x_B<-melt(all_B)
str(x_B)
ggplot(x_B,aes(x=value, fill=variable)) + geom_density(alpha=0.25)+ xlim(0,4)

#With prior C
all_C<-cbind.data.frame(coda_modC[[1]][,1],coda_priors[[1]][,3]) 
names(all_C)<-c("Posterior","Prior C")
x_C<-melt(all_C)
str(x_C)
ggplot(x_C,aes(x=value, fill=variable)) + geom_density(alpha=0.25)+ xlim(0,4)


