# Starter script, fill-in missing (XXX) code, and add additional code as needed

# Load libraries/modules
library(rjags)
load.module('dic')
library(MCMCvis)

#################
### Data prepping

# Read-in and prepare data
water.dat = read.csv("Vegetation_wateruse.csv")
N = nrow(water.dat)
# MAT and MAP
MAT = water.dat$MAT
MAP = XXX
# Since the model allows for the water use of different vegetation types 
# to respond differently to climate, and the intercept provides an overall
# “baseline” estimate of water use for 
# each vegetation type (at average climate conditions),
# we should standardize the climate covariates to allow for this 
# interpretation of the vegetation type-specific intercept.
# Standardized MAT and MAP
zMAT = scale(MAT,center=TRUE,scale=TRUE)
zMAP = XXX

# Take a look at the data distribution and how that differs from log(ET)
# Histogram and summary of water-use data
par(mfrow = c(1,2), mar = c(4, 2, 3, 2)) # bottom, left, top and right margins
hist(water.dat$Annual_ET, xlab = "Water-use (ET)",breaks=20, main = "Histogram of ET")
hist(log(water.dat$Annual_ET), xlab = "Log(ET)",breaks=20, main = "Histogram of log(ET)")
summary(water.dat$Annual_ET)
summary(log(water.dat$Annual_ET))

# Data list for JAGS model, including the "gridded" values for standardized
# MAP ("map") and MAT ("mat") used to computed predicted overall water use
dat = list(N = XXX, # N is the number of ET values
           Y = log(water.dat$Annual_ET), 
           MAT = as.vector(zMAT), 
           MAP = XXX,
           veg = water.dat$VegID, 
           # Nveg is the number of vegetation groups in the data
           Nveg = length(unique(water.dat$VegID))

#################
### Problem 3: Specify initials

# Simple linear regression without vegetation effects to help get initials;
# Uses Y = log(ET) and used standardized climate (from JAGS data list)
lm.out = lm(dat$Y ~ dat$MAT + dat$MAP + I(dat$MAP*dat$MAT))
lm.sum = summary(lm.out)

# Coefficient estimates from lm:
betas = lm.out$coefficients
# residual precision estimate:
tau = 1/(lm.sum$sigma^2)
# estimate for intercept variability (precision)
tau.b0 = 1/((2*lm.sum$coefficients[1,2])^2)
# estimate for coefficient variability (precision)
tau.b = 1/((2*lm.sum$coefficients[2:4,2])^2)

# Create list of relatively dispersed initials for root nodes for 3 chains:
inits = list(list(mu.b0 = betas[1], mu.b = betas[2:4], tau = tau, tau.b0 = tau.b0,
                  tau.b = tau.b),
             list(mu.b0 = betas[1]*runif(n=1,.5,2), mu.b = betas[2:4]*runif(n=3,min=.5,max=2), 
                  tau = tau*runif(n=1,0.5,2), tau.b0 = tau.b0*runif(n=1,.5,2),
                  tau.b = tau.b*runif(n=3,min=.5,max=2)),
             list(mu.b0 = betas[1]*runif(n=1,.5,2), mu.b = betas[2:4]*runif(n=3,min=.5,max=2), 
                  tau = tau*runif(n=1,0.5,2),tau.b0 = tau.b0*runif(n=1,.5,2),
                  tau.b = tau.b*runif(n=3,min=.5,max=2)))


#################
### Implement the model

# Initialize JAGS model with jags.model
jm = jags.model("Project_6_Model_STARTER_wateruse.R", data = dat, 
                inits = inits, n.chains = 3)

# Initial application of coda.samples and monitor quantities of interest
n.iter = 10000
coda = coda.samples(jm,
                    variable.names=c("deviance","b","b0","mu.b","mu.b0",
                                        "sig","sig.b","sig.b0"), 
                    n.iter = n.iter)

# Evaluate mixing, convergence, and effective sample sizes:
# Create history plots (thin by 5 for ease of plotting).
MCMCtrace(window(coda,thin=25,start=start(coda)+100),iter=n.iter,
          file = "wateruse_plots.pdf")

# Check Raftery diagnostics
raftery.diag(coda)

# Posterior summary stats (remove potential burn-in)
out = MCMCsummary(window(coda,start=1000))
# Check Rhat
max(out$Rhat)
out[out$Rhat>1.1,]
# Check n.eff
min(out$n.eff)
length(out[out$n.eff<3000,1])
length(out[,1])
max(out$n.eff)
out[out$n.eff>20000,]

# Rerun coda.samples with larger n.iter and monitor quantities of interest
n.iter = 100000
coda = coda.samples(jm,
                    variable.names=c(XXXX), 
                    n.iter = XXXX)

# Rerun coda.samples to monitor replicated data (Yrep)
n.iter = 5000
coda.rep = coda.samples(jm,
                        variable.names = c(XXXX),
                        n.iter = XXXX)


#################
### Assess model fit

# Re-compute posterior summary statistics, to get posterior stats for R2:
out = MCMCsummary(window(coda,start=1000))
out3 = round(out,digits = 3)

# Compute posterior summary statistics for replicated data:
out.rep = MCMCsummary(window(coda.rep,start=500))

# Plot of predicted vs observed log(ET), with 95% CI intervals, and
# Bayesian R2 overlaid
dev.off()
plot(dat$Y,out.rep$mean,xlim=c(1.5,8), ylim=c(1.5,8), xlab="Observed log(ET)",
     ylab = "Predicted log(ET)")
# 1:1 line
abline(a=0,b=1,col="red")
# CI intervals:
segments(x0=dat$Y,y0=out.rep$`2.5%`,x1=dat$Y,y1=out.rep$`97.5%`,col="gray")
points(dat$Y,out.rep$mean)
# Overlay Bayesian R2 (posterior mean and 95% CI):
text(x=3.2,y=7.7,paste0("Bayesian R2 = ", out3$mean[1]," [",out3$`2.5%`[1],", ",out3$`97.5%`[1],"]"))



#################
### Inference about water use

# Reminder of regression model:
# mu[i] = b0[veg[i]] + b[1,veg[i]]*MAT[i] + b[2,veg[i]]*MAP[i] +
# b[3,veg[i]]*MAT[i]*MAP[i]

# Caterpillar plot of intercepts (baseline water use rate)
dev.off()
MCMCplot(window(coda,start=1000), params = c("b0"), main = "b0 (intercept)",
         sz_labels = 1.1)

# Caterpillar plot of population-level coefficient estimates:
MCMCplot(window(coda,start=1000), params = c("mu.b"), 
         main = "Population-level (overall) climate effects",
         labels = c("MAT", "MAP", "MATxMAP"),
         sz_labels = 1.1)


# Multi-panel figure with caterpillar plots for the veg-type-specific
# climate effects (also included the veg-specific intercept)
# You might have to make your plot window larger
par(mfrow = c(2, 2), mar = c(2, 4, 4, 2))
MCMCplot(window(coda,start=1000), params = c("b0"), main = "b0 (intercept)",
         sz_labels = 1)
MCMCplot(window(coda,start=1000), params = c(paste0("b[",1,",",1:7,"]")),
         main = "MAT effect", sz_labels = 1, ISB=FALSE)
MCMCplot(window(coda,start=1000), params = c(paste0("b[",2,",",1:7,"]")),
         main = "MAP effect", sz_labels = 1, ISB=FALSE)
MCMCplot(window(coda,start=1000), params = c(paste0("b[",3,",",1:7,"]")),
         main = "MATxMAP effect", sz_labels = 1, ISB=FALSE)
dev.off()