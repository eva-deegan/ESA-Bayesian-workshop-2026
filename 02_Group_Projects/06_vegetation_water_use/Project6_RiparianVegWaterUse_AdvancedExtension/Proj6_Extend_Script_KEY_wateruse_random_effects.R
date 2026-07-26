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
MAP = water.dat$MAP
# Standardized MAT and MAP
zMAT = scale(MAT,center=TRUE,scale=TRUE)
zMAP = scale(MAP,center=TRUE,scale=TRUE)

# Data list for JAGS model
dat = list(N = N, 
           Y = log(water.dat$Annual_ET), 
           MAT = as.vector(zMAT), 
           MAP = as.vector(zMAP),
           veg = water.dat$VegID, 
           Nveg = length(unique(water.dat$VegID)),
           metID = water.dat$MetID, 
           Nmet = length(unique(water.dat$MetID)),
           stdID = water.dat$StudyID,
           Nstd = length(unique(water.dat$StudyID)))

#################
### Specify initials

# Simple linear regression without vegetation effects to help get initials;
# Uses Y = log(ET) and used standardized climate (from JAGS data list)
lm.out = lm(dat$Y ~ dat$MAT + dat$MAP + I(dat$MAT^2) + 
              I(dat$MAP^2) + I(dat$MAP*dat$MAT))
lm.sum = summary(lm.out)

# Coefficient estimates from lm:
betas = lm.out$coefficients
# residual precision estimate:
tau = 1/(lm.sum$sigma^2)
# estimate for intercept variability (precision)
tau.b0 = 1/((2*lm.sum$coefficients[1,2])^2)
# estimate for coefficient variability (precision)
tau.b = 1/((2*lm.sum$coefficients[2:6,2])^2)
# rough estimate of the study random effect precision term:
study.means = aggregate(x = dat$Y, by = list(dat$stdID), FUN = mean)
tau.gam = 1/(var(study.means[,2]))
# rough estimate of the method random effect precision term:
method.means = aggregate(x = dat$Y, by = list(dat$metID), FUN = mean)
tau.eps = 1/(var(method.means[,2]))


# Create list of relatively dispersed initials for root nodes for 3 chains:
inits = list(list(mu.b0 = betas[1], mu.b = betas[2:6], tau = tau, tau.b0 = tau.b0,
                  tau.b = tau.b, tau.gam = tau.gam, tau.eps = tau.eps),
             list(mu.b0 = betas[1]*runif(n=1,.5,2), mu.b = betas[2:6]*runif(n=5,min=.5,max=2), 
                  tau = tau*runif(n=1,0.5,2), tau.b0 = tau.b0*runif(n=1,.5,2),
                  tau.b = tau.b*runif(n=5,min=.5,max=2),
                  tau.gam = tau.gam*runif(n=1,0.5,2), tau.eps = tau.eps*runif(n=1,0.5,2)),
             list(mu.b0 = betas[1]*runif(n=1,.5,2), mu.b = betas[2:6]*runif(n=5,min=.5,max=2), 
                  tau = tau*runif(n=1,0.5,2),tau.b0 = tau.b0*runif(n=1,.5,2),
                  tau.b = tau.b*runif(n=5,min=.5,max=2),
                  tau.gam = tau.gam*runif(n=1,0.5,2), tau.eps = tau.eps*runif(n=1,0.5,2)))


#################
### Implement the model

# Initialize JAGS model with jags.model
jm = jags.model("Proj6_Extend_Model_KEY_wateruse_random_effects.R", data = dat, 
                inits = inits, n.chains = 3)

# Initial application of coda.samples and monitor quantities of interest
n.iter = 10000
coda = coda.samples(jm,
                    variable.names=c("deviance","b","b0.star","mu.b","mu.b0.star",
                                        "sig","sig.b","sig.b0","R2",
                                        "sig.eps","sig.gam","eps.star","gam"), 
                    n.iter = n.iter)

# Evaluate mixing, convergence, and effective sample sizes:
# Create history plots (thin by 25 for ease of plotting).
MCMCtrace(window(coda,thin=25),iter=n.iter,file = "wateruse_plots.pdf")
MCMCtrace(window(coda,),iter=n.iter, params = c("deviance","sig.eps"))
MCMCtrace(window(coda,thin=25,start=start(coda)+100),
          iter=n.iter,file = "wateruse_plots_v2.pdf")

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
n.iter = 200000
coda = coda.samples(jm,variable.names=c("deviance","b","b0.star","mu.b","mu.b0.star",
                                        "sig","sig.b","sig.b0","R2",
                                        "sig.eps","sig.gam","eps.star","gam"), 
                    n.iter = n.iter)

# Rerun coda.samples to monitor replicated data (Yrep)
n.iter = 5000
coda.rep = coda.samples(jm,variable.names = c("Yrep"), n.iter = n.iter)


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
text(x=5,y=2,paste0("Bayesian R2 = ", out3$mean[1]," [",out3$`2.5%`[1],", ",out3$`97.5%`[1],"]"))


#################
### Inference about water use


# Caterpillar plot of method random effects
dev.off()
MCMCplot(window(coda,start=1000), params = c("eps.star"), main = "Method effects",
         sz_labels = 1,ISB=TRUE, rank = TRUE)

# Caterpillar plot of study random effects
MCMCplot(window(coda,start=1000), params = c("gam"),
         main = "Study effects", sz_labels = 1, ISB=TRUE, rank = TRUE)


# Caterpillar plot of vegetation-specific identifiable intercepts 
# (baseline water use rate)
MCMCplot(window(coda,start=1000), params = c("b0.star"), main = "b0 (intercept)",
         sz_labels = 1.1)

# Caterpillar plot of population-level coefficient estimates:
MCMCplot(window(coda,start=1000), params = c("mu.b"), 
         main = "Population-level (overall) climate effects",
         labels = c("MAT", "MAP", "MAT^2", "MAP^2", "MATxMAP"),
         sz_labels = 1.1)


# Multi-panel figure with caterpillar plots for the veg-type-specific
# climate effects (also included the veg-specific intercept)
par(mfrow = c(2, 3), mar = c(2, 4, 4, 2))
MCMCplot(window(coda,start=1000), params = c("b0.star"), main = "b0 (intercept)",
         sz_labels = 1)
MCMCplot(window(coda,start=1000), params = c(paste0("b[",1,",",1:7,"]")),
         main = "MAT effect", sz_labels = 1, ISB=FALSE)
MCMCplot(window(coda,start=1000), params = c(paste0("b[",2,",",1:7,"]")),
         main = "MAP effect", sz_labels = 1, ISB=FALSE)
MCMCplot(window(coda,start=1000), params = c(paste0("b[",3,",",1:7,"]")),
         main = "MAT^2 effect",sz_labels = 1, ISB=FALSE)
MCMCplot(window(coda,start=1000), params = c(paste0("b[",4,",",1:7,"]")),
         main = "MAP^2 effect",sz_labels = 1, ISB=FALSE)
MCMCplot(window(coda,start=1000), params = c(paste0("b[",5,",",1:7,"]")),
         main = "MATxMAP effect", sz_labels = 1, ISB=FALSE)
dev.off()
