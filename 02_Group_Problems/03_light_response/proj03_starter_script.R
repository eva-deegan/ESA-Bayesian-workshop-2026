library(tidyverse)
library(rjags)
load.module('dic')
library(MCMCvis)

# Read in data
photo <- read_csv("02_Group_Problems/03_light_response/Photosynthesis_light_data.csv")

# Check structure
str(photo)

# Visualize curves
photo |> 
  ggplot(aes(x = PAR, 
             y = An,
             color = as.factor(CurveID))) +
  geom_point() +
  geom_line() +
  facet_grid(rows = vars(CanopyID),
             cols = vars(SppID))

#### Setting data needed by model ####

# Find species and canopy type IDs for each curve:
curve.spp = photo |> 
  group_by(CurveID) |> 
  summarize(sp = unique(SppID))
curve.can = photo |> 
  group_by(CurveID) |> 
  summarize(can = unique(CanopyID))

# Define data list for JAGS model
dat <- list(An = ,
            PAR =, 
            C =, 
            N =, 
            Ncurve =, 
            Nparm =,
            Nspp =,
            Ncan =,
            Sp =,
            CC = )


#### Initial conditions ####

# Population-level ball-park estimates of Rd, LUE, and Amax
Rd <- 
lm1 <- lm(XX ~ XX)
LUE <- 
Amax <- 

# Estimate of residual precision:
tau <- 
# Estimate of precision among log(Rd) values:
tau.log.Rd <- 
# Estimate of precision among log(Amax) values:
tau.log.Amax <- 
# Estimate of precision among log(LUE) values 
# (though, this s.e. is for the non-log scale):
tau.log.LUE <- 


# Create list of inits for each chain. Simulate values given point estimates
# from glm, and using "inflated" std error as standard deviation in the rnorm
# function. Recall that theta needs to be in matrix format, organized the 
# same as the JAGS model code:
log.theta = array(data=NA,dim=c(3,2,2))
tau.log = c(tau.log.LUE, tau.log.Amax, tau.log.Rd)
for(s in 1:2){
  for(c in 1:2){
    log.theta[,s,c] = log(c(LUE, Amax, Rd))
  }
}
inits1 = list(list(mu.log=log.theta*runif(n=3*2*2,min=0.5,max=2),
                   tau.log=tau.log*runif(n=3,min=0.5,max=2),
                   tau=tau*runif(1,min=0.2,max=2)),
              list(mu.log=log.theta*runif(n=3*2*2,min=0.5,max=2),
                   tau.log=tau.log*runif(n=3,min=0.5,max=2),
                   tau=tau*runif(1,min=0.2,max=2)),
              list(mu.log=log.theta*runif(n=3*2*2,min=0.5,max=2),
                   tau.log=tau.log*runif(n=3,min=0.5,max=2),
                   tau=tau*runif(1,min=0.2,max=2)))
              

### Run model 1

jm1 <- jags.model(XXX)
niter <- 20000
params <- c()
coda1 <- coda.samples(XXX)

# Evaluate convergence and get posterior stats (remove potential burn-in)
MCMCtrace(window(coda1,thin=10),excl = "An.rep", iter=niter,file="Plots_mod1.pdf")

# Compute posterior summary statistics for parameters of interest and replicated data
out1 = MCMCsummary(window(coda1,start=start(coda1)+100), excl = "An.rep")
out.rep1 = MCMCsummary(window(coda1,start=start(coda1)+100),params=c("An.rep"))


# Caterpillar plots for parameters of interest
labs = c(paste0("curve ",1:dat$Ncurve),"maple, gap","maple, shade", "oak, gap", "oak, shade")
MCMCplot(window(coda1,start=start(coda1)+100),
         params=c(paste0("theta[1,",1:21,"]"),"mu.theta[1,1,1]","mu.theta[1,1,2]","mu.theta[1,2,1]","mu.theta[1,2,2]"),ISB=FALSE,
         main="Amax", labels = labs)
MCMCplot(XXX,
         main="LUE", labels = labs)
MCMCplot(XXX,
         main="LUE", labels = labs)



# Observed vs predicted
XXX

# get R2 from classical regression of obs vs pred:
XXX

# Bayesian R2
out1["R2",]




### Run model 2

# Create list of inits for each chain. Simulate values given point estimates
# from glm, and using "inflated" std error as standard deviation in the rnorm
# function. Recall that Beta needs to be in matrix format, organized the 
# same as the JAGS model code:

# Estimate of precision among log(Rd) values:
sig.Rd = 
# Estimate of precision among log(Amax) values:
sig.Amax = 
# Estimate of precision among log(LUE) values (though, this s.e. is for the 
# non-log scale):
sig.LUE = 
E = array(data=NA,dim=c(3,2,2))
S = c(sig.LUE, sig.Amax, sig.Rd)
for(s in 1:2){
  for(c in 1:2){
    E[,s,c] = c(LUE, Amax, Rd)
  }
}

inits2 = list(list(E=E*runif(n=3*2*2,min=0.5,max=2),S=S*runif(n=3,min=0.5,max=2),tau=tau*runif(1,min=0.2,max=2)),
              list(E=E*runif(n=3*2*2,min=0.5,max=2),S=S*runif(n=3,min=0.5,max=2),tau=tau*runif(1,min=0.2,max=2)),
              list(E=E*runif(n=3*2*2,min=0.5,max=2),S=S*runif(n=3,min=0.5,max=2),tau=tau*runif(1,min=0.2,max=2)))


jm2 = jags.model(XXX)
coda2 = coda.samples(XXXX)

# Evaluate convergence and get posterior stats (remove potential burn-in)
MCMCtrace(XXX)

# Compute posterior summary statistics for parameters of interest and replicated data
out2 = XXX
out.rep2 = XXX



# Caterpillar plots for parameters of interest
MCMCplot(window(coda2,start=start(coda2)+1),params=c(paste0("theta[1,",1:21,"]"),"E[1,1,1]","E[1,1,2]","E[1,2,1]","E[1,2,2]"),
         ISB=FALSE, main = "Amax (hierarchical gamma)", labels = labs)
MCMCplot(XXX, main = "LUE (hierarchical gamma)", labels = labs)
MCMCplot(XXX, main = "Rd (hierarchical gamma)", labels = labs)



# Observed vs predicted
XXX

# get R2 from classical regression of obs vs pred:
XXX

# Bayesian R2
out2["R2",]
