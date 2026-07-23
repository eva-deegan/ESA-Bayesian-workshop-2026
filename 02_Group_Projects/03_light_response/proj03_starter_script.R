# This R script runs a
# JAGS model that fits non-linear light response curves
# with normal priors

library(tidyverse)
library(rjags)
load.module('dic')
library(MCMCvis)

# Read in data
photo <- read_csv("02_Group_Projects/03_light_response/Photosynthesis_light_data.csv")

# Check structure
str(photo)

# Visualize curves
photo |> 
  ggplot()

#### Setting data ####

# Find species and canopy type IDs for each curve:
curve.spp <- photo |> 
  group_by(CurveID) |> 
  summarize(sp = unique(SppID))
curve.can <- photo |> 
  group_by(CurveID) |> 
  summarize(can = unique(CanopyID))

# Define the data list 
# Review the JAGS file to make sure dimensions match
datlist <- list(An = ,
                PAR = , 
                C = , 
                N = , 
                Ncurve = , 
                Nparam = ,
                Nspp = ,
                Ncan = ,
                Sp = ,
                CC = )

#### Model 1 ####
##### Initial conditions #####
# Use the empirical data to obtain estimates for root node parameters

# Population-level ball-park estimates of Rd, LUE, and Amax
Rd <- 
lm0_coef <- 
LUE <- 
Amax <- 

# Estimate of residual precision:
tau = 1/var(photo$An[photo$PAR>100 & photo$CurveID==10])
# Estimate of precision among log(Rd) values:
tau.log.Rd = 1/(var(log(photo$An[photo$PAR==0]),
                    na.rm = TRUE))
# Estimate of precision among log(Amax) values:
tau.log.Amax = 1/(var(log(photo$An[photo$PAR>1800])))
# Estimate of precision among log(LUE) values (though, this s.e. is for the 
# non-log scale):
tau.log.LUE = 1/(2*(lm0_coef[2,2])^2)


# Create list of inits for each chain. Simulate values given point estimates
# from glm, and using "inflated" std error as standard deviation in the rnorm
# function. Recall that theta needs to be a 3D array,
# organized the same as the JAGS model code:
log.theta = array(data = NA, dim = c(3, 2, 2)) 
tau.log = c(tau.log.LUE, tau.log.Amax, tau.log.Rd)
for(s in 1:2){
  for(c in 1:2){
    log.theta[,s,c] <- log(c(LUE, Amax, Rd))
  }
}

# Create a list of lists
# and vary the starting points randomly for each chain
initslist <- list(
  list(mu.log = log.theta*runif(n = 3*2*2, min = 0.5, max = 2),
       tau.log = tau.log*runif(n = 3, min = 0.5, max = 2),
       tau = tau*runif(1, min = 0.2, max = 2)),
  list(mu.log = log.theta*runif(n = 3*2*2, min = 0.5, max = 2),
       tau.log = tau.log*runif(n = 3, min = 0.5, max = 2),
       tau = tau*runif(1, min = 0.2, max = 2)),
  list(mu.log = log.theta*runif(n = 3*2*2, min = 0.5, max = 2),
       tau.log = tau.log*runif(n = 3, min = 0.5, max = 2),
       tau = tau*runif(1, min = 0.2, max = 2)))


##### Compile model and monitor posterior ####
# Compile model
jm1 <- jags.model()

# Set up posterior monitoring
niter <- 20000
params1 <- c()

# Sample the posterior 
coda1 <- coda.samples()

##### Summarize posterior and evaluate diagnostics #####
# Evaluate convergence and get posterior stats
# Can remove potential burn-in as needed and/or thin
MCMCtrace(window(coda1, thin = 10),
          excl = "An.rep", 
          iter = niter, 
          file = "02_Group_Projects/03_light_response/plots_mod1.pdf")

# Compute posterior summary statistics for parameters of interest and replicated data
out1 <- MCMCsummary(window(coda1, thin = 10), 
                    excl = "An.rep")
out.rep1 <- MCMCsummary(window(coda1, thin = 10),
                        params = c("An.rep"))


# View convergence diagnostics
max(out1[,"Rhat"])
sum(out1[,"Rhat"]>1.1)
min(out1[,"n.eff"])
sum(out1[,"n.eff"]<3000)


##### Visualize output #####
# Caterpillar plots for parameters of interest
# Curve level
labs <- c(paste0("curve ", 1:datlist$Ncurve))
MCMCplot(window(coda1, thin = 10),
         params = c(paste0("theta[1,", 1:21, "]")),
         ISB = FALSE, main = "Amax", labels = labs)
MCMCplot(XXX,
         main = "LUE", labels = labs)
MCMCplot(XXX,
         main = "Rd", labels = labs)

# Species/canopy level
labs <- c("maple, gap","maple, shade", "oak, gap", "oak, shade")
MCMCplot(window(coda1, thin = 10),
         params = c("mu.theta[1,1,1]", "mu.theta[1,1,2]", 
                    "mu.theta[1,2,1]", "mu.theta[1,2,2]"),
         ISB = FALSE, main = "Amax", labels = labs)
MCMCplot(XXX,
         main = "LUE", labels = labs)
MCMCplot(XXX,
         main = "Rd", labels = labs)

# Contrasts
labs <- c("gap-shade, maple","gap-shade, oak", 
          "maple-oak, gap", "maple-oak, shade")
MCMCplot(window(coda1, thin = 10),
         params = c("mu.diff.can[1,1]", "mu.diff.can[1,2]", 
                    "mu.diff.spp[1,1]", "mu.diff.spp[1,2]"), 
         ISB = FALSE, main = "Amax", labels = labs)
MCMCplot(XXX,
         main = "LUE", labels = labs)
MCMCplot(XXX,
         main = "Rd", labels = labs)

##### Evaluate model fit #####
# Add posterior stats of An.rep to dataframe

# Visualize observed vs predicted
photo |> 
  ggplot(aes(x = An,
             y = An.rep_1)) +
  geom_errorbar(aes(ymin = An.lower_1,
                    ymax = An.upper_1)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  stat_smooth(method = "lm", se = FALSE)

# get R2 from classical regression of obs vs pred:
lm1_sum <- lm(An.rep_1 ~ An, data = photo) |> 
  summary()
lm1_sum$adj.r.squared


# Bayesian R2
out1["R2",]

