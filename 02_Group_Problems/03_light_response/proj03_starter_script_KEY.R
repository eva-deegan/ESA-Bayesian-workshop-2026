# This R script runs Model 1 and Model 2
# JAGS scripts that fit non-linear light response curves
# with different sets of priors

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

#### Setting data needed by both models ####

# Find species and canopy type IDs for each curve:
curve.spp <- photo |> 
  group_by(CurveID) |> 
  summarize(sp = unique(SppID))
curve.can <- photo |> 
  group_by(CurveID) |> 
  summarize(can = unique(CanopyID))

# Define the data list 
# Review the JAGS file to make sure dimensions match
datlist <- list(An = photo$An,
                PAR = photo$PAR, 
                C = photo$CurveID, 
                N = nrow(photo), 
                Ncurve = length(unique(photo$CurveID)), 
                Nparam = 3,
                Nspp = 2,
                Ncan = 2,
                Sp = curve.spp$sp,
                CC = curve.can$can)

#### Model 1 ####
##### Initial conditions for Model 1 #####
# Use the empirical data to obtain estimates for root node parameters

# Population-level ball-park estimates of Rd, LUE, and Amax
Rd <- photo |> 
  filter(PAR == 0) |> 
  pull(An) |> 
  mean() |> 
  abs()
lm0_coef <- photo |> 
  filter(PAR <= 300) |> 
  lm(An ~ PAR, data = _) |> 
  summary() |> 
  coef()
LUE <- lm0_coef[2,1]
Amax <- photo |> 
  filter(PAR > 1800) |> 
  pull(An) |> 
  mean()

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


##### Run Model 1 ####
# Compile model
jm1 <- jags.model("02_Group_Problems/03_light_response/proj03_mod1.JAGS",
                  data = datlist,
                  inits = initslist,
                  n.chains = 3)

# Set up posterior monitoring
niter <- 20000
params1 <- c("deviance", "theta", "mu.theta","sig.log","R2",
            "sig","An.rep","mu.diff.spp","mu.diff.can")

# Sample the posterior 
coda1 <- coda.samples(jm1, 
                      variable.names = params1,
                      n.iter = niter)

##### Model 1 diagnostics and output #####
# Evaluate convergence and get posterior stats (remove potential burn-in as needed)
MCMCtrace(window(coda1, thin = 10),
          excl = "An.rep", 
          iter = niter, 
          file = "02_Group_Problems/03_light_response/plots_mod1.pdf")

# Compute posterior summary statistics for parameters of interest and replicated data
out1 <- MCMCsummary(window(coda1, 
                          start = start(coda1) + 100), 
                    excl = "An.rep")
out.rep1 <- MCMCsummary(window(coda1, start = start(coda1) + 100),
                        params = c("An.rep"))


# View convergence diagnostics
max(out1[,"Rhat"])
sum(out1[,"Rhat"]>1.1)
min(out1[,"n.eff"])
sum(out1[,"n.eff"]<3000)

# Caterpillar plots for parameters of interest
labs <- c(paste0("curve ", 1:dat$Ncurve), 
          "maple, gap","maple, shade", "oak, gap", "oak, shade")
MCMCplot(window(coda1, start = start(coda1) + 100),
         params = c(paste0("theta[1,", 1:21, "]"),
                    "mu.theta[1,1,1]", "mu.theta[1,1,2]", "mu.theta[1,2,1]", 
                    "mu.theta[1,2,2]"), 
         ISB = FALSE, main = "Amax", labels = labs)
MCMCplot(XXX,
         main = "LUE", labels = labs)
MCMCplot(XXX,
         main = "Rd", labels = labs)


##### Model 1 fit #####
# Add posterior stats of An.rep to dataframe
photo$An.rep_1 <- out.rep1$mean
photo$An.lower_1 <- out.rep1$`2.5%`
photo$An.upper_1 <- out.rep1$`97.5%`

# Visualize observed vs predicted
photo |> 
  ggplot(aes(x = An,
             y = An.rep_1)) +
  geom_errorbar(aes(ymin = An.lower_1,
                    ymax = An.upper_1)) +
  geom_point()

# get R2 from classical regression of obs vs pred:
lm1_sum <- lm(An.rep_1 ~ An, data = photo) |> 
  summary()
lm1_sum$adj.r.squared


# Bayesian R2
out1["R2",]



#### Model 2 ####
##### Initial conditions for Model 2 #####

# Create list of inits for each chain. Simulate values given point estimates
# from glm, and using "inflated" std error as standard deviation in the rnorm
# function. Recall that Beta needs to be in matrix format, organized the 
# same as the JAGS model code:

# Estimate of precision among log(Rd) values:
sig.Rd = sqrt(var((photo$An[photo$PAR==0])))
# Estimate of precision among log(Amax) values:
sig.Amax = sqrt(var((photo$An[photo$PAR>1800])))
# Estimate of precision among log(LUE) values (though, this s.e. is for the 
# non-log scale):
sig.LUE = 2*(lm0_coef[2,2])
E <- array(data = NA, dim = c(3,2,2))
S <- c(sig.LUE, sig.Amax, sig.Rd)
for(s in 1:2){
  for(c in 1:2){
    E[,s,c] = c(LUE, Amax, Rd)
  }
}

# Create a list of lists
# and vary the starting points randomly for each chain
initslist2 <- list(
  list(E = E*runif(n = 3*2*2, min = 0.5, max = 2),
       S = S*runif(n = 3, min = 0.5, max = 2),
       tau = tau*runif(1, min = 0.2, max = 2)),
  list(E = E*runif(n = 3*2*2, min = 0.5, max = 2),
       S = S*runif(n = 3, min = 0.5, max = 2),
       tau = tau*runif(1, min = 0.2, max = 2)),
  list(E = E*runif(n = 3*2*2, min = 0.5, max = 2),
       S = S*runif(n = 3, min = 0.5, max = 2),
       tau = tau*runif(1, min = 0.2, max = 2)))

##### Run Model 2 #####
# Compile model
jm2 <- jags.model("02_Group_Problems/03_light_response/proj03_mod2.JAGS",
                 data = datlist,
                 inits = initslist2,
                 n.chains = 3)

# Set up posterior monitoring
niter <- 20000
params2 <- c("deviance", "theta", "E","S","sig","An.rep","R2",
            "mu.diff.spp","mu.diff.can")

# Sample the posterior 
coda2 <- coda.samples(jm2, 
                      variable.names = params2,
                      n.iter = niter)

##### Model 2 diagnostics and output #####
# Evaluate convergence and get posterior stats (remove potential burn-in)
MCMCtrace(window(coda2,thin=5),
          excl = "An.rep",
          iter = niter, type = "trace", 
          file = "02_Group_Problems/03_light_response/plots_mod2.pdf")

# Compute posterior summary statistics for parameters of interest and replicated data
out2 <- MCMCsummary(window(coda2, 
                           start = start(coda1) + 100), 
                    excl = "An.rep")
out.rep2 <- MCMCsummary(window(coda2, start = start(coda1) + 100),
                        params = c("An.rep"))

# Caterpillar plots for parameters of interest
MCMCplot(window(coda2,
                start = start(coda2)+1),
         params=c(paste0("theta[1,",1:21,"]"),"E[1,1,1]","E[1,1,2]","E[1,2,1]","E[1,2,2]"),
         ISB = FALSE, main = "Amax (hierarchical gamma)", 
         labels = labs)
MCMCplot(XXX, main = "LUE (hierarchical gamma)", labels = labs)
MCMCplot(XXX, main = "Rd (hierarchical gamma)", labels = labs)


##### Model 2 fit #####
# Add posterior stats of An.rep to dataframe
photo$An.rep_2 <- out.rep2$mean
photo$An.lower_2 <- out.rep2$`2.5%`
photo$An.upper_2 <- out.rep2$`97.5%`

# Visualize observed vs predicted
photo |> 
  ggplot(aes(x = An,
             y = An.rep_2)) +
  geom_errorbar(aes(ymin = An.lower_2,
                    ymax = An.upper_2)) +
  geom_point()

# get R2 from classical regression of obs vs pred:
lm2_sum <- lm(An.rep_2 ~ An, data = photo) |> 
  summary()
lm2_sum$adj.r.squared


# Bayesian R2
out2["R2",]
