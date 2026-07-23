library("rjags")
load.module("dic")
library("MCMCvis")

# Load data
trees = read.csv("treedim.csv")
head(trees)

# Plot data (height vs radius)
XXX

# Prepare data list for Jags
dat = list(N=nrow(trees), H=trees$H, R=as.matrix(trees[,1:3]))

# Define inits
inits = list(list(Hmax=XX, phi=XX,sig=XX, sigR=XX),
              list(Hmax=XX, phi=XX,sig=XX, sigR=XX),
              list(Hmax=XX, phi=XX,sig=XX, sigR=XX))

# initialize model
jm = jags.model("Project_4_model.R", data=dat, inits=inits,  n.chains = 3)
# simulate coda samples
niter = 5000
coda = coda.samples(jm, variable.names = c(XXX),
                     n.iter=niter)

# Check for convergence and mixing
MCMCtrace(coda,iter=niter,file="mcmc_plots.pdf")

# Compute posterior statistics, check for convergence and effective sample size.
# Apply window function and start to remove burn-in:
out = MCMCsummary(window(coda,start=start(coda)+100))

# Need more MCMC samples; update code again for larger number of iterations
niter = XXXX
coda = coda.samples(XXX)

# Re-compute posterior statistics:
out = MCMCsummary(window(coda,start=start(coda)+100))


#############################################################
# Extension: Add genotype effects and hierarchical priors:
#############################################################
