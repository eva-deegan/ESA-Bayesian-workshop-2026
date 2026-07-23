library("rjags")
load.module("dic")
library("MCMCvis")

# Load data
trees = read.csv("treedim.csv")
head(trees)

# Plot data (height vs radius)
plot(trees$R1,trees$H,xlab="Radius",ylab="Height",pch=20)
points(trees$R2,trees$H, pch=20)
points(trees$R3,trees$H, pch=20)


# Prepare data list for Jags
dat = list(N=nrow(trees), H=trees$H, R=as.matrix(trees[,1:3]))

# Define inits
inits = list(list(Hmax=30, phi=1,sig=1, sigR=2.5),
              list(Hmax=20, phi=2,sig=0.5, sigR=1),
              list(Hmax=50, phi=5,sig=10,sigR=1.5))

# initialize model
jm = jags.model("Project_4_model.R", data=dat, inits=inits,  n.chains = 3)
# simulate coda samples
niter = 5000
coda = coda.samples(jm, variable.names = c("deviance", "Hmax", "phi", "sig","sigR"),
                     n.iter=niter)

# Check for convergence and mixing
MCMCtrace(coda,iter=niter,file="mcmc_plots.pdf")

# Compute posterior statistics, check for convergence and effective sample size.
# Apply window function and start to remove burn-in:
out = MCMCsummary(window(coda,start=start(coda)+100))

# Need more MCMC samples; update code again for larger number of iterations
niter = 15000
coda = coda.samples(jm, variable.names = c("deviance", "Hmax", "phi", "sig","sigR"),
                    n.iter=niter)

# Re-compute posterior statistics:
out = MCMCsummary(window(coda,start=start(coda)+100))


#############################################################
# Extension: Add genotype effects and hierarchical priors:
#############################################################

# Prepare data list for Jags
datG = list(N=nrow(trees), H=trees$H, R=as.matrix(trees[,1:3]), 
            Ng=length(unique(trees$genotype)), genotype=trees$genotype)
# inits
initsG = list(list(mu.Hmax=30, mu.phi=1,sig=1, sigR=0.5, sig.Hmax=1, sig.phi=0.5),
               list(mu.Hmax=20, mu.phi=2,sig=2, sigR=1, sig.Hmax=2, sig.phi=1),
               list(mu.Hmax=50, mu.phi=5,sig=4,sigR=3, sig.Hmax=5, sig.phi=2))

# initialize model
jmG = jags.model("Project_4_model_genotypes.R", data=datG, 
                 inits=initsG,  n.chains = 3)

# simulate coda samples
niter = 15000
codaG = coda.samples(jmG, variable.names = c("deviance", "Hmax", "phi", "mu.Hmax",
                                             "mu.phi","sig","sigR","sig.Hmax",
                                             "sig.phi"),
                    n.iter=niter)

# Check for convergence
MCMCtrace(codaG,iter=niter,file="mcmc_plots_G.pdf")

# Compute posterior statistics, check for convergence and effective sample size.
# Apply window function and start to remove burn-in:
outG = MCMCsummary(window(codaG,start=start(codaG)+100))

# Caterpillar plots to compare genotype specific parameters:
MCMCplot(codaG,params=c("Hmax","mu.Hmax"))
MCMCplot(codaG,params=c("phi","mu.phi"))
MCMCplot(codaG,params=c("sig","sig.Hmax","sigR","sig.phi"))

