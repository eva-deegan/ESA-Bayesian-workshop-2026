# Load necessary libraries
library(rjags)
library(MCMCvis)
load.module('dic')

# ======================================================================
# Data prep 
# ======================================================================
# Load data:
tree_data <- read.csv("tree_drought_data.csv")
Y <- tree_data$Y
N <- tree_data$N
Nplots <- nrow(tree_data)

# Create data list for JAGS
dat_trees = list(Y = Y, N = N, Nplots = Nplots)


# ======================================================================
# Inits and initializing model
# ======================================================================
# Initials for 3 chains (dispersed starting values for theta between 0 and 1)
inits = list(list(theta = 0.1), 
                   list(theta = 0.5),
                   list(theta = 0.9))

# Initialize model
jm_trees = jags.model("tree_drought_mod.R", 
                      data = dat_trees, 
                      inits = inits, 
                      n.chains = 3)

# ======================================================================
# sampling posterior and diagnostics
# ======================================================================
niter = 5000
# Sample from the posterior. We'll monitor our parameter 'theta' and the 'deviance'
coda_trees = coda.samples(jm_trees, 
                          variable.names = c("theta", "deviance"), 
                          n.iter = niter)

# Evaluate convergence and look at trace plots
# This will save a PDF of the trace plots to your working directory
MCMCtrace(coda_trees, iter = niter, file = "TreeDrought_Model_plots.pdf")

# Generate a summary table of the posterior distributions
out_trees = MCMCsummary(coda_trees)
print(out_trees)
