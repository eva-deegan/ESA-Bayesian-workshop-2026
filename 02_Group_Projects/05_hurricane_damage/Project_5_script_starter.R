# Load libraries
library(rjags)
load.module("dic")
library(MCMCvis)

##################################
# Project 4: Hurricane damage

# Implement jags.model and coda.samples.

# Read-in data
data = read.csv("hurricane_damage_data.csv")

# Create data list for JAGS
jags.data = list(Spp = data$Spp, Surv = data$Surv, D = as.matrix(data[,3:5]), 
                 k = rowSums(data[,3:5]), alpha = XX)

# Initialize model with jags.model().
jm = jags.model("model_hurricane.R", data = jags.data, n.chains = XX)

# Update model with coda.samples() and monitor quantities of interest
coda = coda.samples(jm, variable.names = c(XX), 
                    n.iter = XX)

# Evaluate burn-in, convergence, and mixing:
MCMCtrace(coda, iter=XX, filename = "Hurricane_plots.pdf")

# Obtain posterior estimates for quantities of interest
# and Rhat statistic.
out = MCMCsummary(coda)
# Round posterior statistics and only grab quantities of interest
out.table = round(out[-1,c(1,3,5,6)],digits=4)
out.table

# Caterpillar plot of pairwise differences in damage
# probabilities (surviving - dead trees)
MCMCplot(coda,params=XX,horiz=FALSE)
MCMCplot(coda,params=XX,horiz=FALSE, 
         ylab = "Diff in damage probs (surv-dead)",
         labels = c("Sp1,D1","Sp2,D1","Sp3,D1","Sp4,D1",
                    "Sp1,D2","Sp2,D2","Sp3,D2","Sp4,D2",
                    "Sp1,D3","Sp2,D3","Sp3,D3","Sp4,D3"),
         sz_labels = 0.9, sz_main_txt =1,sz_ax = 1.5, sz_tick_txt=1,
         sz_ax_txt=1)
