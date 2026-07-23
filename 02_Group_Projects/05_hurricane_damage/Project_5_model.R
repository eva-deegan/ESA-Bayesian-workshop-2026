## Project 4. JAGS model code for the hurricane damage problem.

model{
  # Likelihood, loop through all observations (8 total observed vectors: 
  # 8 = 4 species x 2 survival classes.
  # Surv = 1 for trees that died from the hurricane and
  # Surv = 2 for trees that survived
  for(i in 1:8){
    # Multinomial likelihood for vector (D, length 3) of number of trees in 
    # each of 3 possible damage categories. The damage probability vector
    # varies by species ID (Spp) and survival class (Surv); k is the total
    # number of trees counted for each Spp-Surv category.
    D[i,1:3] ~ dmulti(p[Spp[i], Surv[i], 1:3],k[i])
  }
  
  # Conjugate Dirichlet prior for the damage probability vectors. Loop through
  # species (sp) and survival class (su). Provide values of the hyperparameter,
  # alpha, in the data list.
  for(sp in 1:4){
    for(su in 1:2){
      p[sp,su,1:3] ~ ddirch(alpha[1:3])
    }
    # Compute pairwise difference in damage probabilities between surviving
    # and dying trees for each species and each damage category:
    for(d in 1:3){
      # Survived - died damage probabilities:
      p.diff[sp,d] = p[sp,2,d] - p[sp,1,d]
    }
  }
}