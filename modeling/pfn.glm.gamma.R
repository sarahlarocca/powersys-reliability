# n := normalized
# s := subset (by number of nodes removed)
# r := reduced to significant variables

p.max <- 0.05
s <- 2

AC.s <- AC[pfn.full$n.fail == s]
#AC.s <- (AC.s - mean(AC.s))/sd(AC.s)
AC.s <- data.frame(AC.s)
AC.s <- as.matrix(AC.s)

pfn.ns <- pfn.n[pfn.full$n.fail == s,-max(dim(pfn.n)[2])]
pfn.ns <- data.frame(pfn.ns)
pfn.ns <- as.matrix(pfn.ns)

###############################################################################################
# Gamma GLM, all normalized covariates

######################################

# Fit initial model
model.pfn.glm.gamma <- glm(AC.s ~ pfn.ns, family = Gamma)

# Record p-values for initial model
model.pfn.glm.gamma.coef.p <- data.frame(summary(model.pfn.glm.gamma)[12])[,4]

######################################

# Reduce model to include only significant covariates

remove.ind <- c()					   # initialize list of indices for removal
j <- 0
pfn.nsr <- pfn.ns 	                                  # initialize reduced data object

while (sum(model.pfn.glm.gamma.coef.p > p.max) > 0){     # while there are covariates with p > p.max
  j <- j + 1
  remove.ind[j] <- which(model.pfn.glm.gamma.coef.p[-1] == max(model.pfn.glm.gamma.coef.p[-1]))
  pfn.nsr <- pfn.nsr[,-(remove.ind[j])]
  model.pfn.glm.gamma <- glm(AC.s ~ pfn.nsr, family = Gamma) 
  model.pfn.glm.gamma.coef.p <- data.frame(summary(model.pfn.glm.gamma)[12])[,4]
}

###############################################################################################3

summary(model.pfn.glm.gamma)