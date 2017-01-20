# n := normalized
# s := subset (by number of nodes removed)
# r := reduced to significant variables

p.max <- 0.05
s <- 5

AC.s <- AC[pfn.full$n.fail == s]

AC.s <- AC.s/load.max  		# relative load curtailed

AC.s <- data.frame(AC.s)
AC.s <- as.matrix(AC.s)

pfn.ns <- pfn.n[pfn.full$n.fail == s,-max(dim(pfn.n)[2])]
pfn.ns <- data.frame(pfn.ns)
pfn.ns <- as.matrix(pfn.ns)

###############################################################################################
# Beta regression, all normalized covariates

######################################

# Fit initial model
model.pfn.betareg <- betareg(AC.s ~ pfn.ns)

# Record p-values for initial model
model.pfn.betareg.coef.p <- coeftest(model.pfn.betareg)[,4]

######################################

# Reduce model to include only significant covariates

remove.ind <- c()					   # initialize list of indices for removal
j <- 0
pfn.nsr <- pfn.ns 	                                  # initialize reduced covariates object
AC.sr <- AC.s						   # initialize reduced response object

while (sum(model.pfn.betareg.coef.p > p.max) > 0){     # while there are covariates with p > p.max
  j <- j + 1
  remove.ind[j] <- which(model.pfn.betareg.coef.p[-1] == max(model.pfn.betareg.coef.p[-1]))
  pfn.nsr <- pfn.nsr[,-(remove.ind[j])]
  model.pfn.betareg <- betareg(AC.sr ~ pfn.nsr) 
  model.pfn.betareg.coef.p <- coeftest(model.pfn.betareg)[,4]
}

###############################################################################################3

summary(model.pfn.betareg)

