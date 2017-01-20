################################################################################
# NOTATION KEY

# pfn := power flow, node failures
# pfe := power flow, edge failures

# n := normalized or rescaled
# s := subset
# r := reduced to significant variables

################################################################################

modelPowerflow <- function(pf.all,AC.all,covariates,load.max,p.max,s,num.holdouts,holdout.indices){

################################################################################

AC.n <- AC.all/load.max					# relative load curtailed

########################################

# Normalize covariates

attach(pf.all)

numfail.n <- (numfail - mean(numfail))/sd(numfail)
LCSG.n <- (LCSG - mean(LCSG))/sd(LCSG)
D.n <- (D - mean(D))/sd(D)
E.n <- (E - mean(E))/sd(E)
EN.n <- (EN - mean(EN))/sd(EN)
ENE.n <- (ENE - mean(ENE))/sd(ENE)
CL.n <- (CL - mean(CL))/sd(CL)
PCL.n <- (PCL - mean(PCL))/sd(PCL)
PNS.n <- (PNS - mean(PNS))/sd(PNS)
DC.n <- (DC - mean(DC))/sd(DC)

detach(pf.all)

pf.n <- cbind(numfail.n,LCSG.n, D.n, E.n, EN.n, ENE.n, CL.n, PCL.n, PNS.n, DC.n)
pf.n <- data.frame(pf.n)
names(pf.n) <- c('numfail','LCSG','D','E','EN','ENE','CL','PCL','PNS','DC')

pf.n <- data.frame(pf.n)

attach(pf.n)

########################################

# Choose covariates to use

pf.n.keep <- c()

j <- 1

for (i in 1:length(names(pf.n))){
  if ((sum(names(pf.n)[i] == covariates)) > 0){
    pf.n.keep[j] <- i
    j <- j + 1
  }
}

tmp <- c()
for (i in 1:length(pf.n.keep)){
  tmp[i] <- pf.n[pf.n.keep[i]]
}
pf.n <- tmp
pf.n <- data.frame(pf.n)
names(pf.n) <- covariates

########################################

# Subset data by number of elements removed

if (s == 0){

AC.ns <- AC.n
AC.ns <- data.frame(AC.ns)
AC.ns <- as.matrix(AC.ns)

pf.ns <- pf.n
pf.ns <- data.frame(pf.ns)
pf.ns <- as.matrix(pf.ns)

}
else {

AC.ns <- AC.n[pf.all$numfail == s]
AC.ns <- data.frame(AC.ns)
AC.ns <- as.matrix(AC.ns)

pf.ns <- pf.n[pf.all$numfail == s,]
pf.ns <- data.frame(pf.ns)
pf.ns <- as.matrix(pf.ns)

}

########################################

################################################################################

# Linear model

######################################

# Fit initial model
model.linear <- lm(AC.ns ~ pf.ns)

# Record p-values for initial model
model.linear.coef.p <- coeftest(model.linear)[,4]

######################################

# Reduce model to include only significant covariates

remove.ind <- c()					   # initialize list of indices for removal
j <- 0
pf.nsr <- pf.ns 	                                  # initialize reduced covariates object

while (sum(model.linear.coef.p > p.max) > 0){            # while there are covariates with p > p.max
  j <- j + 1
  remove.ind[j] <- which(model.linear.coef.p[-1] == max(model.linear.coef.p[-1]))
  pf.nsr <- pf.nsr[,-(remove.ind[j])]
  model.linear <- lm(AC.ns ~ pf.nsr)
  model.linear.coef.p <- coeftest(model.linear)[,4]
}

################################################################################

# Holdouts

######################################

# Data to predict from
data <- pf.ns

# Data to predict
actual <- AC.ns

# Initialize error vectors
MSE.linear <- matrix(0,0,0)
MAE.linear <- matrix(0,0,0)

######################################

# Holdouts

for(i in 1:num.holdouts){

# Generate random string
y <- holdout.indices[i,]

# Create holdout sample for this iteration
tmp.data <- cbind(data,y)
tmp.actual <- cbind(actual,y)

holdout.data <- subset(tmp.data,y==1)[,1:ncol(data)]
holdout.actual <- subset(tmp.actual,y==1)[,1]
leftover.data <- subset(tmp.data,y==0)[,1:ncol(data)]
leftover.data <- as.matrix(leftover.data)
leftover.actual <- subset(tmp.actual,y==0)[,1]

# Fit Model to leftover data

model.linear.holdout <- lm(leftover.actual ~ leftover.data)     	# fit initial model
model.linear.holdout.p <- coeftest(model.linear.holdout)[,4]		# p-values for initial model

remove.ind <- c()
j <- 0
leftover.data.r <- leftover.data

while (sum(model.linear.holdout.p > 0.05) > 0){
# while there are covariates with p > 0.05
  j <- j + 1
  remove.ind[j] <- which(model.linear.holdout.p == max(model.linear.holdout.p))
  leftover.data.r <- leftover.data.r[,-(remove.ind[j]-1)]
  model.linear.holdout <- lm(leftover.actual ~ leftover.data.r)    
  model.linear.holdout.p <- coeftest(model.linear.holdout)[,4]
}

# Reduce holdout data

holdout.data.r <- holdout.data
for (m in 1:j){
  if (!is.null(remove.ind)){
    holdout.data.r <- holdout.data.r[,-(remove.ind[m]-1)]
  }    
}

# Calculate fitted Y

fitted.value <- c()
for (k in 1:nrow(holdout.data.r)){
  fitted.value[k] <- model.linear.holdout$coefficients[1]
  for (l in 1:ncol(holdout.data.r)){
    fitted.value[k] <- fitted.value[k] + model.linear.holdout$coefficients[l+1]*holdout.data.r[k,l]
  }
}


#Calculate mean error for this holdout sample
MSE.linear[i] <- mean((holdout.actual - fitted.value)^2)
MAE.linear[i] <- mean(abs(holdout.actual - fitted.value))

}


################################################################################

# Beta regression model

######################################

# Fit initial model
model.betareg <- lm(AC.ns ~ pf.ns)

# Record p-values for initial model
model.linear.coef.p <- coeftest(model.linear)[,4]

######################################

# Reduce model to include only significant covariates

remove.ind <- c()					   # initialize list of indices for removal
j <- 0
pf.nsr <- pf.ns 	                                  # initialize reduced covariates object

while (sum(model.linear.coef.p > p.max) > 0){            # while there are covariates with p > p.max
  j <- j + 1
  remove.ind[j] <- which(model.linear.coef.p[-1] == max(model.linear.coef.p[-1]))
  pf.nsr <- pf.nsr[,-(remove.ind[j])]
  model.linear <- lm(AC.ns ~ pf.nsr)
  model.linear.coef.p <- coeftest(model.linear)[,4]
}

################################################################################

# Holdouts

######################################

# Data to predict from
data <- pf.ns

# Data to predict
actual <- AC.ns

# Initialize error vectors
MSE.linear <- matrix(0,0,0)
MAE.linear <- matrix(0,0,0)

######################################

# Holdouts

for(i in 1:num.holdouts){

# Generate random string
y <- holdout.indices[i,]

# Create holdout sample for this iteration
tmp.data <- cbind(data,y)
tmp.actual <- cbind(actual,y)

holdout.data <- subset(tmp.data,y==1)[,1:ncol(data)]
holdout.actual <- subset(tmp.actual,y==1)[,1]
leftover.data <- subset(tmp.data,y==0)[,1:ncol(data)]
leftover.data <- as.matrix(leftover.data)
leftover.actual <- subset(tmp.actual,y==0)[,1]

# Fit Model to leftover data

model.linear.holdout <- lm(leftover.actual ~ leftover.data)     	# fit initial model
model.linear.holdout.p <- coeftest(model.linear.holdout)[,4]		# p-values for initial model

remove.ind <- c()
j <- 0
leftover.data.r <- leftover.data

while (sum(model.linear.holdout.p > 0.05) > 0){
# while there are covariates with p > 0.05
  j <- j + 1
  remove.ind[j] <- which(model.linear.holdout.p == max(model.linear.holdout.p))
  leftover.data.r <- leftover.data.r[,-(remove.ind[j]-1)]
  model.linear.holdout <- lm(leftover.actual ~ leftover.data.r)    
  model.linear.holdout.p <- coeftest(model.linear.holdout)[,4]
}

# Reduce holdout data

holdout.data.r <- holdout.data
for (m in 1:j){
  if (!is.null(remove.ind)){
    holdout.data.r <- holdout.data.r[,-(remove.ind[m]-1)]
  }    
}

# Calculate fitted Y

fitted.value <- c()
for (k in 1:nrow(holdout.data.r)){
  fitted.value[k] <- model.linear.holdout$coefficients[1]
  for (l in 1:ncol(holdout.data.r)){
    fitted.value[k] <- fitted.value[k] + model.linear.holdout$coefficients[l+1]*holdout.data.r[k,l]
  }
}


#Calculate mean error for this holdout sample
MSE.linear[i] <- mean((holdout.actual - fitted.value)^2)
MAE.linear[i] <- mean(abs(holdout.actual - fitted.value))

}

################################################################################







pfmodel <- list(pf.nsr, AC.ns, model.linear, model.linear.coef.p, MSE.linear, MAE.linear, pf.all, AC.all, covariates, load.max, p.max,s)
names(pfmodel) <- c('pf.nsr', 'AC.ns','model.linear', 'model.linear.coef.p', 'MSE.linear', 'MAE.linear', 'pf.all', 'AC.all', 'covariates', 'load.max', 'p.max','s')

return(pfmodel)

}

