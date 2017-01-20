################################################################################
# NOTATION KEY

# pfn := power flow, node failures
# pfe := power flow, edge failures

# n := normalized or rescaled
# s := subset
# r := reduced to significant variables

################################################################################

modelPowerflow <- function(pf.all,AC.all,covariates,load.max,p.max,s,num.holdouts,holdout.indices,run.betareg){

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

if (s==0){

pf.na <- pf.n				# all covariates, normalized
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

pf.n <- cbind(pf.na[,1],pf.n)
names(pf.n) <- c('numfail',covariates)

}else{
pf.na <- pf.n				# all covariates, normalized
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
}

########################################

# Subset data by number of elements removed

if (s == 0){

AC.ns <- AC.n
AC.ns <- data.frame(AC.ns)
AC.ns <- as.matrix(AC.ns)

pf.ns <- pf.n
pf.ns <- data.frame(pf.ns)
pf.ns <- as.matrix(pf.ns)

pf.nsa <- pf.na			# includes all covariates
pf.nsa <- data.frame(pf.nsa)

}else{

AC.ns <- AC.n[pf.all$numfail == s]
AC.ns <- data.frame(AC.ns)
AC.ns <- as.matrix(AC.ns)

pf.ns <- pf.n[pf.all$numfail == s,]
pf.ns <- data.frame(pf.ns)
pf.ns <- as.matrix(pf.ns)

pf.nsa <- pf.na[pf.all$numfail == s,]			# includes all covariates
pf.nsa <- data.frame(pf.nsa)
}

########################################

# Initialize variables

holdout.actual.linear <- matrix(0,0,0)
fitted.value.linear <- matrix(0,0,0)

holdout.actual.betareg <- matrix(0,0,0)
fitted.value.betareg <- matrix(0,0,0)

holdout.actual.nomodel <- matrix(0,0,0)
fitted.value.nomodel <- matrix(0,0,0)

################################################################################

# Linear models for individual covariates

######################################

# Fit models
model.LCSG <- lm(AC.ns ~ pf.nsa$LCSG)
model.D <- lm(AC.ns ~ pf.nsa$D)
model.E <- lm(AC.ns ~ pf.nsa$E)
model.EN <- lm(AC.ns ~ pf.nsa$EN)
model.ENE <- lm(AC.ns ~ pf.nsa$ENE)
model.CL <- lm(AC.ns ~ pf.nsa$CL)
model.PCL <- lm(AC.ns ~ pf.nsa$PCL)
model.PNS <- lm(AC.ns ~ pf.nsa$PNS)
model.DC <- lm(AC.ns ~ pf.nsa$DC)

# Residual sum of squares

rss.LCSG <- sum(resid(model.LCSG)^2)
rss.D <- sum(resid(model.D)^2)
rss.E <- sum(resid(model.E)^2)
rss.EN <- sum(resid(model.EN)^2)
rss.ENE <- sum(resid(model.ENE)^2)
rss.CL <- sum(resid(model.CL)^2)
rss.PCL <- sum(resid(model.PCL)^2)
rss.PNS <- sum(resid(model.PNS)^2)
rss.DC <- sum(resid(model.DC)^2)

tss.LCSG <- sum((pf.nsa$LCSG - mean(pf.nsa$LCSG))^2)
tss.D <- sum((pf.nsa$D - mean(pf.nsa$D))^2)
tss.E <- sum((pf.nsa$E - mean(pf.nsa$E))^2)
tss.EN <- sum((pf.nsa$EN - mean(pf.nsa$EN))^2)
tss.ENE <- sum((pf.nsa$ENE - mean(pf.nsa$ENE))^2)
tss.CL <- sum((pf.nsa$CL - mean(pf.nsa$CL))^2)
tss.PCL <- sum((pf.nsa$PCL - mean(pf.nsa$PCL))^2)
tss.PNS <- sum((pf.nsa$PNS - mean(pf.nsa$PNS))^2)
tss.DC <- sum((pf.nsa$DC - mean(pf.nsa$DC))^2)

################################################################################

if (num.holdouts > 0){

# Holdouts

######################################

# Data to predict from
data <- pf.nsa

# Data to predict
actual <- AC.ns

# Initialize error vectors
MSE.LCSG <- matrix(0,0,0)
MAE.LCSG <- matrix(0,0,0)

MSE.D <- matrix(0,0,0)
MAE.D <- matrix(0,0,0)

MSE.E <- matrix(0,0,0)
MAE.E <- matrix(0,0,0)

MSE.EN <- matrix(0,0,0)
MAE.EN <- matrix(0,0,0)

MSE.ENE <- matrix(0,0,0)
MAE.ENE <- matrix(0,0,0)

MSE.CL <- matrix(0,0,0)
MAE.CL <- matrix(0,0,0)

MSE.PCL <- matrix(0,0,0)
MAE.PCL <- matrix(0,0,0)

MSE.PNS <- matrix(0,0,0)
MAE.PNS <- matrix(0,0,0)

MSE.DC <- matrix(0,0,0)
MAE.DC <- matrix(0,0,0)

######################################
# Holdouts

for(i in 1:num.holdouts){

# Generate random string
y <- holdout.indices[i,]

# Create holdout sample for this iteration
tmp.data <- cbind(data,y)
tmp.actual <- cbind(actual,y)

holdout.data <- subset(tmp.data,y==1)[,1:ncol(data)]
names(holdout.data) <- c('numfail','LCSG','D','E','EN','ENE','CL','PCL','PNS','DC')
holdout.actual <- subset(tmp.actual,y==1)[,1]
leftover.data <- subset(tmp.data,y==0)[,1:ncol(data)]
leftover.data <- data.frame(leftover.data)
names(leftover.data) <- c('numfail','LCSG','D','E','EN','ENE','CL','PCL','PNS','DC')
leftover.actual <- subset(tmp.actual,y==0)[,1]

# Fit Model to leftover data

model.LCSG.holdout <- lm(leftover.actual ~ leftover.data$LCSG)
model.D.holdout <- lm(leftover.actual ~ leftover.data$D)
model.E.holdout <- lm(leftover.actual ~ leftover.data$E)
model.EN.holdout <- lm(leftover.actual ~ leftover.data$EN)
model.ENE.holdout <- lm(leftover.actual ~ leftover.data$ENE)
model.CL.holdout <- lm(leftover.actual ~ leftover.data$CL)
model.PCL.holdout <- lm(leftover.actual ~ leftover.data$PCL)
model.PNS.holdout <- lm(leftover.actual ~ leftover.data$PNS)
model.DC.holdout <- lm(leftover.actual ~ leftover.data$DC)

#Calculate mean error for this holdout sample

fitted.value <- c()
for (k in 1:nrow(holdout.data)){
  fitted.value[k] <- model.LCSG.holdout$coefficients[1]
  fitted.value[k] <- fitted.value[k] + model.LCSG.holdout$coefficients[2]*holdout.data$LCSG[k]
}
MSE.LCSG[i] <- mean((holdout.actual - fitted.value)^2)
MAE.LCSG[i] <- mean(abs(holdout.actual - fitted.value))

fitted.value <- c()
for (k in 1:nrow(holdout.data)){
  fitted.value[k] <- model.D.holdout$coefficients[1]
  fitted.value[k] <- fitted.value[k] + model.D.holdout$coefficients[2]*holdout.data$D[k]
}
MSE.D[i] <- mean((holdout.actual - fitted.value)^2)
MAE.D[i] <- mean(abs(holdout.actual - fitted.value))

fitted.value <- c()
for (k in 1:nrow(holdout.data)){
  fitted.value[k] <- model.E.holdout$coefficients[1]
  fitted.value[k] <- fitted.value[k] + model.E.holdout$coefficients[2]*holdout.data$E[k]
}
MSE.E[i] <- mean((holdout.actual - fitted.value)^2)
MAE.E[i] <- mean(abs(holdout.actual - fitted.value))

fitted.value <- c()
for (k in 1:nrow(holdout.data)){
  fitted.value[k] <- model.EN.holdout$coefficients[1]
  fitted.value[k] <- fitted.value[k] + model.EN.holdout$coefficients[2]*holdout.data$EN[k]
}
MSE.EN[i] <- mean((holdout.actual - fitted.value)^2)
MAE.EN[i] <- mean(abs(holdout.actual - fitted.value))

fitted.value <- c()
for (k in 1:nrow(holdout.data)){
  fitted.value[k] <- model.ENE.holdout$coefficients[1]
  fitted.value[k] <- fitted.value[k] + model.ENE.holdout$coefficients[2]*holdout.data$ENE[k]
}
MSE.ENE[i] <- mean((holdout.actual - fitted.value)^2)
MAE.ENE[i] <- mean(abs(holdout.actual - fitted.value))

fitted.value <- c()
for (k in 1:nrow(holdout.data)){
  fitted.value[k] <- model.CL.holdout$coefficients[1]
  fitted.value[k] <- fitted.value[k] + model.CL.holdout$coefficients[2]*holdout.data$CL[k]
}
MSE.CL[i] <- mean((holdout.actual - fitted.value)^2)
MAE.CL[i] <- mean(abs(holdout.actual - fitted.value))

fitted.value <- c()
for (k in 1:nrow(holdout.data)){
  fitted.value[k] <- model.PCL.holdout$coefficients[1]
  fitted.value[k] <- fitted.value[k] + model.PCL.holdout$coefficients[2]*holdout.data$PCL[k]
}
MSE.PCL[i] <- mean((holdout.actual - fitted.value)^2)
MAE.PCL[i] <- mean(abs(holdout.actual - fitted.value))

fitted.value <- c()
for (k in 1:nrow(holdout.data)){
  fitted.value[k] <- model.PNS.holdout$coefficients[1]
  fitted.value[k] <- fitted.value[k] + model.PNS.holdout$coefficients[2]*holdout.data$PNS[k]
}
MSE.PNS[i] <- mean((holdout.actual - fitted.value)^2)
MAE.PNS[i] <- mean(abs(holdout.actual - fitted.value))

fitted.value <- c()
for (k in 1:nrow(holdout.data)){
  fitted.value[k] <- model.DC.holdout$coefficients[1]
  fitted.value[k] <- fitted.value[k] + model.DC.holdout$coefficients[2]*holdout.data$DC[k]
}
MSE.DC[i] <- mean((holdout.actual - fitted.value)^2)
MAE.DC[i] <- mean(abs(holdout.actual - fitted.value))


}

}


################################################################################
################################################################################

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

pf.nsr.linear <- pf.nsr

rss.linear <- sum(resid(model.linear)^2)
tss.linear <- sum((AC.ns-mean(AC.ns))^2)

################################################################################

if (num.holdouts > 0){

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
leftover.data.r <- as.matrix(leftover.data.r)

while (sum(model.linear.holdout.p[-1] > p.max) > 0){
# while there are covariates with p > 0.05
  j <- j + 1
  remove.ind[j] <- which(model.linear.holdout.p[-1] == max(model.linear.holdout.p[-1]))
  leftover.data.r <- leftover.data.r[,-(remove.ind[j])]
  model.linear.holdout <- lm(leftover.actual ~ leftover.data.r)    
  model.linear.holdout.p <- coeftest(model.linear.holdout)[,4]
}

# Reduce holdout data

holdout.data.r <- holdout.data
for (m in 1:j){
  if (!is.null(remove.ind)){
    holdout.data.r <- holdout.data.r[,-(remove.ind[m])]
  }    
}

# Calculate fitted Y

if (is.null(dim(holdout.data.r))){
  fitted.value <- c()
  for (k in 1:length(holdout.data.r)){
    fitted.value[k] <- model.linear.holdout$coefficients[1]
    for (l in 1){
      fitted.value[k] <- fitted.value[k] + model.linear.holdout$coefficients[l+1]*holdout.data.r[k]
    }
  }
}
else{
  fitted.value <- c()
  for (k in 1:nrow(holdout.data.r)){
    fitted.value[k] <- model.linear.holdout$coefficients[1]
    for (l in 1:ncol(holdout.data.r)){
      fitted.value[k] <- fitted.value[k] + model.linear.holdout$coefficients[l+1]*holdout.data.r[k,l]
    }
  }
}


#Calculate mean error for this holdout sample

MSE.linear[i] <- mean((holdout.actual - fitted.value)^2)
MAE.linear[i] <- mean(abs(holdout.actual - fitted.value))


#Save holdout predictions and actual values

#holdout.actual.linear <- cbind(holdout.actual.linear, holdout.actual)
#fitted.value.linear[i,1:length(fitted.value)] <- fitted.value

}

}else{
MSE.linear <- c()
MAE.linear <- c()
holdout.actual.linear <- matrix(0,0,0)
fitted.value.linear <- matrix(0,0,0)
}

################################################################################

################################################################################

if (run.betareg == 1){

# Beta regression model

######################################

pf.ns <- pf.n[pf.all$numfail == s,]
pf.ns <- data.frame(pf.ns)
pf.ns <- as.matrix(pf.ns)


# Fit initial model
model.betareg <- betareg(AC.ns ~ pf.ns)

# Record p-values for initial model
model.betareg.coef.p <- coeftest(model.betareg)[,4]

######################################

# Reduce model to include only significant covariates

remove.ind <- c()					   # initialize list of indices for removal
j <- 0
pf.nsr <- pf.ns 	                                  # initialize reduced covariates object

while (sum(model.betareg.coef.p[-1] > p.max) > 0){            # while there are covariates with p > p.max
  j <- j + 1
  remove.ind[j] <- which(model.betareg.coef.p[-1] == max(model.betareg.coef.p[-1]))
  pf.nsr <- pf.nsr[,-(remove.ind[j])]
  model.betareg <- betareg(AC.ns ~ pf.nsr)
  model.betareg.coef.p <- coeftest(model.betareg)[,4]
}

pf.nsr.betareg <- pf.nsr

rss.betareg <- sum(residuals(model.betareg, type = c('response'))^2)
tss.betareg <- sum((AC.ns-mean(AC.ns))^2)

################################################################################

if (num.holdouts > 0){

# Holdouts

######################################

# Data to predict from
data <- pf.ns

# Data to predict
actual <- AC.ns

# Initialize error vectors
MSE.betareg <- matrix(0,0,0)
MAE.betareg <- matrix(0,0,0)

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

model.betareg.holdout <- betareg(leftover.actual ~ leftover.data)     	# fit initial model
model.betareg.holdout.p <- coeftest(model.betareg.holdout)[,4]		# p-values for initial model

remove.ind <- c()
j <- 0
leftover.data.r <- leftover.data

while (sum(model.betareg.holdout.p[-1] > p.max) > 0){
# while there are covariates with p > 0.05
  j <- j + 1
  remove.ind[j] <- which(model.betareg.holdout.p[-1] == max(model.betareg.holdout.p[-1]))
  leftover.data.r <- leftover.data.r[,-(remove.ind[j])]
  model.betareg.holdout <- betareg(leftover.actual ~ leftover.data.r)    
  model.betareg.holdout.p <- coeftest(model.betareg.holdout)[,4]
}

# Reduce holdout data

holdout.data.r <- holdout.data
for (m in 1:j){
  if (!is.null(remove.ind)){
    holdout.data.r <- holdout.data.r[,-(remove.ind[m])]
  }    
}

# Calculate fitted Y

if (is.null(dim(holdout.data.r))){
fitted.value <- c()
for (k in 1:length(holdout.data.r)){
  fitted.value[k] <- model.betareg.holdout$coefficients$mean[1]
  for (l in 1){
    fitted.value[k] <- fitted.value[k] + model.betareg.holdout$coefficients$mean[l+1]*holdout.data.r[k]
  }
}
fitted.value <- exp(fitted.value)/(1 + exp(fitted.value))
}
else{
fitted.value <- c()
for (k in 1:nrow(holdout.data.r)){
  fitted.value[k] <- model.betareg.holdout$coefficients$mean[1]
  for (l in 1:ncol(holdout.data.r)){
    fitted.value[k] <- fitted.value[k] + model.betareg.holdout$coefficients$mean[l+1]*holdout.data.r[k,l]
  }
}
fitted.value <- exp(fitted.value)/(1 + exp(fitted.value))
}

#Calculate mean error for this holdout sample

MSE.betareg[i] <- mean((holdout.actual - fitted.value)^2)
MAE.betareg[i] <- mean(abs(holdout.actual - fitted.value))


#Save holdout predictions and actual values

#holdout.actual.betareg[i,] <- holdout.actual
#fitted.value.betareg[i,] <- fitted.value

}
}

else{
MSE.betareg <- c()
MAE.betareg <- c()
holdout.actual.betareg <- matrix(0,0,0)
fitted.value.betareg <- matrix(0,0,0)
}

}else{
model.betareg <- c()
model.betareg.coef.p <- c()
rss.betareg <- c()
tss.betareg <- c()
MSE.betareg <- c()
MAE.betareg <- c()
holdout.actual.betareg <- c()
fitted.value.betareg <- c()
pf.nsr.betareg <- c()
}

################################################################################

################################################################################

# No model (mean)

model.nomodel <- mean(AC.ns)

######################################

if (num.holdouts > 0){

# Holdouts

# Data to predict from
data <- pf.ns

# Data to predict
actual <- AC.ns

# Initialize error vectors
MSE.nomodel <- matrix(0,0,0)
MAE.nomodel <- matrix(0,0,0)

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

model.nomodel.holdout <- mean(leftover.actual)     	# fit initial model

# Calculate fitted Y

fitted.value <- model.nomodel.holdout

# Calculate mean error for this holdout sample

MSE.nomodel[i] <- mean((holdout.actual - fitted.value)^2)
MAE.nomodel[i] <- mean(abs(holdout.actual - fitted.value))


# Save holdout predictions and actual values

#holdout.actual.nomodel[i,] <- holdout.actual
#fitted.value.nomodel[i,] <- fitted.value

}
}

else{
MSE.nomodel <- c()
MAE.nomodel <- c()
holdout.actual.nomodel <- matrix(0,0,0)
fitted.value.nomodel <- matrix(0,0,0)
}

################################################################################

################################################################################
# Output

pfmodel <- list(model.LCSG, model.D, model.E, model.EN, model.ENE, model.CL, model.PCL, model.PNS, model.DC, rss.LCSG, rss.D, rss.E, rss.EN, rss.ENE, rss.CL, rss.PCL, rss.PNS, rss.DC, tss.LCSG, tss.D, tss.E, tss.EN, tss.ENE, tss.CL, tss.PCL, tss.PNS, tss.DC, MSE.LCSG, MAE.LCSG, MSE.D, MAE.D, MSE.E, MAE.E, MSE.EN, MAE.EN, MSE.ENE, MAE.ENE, MSE.CL, MAE.CL, MSE.PCL, MAE.PCL, MSE.PNS, MAE.PNS, MSE.DC, MAE.DC, model.linear, model.linear.coef.p, rss.linear, tss.linear, MSE.linear, MAE.linear, holdout.actual.linear, fitted.value.linear, model.betareg, model.betareg.coef.p, rss.betareg, tss.betareg, MSE.betareg, MAE.betareg, holdout.actual.betareg, fitted.value.betareg, model.nomodel, MSE.nomodel, MAE.nomodel, holdout.actual.nomodel, fitted.value.nomodel, pf.nsr.linear, pf.nsr.betareg, AC.ns, pf.all, AC.all, covariates, load.max, p.max, s, num.holdouts, holdout.indices)

names(pfmodel) <- c('model.LCSG', 'model.D', 'model.E', 'model.EN', 'model.ENE', 'model.CL', 'model.PCL', 'model.PNS', 'model.DC', 'rss.LCSG', 'rss.D', 'rss.E', 'rss.EN', 'rss.ENE', 'rss.CL', 'rss.PCL', 'rss.PNS', 'rss.DC', 'tss.LCSG', 'tss.D', 'tss.E', 'tss.EN', 'tss.ENE', 'tss.CL', 'tss.PCL', 'tss.PNS', 'tss.DC', 'MSE.LCSG', 'MAE.LCSG', 'MSE.D', 'MAE.D', 'MSE.E', 'MAE.E', 'MSE.EN', 'MAE.EN', 'MSE.ENE', 'MAE.ENE', 'MSE.CL', 'MAE.CL', 'MSE.PCL', 'MAE.PCL', 'MSE.PNS', 'MAE.PNS', 'MSE.DC', 'MAE.DC', 'model.linear', 'model.linear.coef.p', 'rss.linear', 'tss.linear', 'MSE.linear', 'MAE.linear', 'holdout.actual.linear', 'fitted.value.linear', 'model.betareg', 'model.betareg.coef.p', 'rss.betareg', 'tss.betareg', 'MSE.betareg', 'MAE.betareg', 'holdout.actual.betareg', 'fitted.value.betareg', 'model.nomodel', 'MSE.nomodel', 'MAE.nomodel', 'holdout.actual.nomodel', 'fitted.value.nomodel', 'pf.nsr.linear', 'pf.nsr.betareg', 'AC.ns', 'pf.all', 'AC.all', 'covariates', 'load.max', 'p.max','s', 'num.holdouts', 'holdout.
indices')


return(pfmodel)

}

