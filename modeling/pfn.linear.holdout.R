###############################################################################################
# Data to predict from
data <- pfn.ns

# Data to predict
actual <- AC.s

# Initialize error vectors
MSE.linear <- matrix(0,0,0)
MAE.linear <- matrix(0,0,0)

###############################################################################################
# Holdouts

for(i in 1:numHoldouts){

# Generate random string
y <- holdout.indices[i,]

# Create holdout sample for this iteration
tmp.data <- cbind(data,y)
tmp.actual <- cbind(actual,y)

holdout.data <- subset(tmp.data,y==1)[,1:ncol(data)]
holdout.actual <- subset(tmp.actual,y==1)[,1]
leftover.data <- subset(tmp.data,y==0)[,1:ncol(data)]
leftover.actual <- subset(tmp.actual,y==0)[,1]

# Fit Model to leftover data

model.linear.holdout <- lm(leftover.actual ~ leftover.data)     
# fit initial model

model.linear.holdout.p <- coeftest(model.linear.holdout)[,4]
# p-values for initial model

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

holdout.data.red <- holdout.data
for (m in 1:j){
  if (!is.null(remove.ind)){
    holdout.data.red <- holdout.data.red[,-(remove.ind[m]-1)]
  }    
}

# Calculate fitted Y

fitted.value <- c()
for (k in 1:nrow(holdout.data.red)){
  fitted.value[k] <- model.linear.holdout$coefficients[1]
  for (l in 1:ncol(holdout.data.red)){
    fitted.value[k] <- fitted.value[k] + model.linear.holdout$coefficients[l+1]*holdout.data.red[k,l]
  }
}


#Calculate mean error for this holdout sample
MSE.linear[i] <- mean((holdout.actual - fitted.value)^2)
MAE.linear[i] <- mean(abs(holdout.actual - fitted.value))

}

###############################################################################################
