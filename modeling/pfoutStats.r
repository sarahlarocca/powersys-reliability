################################################################################

rmse.linear.full <- matrix(0,length(covariates.list)+10,length(s.list))

for (i in 1:length(covariates.list)){
  for (j in 1:length(s.list)){
    rmse.linear.full[i,j] <- sqrt(mean((pfout[[paste(i,j)]]$model.linear$residuals)^2))
  }
}

for (j in 1:length(s.list)){
  rmse.linear.full[i+1,j] <- sqrt(mean((pfout[[paste(i,j)]]$model.LCSG$residuals)^2))
  rmse.linear.full[i+2,j] <- sqrt(mean((pfout[[paste(i,j)]]$model.D$residuals)^2))
  rmse.linear.full[i+3,j] <- sqrt(mean((pfout[[paste(i,j)]]$model.E$residuals)^2))
  rmse.linear.full[i+4,j] <- sqrt(mean((pfout[[paste(i,j)]]$model.EN$residuals)^2))
  rmse.linear.full[i+5,j] <- sqrt(mean((pfout[[paste(i,j)]]$model.ENE$residuals)^2))
  rmse.linear.full[i+6,j] <- sqrt(mean((pfout[[paste(i,j)]]$model.CL$residuals)^2))
  rmse.linear.full[i+7,j] <- sqrt(mean((pfout[[paste(i,j)]]$model.PCL$residuals)^2))
  rmse.linear.full[i+8,j] <- sqrt(mean((pfout[[paste(i,j)]]$model.PNS$residuals)^2))
  rmse.linear.full[i+9,j] <- sqrt(mean((pfout[[paste(i,j)]]$model.DC$residuals)^2))  
  rmse.linear.full[i+10,j] <- sqrt(mean((pfout[[paste(i,j)]]$model.nomodel - pfout[[paste(i,j)]]$AC.ns)^2))

}

################################################################################


rmse.linear.holdout <- matrix(0,length(covariates.list)+10,length(s.list))

for (i in 1:length(covariates.list)){
  for (j in 1:length(s.list)){
    rmse.linear.holdout[i,j] <- mean(sqrt(pfout[[paste(i,j)]]$MSE.linear))
  }
}

for (j in 1:length(s.list)){
  rmse.linear.holdout[i+1,j] <- mean(sqrt(pfout[[paste(i,j)]]$MSE.LCSG))
  rmse.linear.holdout[i+2,j] <- mean(sqrt(pfout[[paste(i,j)]]$MSE.D))
  rmse.linear.holdout[i+3,j] <- mean(sqrt(pfout[[paste(i,j)]]$MSE.E))
  rmse.linear.holdout[i+4,j] <- mean(sqrt(pfout[[paste(i,j)]]$MSE.EN))
  rmse.linear.holdout[i+5,j] <- mean(sqrt(pfout[[paste(i,j)]]$MSE.ENE))
  rmse.linear.holdout[i+6,j] <- mean(sqrt(pfout[[paste(i,j)]]$MSE.CL))
  rmse.linear.holdout[i+7,j] <- mean(sqrt(pfout[[paste(i,j)]]$MSE.PCL))
  rmse.linear.holdout[i+8,j] <- mean(sqrt(pfout[[paste(i,j)]]$MSE.PNS))
  rmse.linear.holdout[i+9,j] <- mean(sqrt(pfout[[paste(i,j)]]$MSE.DC))
  rmse.linear.holdout[i+10,j] <- mean(sqrt(pfout[[paste(i,j)]]$MSE.nomodel))

}

################################################################################

rmse.linear.holdout.min95 <- matrix(0,length(covariates.list)+10,length(s.list))

for (i in 1:length(covariates.list)){
  for (j in 1:length(s.list)){
    rmse.linear.holdout.min95[i,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.linear))[3]
  }
}

for (j in 1:length(s.list)){
  rmse.linear.holdout.min95[i+1,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.LCSG))[3]
  rmse.linear.holdout.min95[i+2,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.D))[3]
  rmse.linear.holdout.min95[i+3,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.E))[3]
  rmse.linear.holdout.min95[i+4,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.EN))[3]
  rmse.linear.holdout.min95[i+5,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.ENE))[3]
  rmse.linear.holdout.min95[i+6,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.CL))[3]
  rmse.linear.holdout.min95[i+7,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.PCL))[3]
  rmse.linear.holdout.min95[i+8,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.PNS))[3]
  rmse.linear.holdout.min95[i+9,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.DC))[3]
  rmse.linear.holdout.min95[i+10,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.nomodel))[3]

}

################################################################################

rmse.linear.holdout.max95 <- matrix(0,length(covariates.list)+10,length(s.list))
pfout[[paste(i,j)]]$MSE.LCSG
for (i in 1:length(covariates.list)){
  for (j in 1:length(s.list)){
    rmse.linear.holdout.max95[i,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.linear))[98]
  }
}

for (j in 1:length(s.list)){
  rmse.linear.holdout.max95[i+1,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.LCSG))[98]
  rmse.linear.holdout.max95[i+2,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.D))[98]
  rmse.linear.holdout.max95[i+3,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.E))[98]
  rmse.linear.holdout.max95[i+4,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.EN))[98]
  rmse.linear.holdout.max95[i+5,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.ENE))[98]
  rmse.linear.holdout.max95[i+6,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.CL))[98]
  rmse.linear.holdout.max95[i+7,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.PCL))[98]
  rmse.linear.holdout.max95[i+8,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.PNS))[98]
  rmse.linear.holdout.max95[i+9,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.DC))[98]
  rmse.linear.holdout.max95[i+10,j] <- sort(sqrt(pfout[[paste(i,j)]]$MSE.nomodel))[98]

}

################################################################################

