################################################################################
# Set directory, load libraries

setwd('/media/files/jhu/infrastructure/power.systems/statistics')

library(mgcv)
library(MASS)
library(nlme)
library(betareg)
library(lmtest)
library(gam)

source('modelPowerflow.R')

################################################################################
# Import data

pfn.all <- read.table('edgeremovals.csv',header=TRUE)
names(pfn.all) <- c('id','numfail','LCSG','D','E','EN','ENE','CL','PCL','PNS','DC','AC')
pf.all <- data.frame(pfn.all)				# original dataset
AC.all <- pfn.all$AC					# original AC results

################################################################################

load.max <- 2850
p.max <- 0.05
num.holdouts <- 100
holdout.indices <- read.table('holdoutIndices.txt',header=TRUE)
run.betareg <- 0

s.list <- c(1,3,5,7,9)
covariates.list <- list(c('LCSG','D','E'),c('LCSG','D','EN'),c('LCSG','D','EN','CL'),c('LCSG','D','EN','CL','PCL','PNS'),c('CL','PCL','PNS'),c('LCSG','D','EN','CL','PCL','PNS','DC'))


pfout <- list()

for (i in 1:length(covariates.list)){
  for (j in 1:length(s.list)){
    pfout[[paste(i,j)]] <- modelPowerflow(pf.all,AC.all,covariates.list[[i]],load.max,p.max,s.list[j],num.holdouts,holdout.indices,run.betareg)
  }
}

################################################################################