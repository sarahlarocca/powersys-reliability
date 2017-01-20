################################################################################
# Set directory, load libaries

library(graphics)
library(ggplot2)
library(lattice)
library(directlabels)

setwd('/media/files/jhu/infrastructure/power.systems/statistics')
load('pfout111913e.RData')

################################################################################################################

colnames(rmse.linear.holdout) <- c('5','7','9','11','13')
rownames(rmse.linear.holdout) <- c('LCSG/D/E','LCSG/D/EN','LCSG/D/EN/CL','LCSG/D/EN/CL/PCL/PNS','CL/PCL/PNS','LCSG/D/EN/CL/PCL/PNS/DC','LCSG','D','E','EN','ENE','CL','PCL','PNS','DC','Mean-only model')

rmse.linear.holdout.dfr <- data.frame()

j <- 0

for (i in 1:dim(rmse.linear.holdout)[1]){
  rmse.linear.holdout.dfr[j+i,1] <- rmse.linear.holdout[i,1]
  rmse.linear.holdout.dfr[j+i,2] <- rmse.linear.holdout.min95[i,1]
  rmse.linear.holdout.dfr[j+i,3] <- rmse.linear.holdout.max95[i,1]
  rmse.linear.holdout.dfr[j+i,4] <- '5'
  rmse.linear.holdout.dfr[j+i,5] <- rownames(rmse.linear.holdout)[i]
  k <- j + i
}

for (i in 1:dim(rmse.linear.holdout)[1]){
  rmse.linear.holdout.dfr[k+i,1] <- rmse.linear.holdout[i,2]
  rmse.linear.holdout.dfr[k+i,2] <- rmse.linear.holdout.min95[i,2]
  rmse.linear.holdout.dfr[k+i,3] <- rmse.linear.holdout.max95[i,2]
  rmse.linear.holdout.dfr[k+i,4] <- '7'
  rmse.linear.holdout.dfr[k+i,5] <- rownames(rmse.linear.holdout)[i]
  l <- k + i
}

for (i in 1:dim(rmse.linear.holdout)[1]){
  rmse.linear.holdout.dfr[l+i,1] <- rmse.linear.holdout[i,3]
  rmse.linear.holdout.dfr[l+i,2] <- rmse.linear.holdout.min95[i,3]
  rmse.linear.holdout.dfr[l+i,3] <- rmse.linear.holdout.max95[i,3]
  rmse.linear.holdout.dfr[l+i,4] <- '9'
  rmse.linear.holdout.dfr[l+i,5] <- rownames(rmse.linear.holdout)[i]
  m <- l + i
}

for (i in 1:dim(rmse.linear.holdout)[1]){
  rmse.linear.holdout.dfr[m+i,1] <- rmse.linear.holdout[i,4]
  rmse.linear.holdout.dfr[m+i,2] <- rmse.linear.holdout.min95[i,4]
  rmse.linear.holdout.dfr[m+i,3] <- rmse.linear.holdout.max95[i,4]
  rmse.linear.holdout.dfr[m+i,4] <- '11'
  rmse.linear.holdout.dfr[m+i,5] <- rownames(rmse.linear.holdout)[i]
  r <- m + i
}

for (i in 1:dim(rmse.linear.holdout)[1]){
  rmse.linear.holdout.dfr[r+i,1] <- rmse.linear.holdout[i,5]
  rmse.linear.holdout.dfr[r+i,2] <- rmse.linear.holdout.min95[i,5]
  rmse.linear.holdout.dfr[r+i,3] <- rmse.linear.holdout.max95[i,5]
  rmse.linear.holdout.dfr[r+i,4] <- '13'
  rmse.linear.holdout.dfr[r+i,5] <- rownames(rmse.linear.holdout)[i]
}





for (i in 1:dim(rmse.linear.holdout.dfr)[1]){
  if ((rmse.linear.holdout.dfr[i,5] == 'LCSG') | (rmse.linear.holdout.dfr[i,5] == 'D') | (rmse.linear.holdout.dfr[i,5] == 'E') | (rmse.linear.holdout.dfr[i,5] == 'EN') | (rmse.linear.holdout.dfr[i,5] == 'ENE') | (rmse.linear.holdout.dfr[i,5] == 'CL') | (rmse.linear.holdout.dfr[i,5] == 'PCL') | (rmse.linear.holdout.dfr[i,5] == 'PNS') | (rmse.linear.holdout.dfr[i,5] == 'DC')){
    rmse.linear.holdout.dfr[i,6] <- 'Simple regression'
  }
  else{
    rmse.linear.holdout.dfr[i,6] <- 'Multiple regression'
  }
}

names(rmse.linear.holdout.dfr) <- c('rmse','min95','max95','numfail','model','regtype')

data = rmse.linear.holdout.dfr#[(rmse.linear.holdout.dfr$numfail != 'all'),]		# remove data for all nodes removed

data$model = factor(data$model, levels = c('LCSG/D/E','LCSG/D/EN','LCSG/D/EN/CL','LCSG/D/EN/CL/PCL/PNS','CL/PCL/PNS','LCSG/D/EN/CL/PCL/PNS/DC','LCSG','D','E','EN','ENE','CL','PCL','PNS','DC','Mean-only model'))
data$regtype = factor(data$regtype, levels = c('Simple regression', 'Multiple regression'))

rmse.linear.holdout.plot <-	ggplot(data = data, aes(x = numfail, y = rmse, group = model)) +
				#geom_errorbar(width=0.05,aes(ymin = min95, ymax = max95,colour=model)) +
				geom_point(aes(colour=model,fill = model)) +
				#scale_shape_manual(values = c(22,23,24,21,25,22,23,21,22,21,24,25,22,23,24,21)) +
				geom_line(aes(colour=model)) + 
				#scale_linetype_manual(values = c(2,3,4,6,5,1,3,1,2,6,4,5,1,2,3,1)) +
				facet_grid(. ~regtype) +
				ylab('Root mean squared error') + 
				xlab('Number of edge failures') +
				scale_x_discrete(limits=c('5','7','9','11','13')) +
				geom_dl(aes(label = model), method = list('last.points', 'bumpup', dl.trans(x=x+0.2), cex=.3)) +
				theme(legend.position='none')
		 
################################################################################################################

pf.all.dfr <- data.frame()

AC.init <- 2850
LCSG.init <- 24
D.init <- 7
E.init <- 0.4063
EN.init <- 0.3632
ENE.init <- 7.9224

j <- 0

for (i in 1:dim(pf.all)[1]){
  pf.all.dfr[j+i,1] <- pf.all[i,12]/AC.init
  pf.all.dfr[j+i,2] <- 1 - (pf.all[i,3]/LCSG.init)
  pf.all.dfr[j+i,3] <- pf.all[i,2]
  pf.all.dfr[j+i,4] <- 'LCSG'
  pf.all.dfr[j+i,5] <- 1
  k <- j + i
}

for (i in 1:dim(pf.all)[1]){
  pf.all.dfr[k+i,1] <- pf.all[i,12]/AC.init
  pf.all.dfr[k+i,2] <- 1 - (pf.all[i,4]/D.init)
  pf.all.dfr[k+i,3] <- pf.all[i,2]
  pf.all.dfr[k+i,4] <- 'D'
  pf.all.dfr[k+i,5] <- 1
  l <- k + i
}

for (i in 1:dim(pf.all)[1]){
  pf.all.dfr[l+i,1] <- pf.all[i,12]/AC.init
  pf.all.dfr[l+i,2] <- 1 - (pf.all[i,5]/E.init)
  pf.all.dfr[l+i,3] <- pf.all[i,2]
  pf.all.dfr[l+i,4] <- 'E'
  pf.all.dfr[l+i,5] <- 1
  m <- l + i
}

for (i in 1:dim(pf.all)[1]){
  pf.all.dfr[m+i,1] <- pf.all[i,12]/AC.init
  pf.all.dfr[m+i,2] <- 1 - (pf.all[i,6]/EN.init)
  pf.all.dfr[m+i,3] <- pf.all[i,2]
  pf.all.dfr[m+i,4] <- 'EN'
  pf.all.dfr[m+i,5] <- 2
  jj <- m + i
}

for (i in 1:dim(pf.all)[1]){
  pf.all.dfr[jj+i,1] <- pf.all[i,12]/AC.init
  pf.all.dfr[jj+i,2] <- 1 - (pf.all[i,7]/ENE.init)
  pf.all.dfr[jj+i,3] <- pf.all[i,2]
  pf.all.dfr[jj+i,4] <- 'ENE'
  pf.all.dfr[jj+i,5] <- 2
  kk <- jj + i
}

for (i in 1:dim(pf.all)[1]){
  pf.all.dfr[kk+i,1] <- pf.all[i,12]/AC.init
  pf.all.dfr[kk+i,2] <- pf.all[i,8]
  pf.all.dfr[kk+i,3] <- pf.all[i,2]
  pf.all.dfr[kk+i,4] <- 'CL'
  pf.all.dfr[kk+i,5] <- 2
  ll <- kk + i
}

for (i in 1:dim(pf.all)[1]){
  pf.all.dfr[ll+i,1] <- pf.all[i,12]/AC.init
  pf.all.dfr[ll+i,2] <- pf.all[i,9]
  pf.all.dfr[ll+i,3] <- pf.all[i,2]
  pf.all.dfr[ll+i,4] <- 'PCL'
  pf.all.dfr[ll+i,5] <- 3
  mm <- ll + i
}

for (i in 1:dim(pf.all)[1]){
  pf.all.dfr[mm+i,1] <- pf.all[i,12]/AC.init
  pf.all.dfr[mm+i,2] <- pf.all[i,10]/AC.init
  pf.all.dfr[mm+i,3] <- pf.all[i,2]
  pf.all.dfr[mm+i,4] <- 'PNS'
  pf.all.dfr[mm+i,5] <- 3
  jjj <- mm + i
}

for (i in 1:dim(pf.all)[1]){
  pf.all.dfr[jjj+i,1] <- pf.all[i,12]/AC.init
  pf.all.dfr[jjj+i,2] <- pf.all[i,11]/AC.init
  pf.all.dfr[jjj+i,3] <- pf.all[i,2]
  pf.all.dfr[jjj+i,4] <- 'DC'
  pf.all.dfr[jjj+i,5] <- 3
}

#############################################################

scale.pt.zero <- data.frame(x=0,y=0)
scale.pt.one <- data.frame(x=1,y=1)

names(pf.all.dfr) <- c('AC','cov','numfail','model','group')
pf.all.dfr$model <- factor(pf.all.dfr$model, levels = c('LCSG','D','E','EN','ENE','CL','PCL','PNS','DC'))

pf.all.plot.d<-	ggplot(data = pf.all.dfr, aes(x = AC, y = cov)) +
		geom_point(aes(colour = numfail)) +
		scale_colour_gradient(low = '#DDDDDD', high = 'black',name = 'Number of node failures') +
		theme_bw() +
		geom_abline(aes(intercept = 0, slope = 1)) +
		geom_point(data = scale.pt.zero, aes(x,y), alpha = 0) +
		geom_point(data = scale.pt.one, aes(x,y), alpha = 0) +
		facet_wrap(~model, ncol = 3, scales = 'free_y') +
		xlab('AC') +
		ylab('Performance measure') +
		theme(legend.position = 'bottom') +
		theme(axis.text.x = element_text(size=7)) +
		theme(axis.text.y = element_text(size=7))

		
#############################################################		
		
		

gt_getgrob <- function(gt, pattern) {
  idx <- grep(pattern, gt$layout$name)
  if (length(idx) > 1)
    stop("More than one match for pattern '", pattern, "'")
  gt$grobs[[idx]]
}	

################################################
		
g <- ggplotGrob(pf.all.plot.d)
g$layout
strip <- gt_getgrob(g, 't-1')
str(strip)

strip$children$strip.text.x.text.14332$label <- expression(paste('1 - ', Delta, 'LCSG'),sep='')
gt <- ggplot_gtable(ggplot_build(pf.all.plot.d))  
gt <- gtable_add_grob(gt, strip, t=3, l=4, b=3, r=4)

################################################

gt$layout
strip <- gt_getgrob(gt, 't-2')
str(strip)

strip$children$strip.text.x.text.14723$label <- expression(paste('1 - ', Delta, 'D'),sep='')
gt <- gtable_add_grob(gt, strip, t=3, l=7, b=3, r=7)

################################################

gt$layout
strip <- gt_getgrob(gt, 't-3')
str(strip)

strip$children$strip.text.x.text.14729$label <- expression(paste('1 - ', Delta, 'E'),sep='')
gt <- gtable_add_grob(gt, strip, t=3, l=10, b=3, r=10)

################################################

gt$layout
strip <- gt_getgrob(gt, 't-4')
str(strip)

strip$children$strip.text.x.text.14735$label <- expression(paste('1 - ', Delta, 'EN'),sep='')
gt <- gtable_add_grob(gt, strip, t=7, l=4, b=7, r=4)


################################################

gt$layout
strip <- gt_getgrob(gt, 't-5')
str(strip)

strip$children$strip.text.x.text.14741$label <- expression(paste('1 - ', Delta, 'ENE'),sep='')
gt <- gtable_add_grob(gt, strip, t=7, l=7, b=7, r=7)

grid.newpage()
grid.draw(gt)