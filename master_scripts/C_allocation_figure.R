###this script visualizes allocation to different components by treatment
# source("functions_and_packages/plot_objects.R")
# source("functions_and_packages/functions.R")
# library(plotrix)

##need final harvest values (not treatmen means)
tree_C <- read.csv("master_scripts/harvest_chamber.csv")

##calculated component fractions
tree_C$tree_total <- with(tree_C, branchC+boleC+leafC+litterC+frootC_all+CrootC)
  tree_C$lmf <- with(tree_C, leafC_litterC/tree_total)
  tree_C$smf <- with(tree_C, (branchC+boleC)/tree_total)
  tree_C$rmf <- with(tree_C, (frootC_all + CrootC)/tree_total)

##order by treatment
tree_C <- tree_C[order(tree_C$treatment),]

#simple mode for abline and sig
lmfmod <- lm(lmf ~ tree_total, data = tree_C)
#   plmf <- round(getP(lmfmod),3)
#   r2lmf <- round(getR(lmfmod),2)

smfmod <- lm(smf~ tree_total, data = tree_C)
#   psmf <- round(getP(smfmod),3)
#   r2smf <- getR(smfmod)

rmfmod <- lm(rmf ~ tree_total, data = tree_C)
#   prmf <- round(getP(rmfmod),3)
#   r2rmf <- getR(rmfmod)


####plot bits (palette and custom axes)
palette(c("blue", "red"))

at.y2 <- seq(.3, by=.1, length.out=5)
at.y3 <- seq(.2, by=.1, length.out=5)

at.x <- seq(5000, by=5000, length.out=5)

###3 panel box plotwith LMF, RMF, SMF by treatment

# windows(7,7)

par(cex.axis=1.21, cex.lab=1.51, las=1,mgp=c(3,1,0),mfrow=c(3,1), oma=c(5, 0, 2,0))  
#lmf
par(mar=c(0,7,0,2))
with(tree_C, plot(lmf~tree_total, ylab="", xaxt='n', xlab="", xlim=c(7500,25000), ylim=c(0,.175), type='n'))
ablineclip(lmfmod, x1=min(tree_C$tree_total), x2=max(tree_C$tree_total),lwd=2)
with(tree_C, points(lmf~tree_total, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5))
title(ylab=lmflab, mgp=c(4,1,0))
legend("topleft", leglab2, pch=c(19,1, 19, 1), col=c("blue", "blue", "red", "red"), inset = 0.01,bty='n')
#smf
par(mar=c(0,7,0,2))
with(tree_C, plot(smf~tree_total,  ylab="", xaxt='n', yaxt='n', xlab="",ylim=c(.35,.75), xlim=c(5000,25000),type='n'))
ablineclip(smfmod, x1=min(tree_C$tree_total), x2=max(tree_C$tree_total),lwd=2)
with(tree_C, points(smf~tree_total, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5))
title(ylab=smflab, mgp=c(4,1,0))
axis(2, at=at.y2 ,labels=TRUE)
#rmf
par(mar=c(0,7,0,2))
with(tree_C, plot(rmf~tree_total,  ylab="", xaxt='n',yaxt='n',xlab= "",ylim=c(.25, .55), xlim=c(7500,25000),type='n'))
ablineclip(rmfmod, x1=min(tree_C$tree_total), x2=max(tree_C$tree_total),lwd=2)
with(tree_C, points(rmf~tree_total, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5))
axis(2, at=at.y3 ,labels=TRUE)
axis(1, at=at.x ,labels=TRUE, outer=TRUE)
title(ylab=rmflab, mgp=c(4,1,0))
mtext(treeclab, side=1, outer=TRUE, line=3)

# dev.copy2pdf(file="master_scripts/paper_figs/c_allocation.pdf")
# dev.off() 
   