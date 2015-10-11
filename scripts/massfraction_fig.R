###this script visualizes partitioning to different components by treatment
source("functions_and_packages/plot_objects.R")
source("functions_and_packages/functions.R")
library(plotrix)

#read data for cumulative C flux and harvest mass
tree_C <- read.csv("calculated_mass/chamber_carbon.csv")

##calculated component fractions
  tree_C$lmf <- with(tree_C, (leafcarbon+littercarbon)/treeC)
  tree_C$smf <- with(tree_C, (boleC+branchC)/treeC)
  tree_C$rmf <- with(tree_C, (rootC)/treeC)

##order by treatment
tree_C$treatment <- with(tree_C, paste(CO2_treatment, Water_treatment, sep="-"))  
tree_C <- tree_C[order(tree_C$treatment),]

#simple mode for abline and sig
lmfmod <- lm(lmf ~ treeC, data = tree_C)
summary(lmfmod)
#   plmf <- round(getP(lmfmod),3)
#   r2lmf <- round(getR(lmfmod),2)

smfmod <- lm(smf~ treeC, data = tree_C)
summary(smfmod)
#   psmf <- round(getP(smfmod),3)
#   r2smf <- getR(smfmod)

rmfmod <- lm(rmf ~ treeC, data = tree_C)
summary(rmfmod)
#   prmf <- round(getP(rmfmod),3)
#   r2rmf <- getR(rmfmod)


####plot bits (palette and custom axes)
palette(c("blue", "red"))

at.y2 <- seq(.3, by=.1, length.out=5)
# at.y3 <- seq(.15, by=.075, length.out=5)

at.x <- seq(5000, by=5000, length.out=4)

###3 panel box plotwith LMF, RMF, SMF by treatment

# windows(7,7)

par(cex.axis=1.21, cex.lab=1.51, las=1,mgp=c(3,1,0),mfrow=c(3,1), oma=c(5, 0, 2,0))  

#lmf
par(mar=c(0,7,0,2))
with(tree_C, plot(lmf~treeC, ylab="", xaxt='n', xlab="", xlim=c(5000,22500), ylim=c(.15,.4), type='n'))
ablineclip(lmfmod, x1=min(tree_C$treeC), x2=max(tree_C$treeC),lwd=2)
with(tree_C, points(lmf~treeC, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5))
title(ylab=lmflab, mgp=c(4,1,0))
legend("topleft", leglab2, pch=c(19,1, 19, 1), col=c("blue", "blue", "red", "red"), inset = 0.01,bty='n')
text(x=22500, y=0.39,labels="(a)", cex=1.5)

#smf
par(mar=c(0,7,0,2))
with(tree_C, plot(smf~treeC,  ylab="", xaxt='n', yaxt='n', xlab="",ylim=c(.35,.75), xlim=c(5000,22500),type='n'))
ablineclip(smfmod, x1=min(tree_C$treeC), x2=max(tree_C$treeC),lwd=2)
with(tree_C, points(smf~treeC, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5))
title(ylab=smflab, mgp=c(4,1,0))
axis(2, at=at.y2 ,labels=TRUE)
text(x=22500, y=0.73,labels="(b)", cex=1.5)

#rmf
par(mar=c(0,7,0,2))
with(tree_C, plot(rmf~treeC,  ylab="", xaxt='n',yaxt='n',xlab= "",ylim=c(.1, .3), xlim=c(5000,22500),type='n'))
ablineclip(rmfmod, x1=min(tree_C$treeC), x2=max(tree_C$treeC),lwd=2)
with(tree_C, points(rmf~treeC, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5))
axis(2, labels=TRUE)
axis(1, at=at.x ,labels=TRUE, outer=TRUE)
title(ylab=rmflab, mgp=c(4,1,0))
mtext(treeclab, side=1, outer=TRUE, line=3)
text(x=22500, y=0.29,labels="(c)", cex=1.5)

# dev.copy2pdf(file="master_scripts/paper_figs/massfractions.pdf")
# dev.off()
   