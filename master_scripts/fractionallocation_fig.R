###manuscript figure with both allocation and partitioning
# source("functions_and_packages/plot_objects.R")
# source("functions_and_packages/functions.R")
# library(plotrix)


##calculate allocation and regressions-------------------------------------------------------------------------------------

alloc <- read.csv("master_scripts/Cmassflux11.csv")

## LEAFmass = LEAFalloc * Fc,t - LITTERtotal
leafalloc <- alloc[, c(1:3,6:7,9)]
leafalloc$leafallocation <- with(leafalloc, (leaf11+litter11)/cflux11)

##STEM allocation
##turnover is already added to final branch mass so is included in this calculations
stemalloc <- alloc[, c(1:5,9)]
stemalloc$stemallocation <- with(stemalloc, (bole11+branch11)/cflux11)

##simple dataframe for stats and plots
alloc_C <- merge(stemalloc[,c(1:3,6:7)], leafalloc[, c(1:3,6:7)])
##order by treatment
alloc_C$treatment <- with(alloc_C, paste(CO2_treatment, Water_treatment, sep="-"))  
alloc_C <- alloc_C[order(alloc_C$treatment),]

##stats and regression lines
leafmod <- lm(leafallocation ~ cflux11, data = alloc_C)
summary(leafmod)

stemmod <- lm(stemallocation~ cflux11, data = alloc_C)
summary(stemmod)

##calculate partitioning and regressions-------------------------------------------------------------------------------------

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

smfmod <- lm(smf~ treeC, data = tree_C)
summary(smfmod)

rmfmod <- lm(rmf ~ treeC, data = tree_C)
summary(rmfmod)

###make big panel figures---------------------------------------------------------------------------------------------------

##1: combine dataframes

fracalloc <- merge(tree_C[,c(1, 9:15)],alloc_C)

##2: plot bits
palette(c("blue", "red"))

at.y2 <- seq(.3, by=.1, length.out=5)
at.x <- seq(5000, by=5000, length.out=4)

##3: plot

#par(cex.axis=1.21, cex.lab=1.51 ) 

# windows (7,10)
par(mfrow=c(3,2), las=1, mgp=c(3,1,0), oma=c(5,7,1,7))

#lmf
par(mar=c(0,0,0,0))
with(fracalloc, plot(lmf~treeC, ylab="", xaxt='n', xlab="", xlim=c(5000,22500), ylim=c(0,.375), type='n'))
ablineclip(lmfmod, x1=min(fracalloc$treeC), x2=max(fracalloc$treeC),lwd=2)
with(fracalloc, points(lmf~treeC, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5))
axis(1, labels=FALSE, tcl=.5)
mtext(lmflab, side=2, outer=TRUE, line=3.5, at=.85,las=3)
legend("topright", leglab2, pch=c(19,1, 19, 1), col=c("blue", "blue", "red", "red"), bty='n')
text(x=5250, y=0.365,labels="(a)", cex=1.5)

#leaf allocation
par(mar=c(0,0,0,0))
with(fracalloc, plot(leafallocation ~ cflux11, ylab="", axes=FALSE, xlab="", xlim=c(7500,27500), ylim=c(.0,.375), type='n'))
ablineclip(leafmod, x1=min(fracalloc$cflux11), x2=max(fracalloc$cflux11),lwd=2)
with(fracalloc, points(leafallocation ~ cflux11, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5))
mtexti(leafalloclab, 4, outer=TRUE, cex=1.5, off=.5)
axis(1, labels=FALSE, tcl=.5)
axis(4, labels=TRUE)
box()
text(x=7800, y=0.365,labels="(b)", cex=1.5)

#smf
par(mar=c(0,0,0,0))
with(fracalloc, plot(smf~treeC,  ylab="", xaxt='n', yaxt='n', xlab="",ylim=c(.25,.65), xlim=c(5000,22500),type='n'))
ablineclip(smfmod, x1=min(fracalloc$treeC), x2=max(fracalloc$treeC),lwd=2)
with(fracalloc, points(smf~treeC, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5))
mtext(smflab, side=2, outer=TRUE, line=3.5, at=.5,las=3)
axis(1, labels=FALSE, tcl=.5)
axis(2, at=at.y2 ,labels=TRUE)
text(x=5250, y=0.635,labels="(c)", cex=1.5)

#stem allocation
par(mar=c(0,0,0,0))
with(fracalloc, plot(stemallocation~ cflux11,  yaxt='n', ylab="", xlab="",ylim=c(.25,.65), xlim=c(7500,27500),type='n'))
ablineclip(stemmod, x1=min(fracalloc$cflux11), x2=max(fracalloc$cflux11),lwd=2)
with(fracalloc, points(stemallocation~ cflux11, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5))
mtext(treefluxlab2, side=1, outer=TRUE, line=-12.5, at=.75)
mtexti(stemalloclab, 4, outer=TRUE, cex=1.5, off=.5)
axis(4, labels=TRUE)
box()
text(x=7800, y=0.635,labels="(d)", cex=1.5)

#rmf
par(mar=c(0,0,0,0))
with(fracalloc, plot(rmf~treeC,  ylab="", xaxt='n',yaxt='n',xlab= "",ylim=c(.1, .3), xlim=c(5000,22500),type='n'))
ablineclip(rmfmod, x1=min(fracalloc$treeC), x2=max(fracalloc$treeC),lwd=2)
with(fracalloc, points(rmf~treeC, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5))
axis(2, labels=TRUE)
axis(1, at=at.x ,labels=TRUE, outer=TRUE)
#title(ylab=rmflab, mgp=c(4,1,0), outer=TRUE)

mtext(rmflab, side=2, outer=TRUE, line=3.5, at=.15, las=3)
mtext(treeclab, side=1, outer=TRUE, line=3.5, at=.25)
text(x=5250, y=0.29,labels="(e)", cex=1.5)

#root allocation
plot(1,1, type="n", axes=FALSE )

# dev.copy2pdf(file="master_scripts/paper_figs/allocfractions.pdf")
# dev.off()
