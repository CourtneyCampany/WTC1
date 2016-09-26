###manuscript figure with both allocation and partitioning
source("functions_and_packages/plot_objects.R")
source("functions_and_packages/functions.R")
library(plotrix)

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

tree_C$standingtreemass <- with(tree_C, treeC-littercarbon)
##calculated component fractions
tree_C$lmf <- with(tree_C, leafcarbon/standingtreemass)
tree_C$smf <- with(tree_C, (boleC+branchC)/standingtreemass)
tree_C$rmf <- with(tree_C, (rootC)/standingtreemass)
##order by treatment
tree_C$treatment <- with(tree_C, paste(CO2_treatment, Water_treatment, sep="-"))  
tree_C <- tree_C[order(tree_C$treatment),]

#simple mode for abline and sig
lmfmod <- lm(lmf ~ standingtreemass, data = tree_C)
summary(lmfmod)

smfmod <- lm(smf~ standingtreemass, data = tree_C)
summary(smfmod)

rmfmod <- lm(rmf ~ standingtreemass, data = tree_C)
summary(rmfmod)


fracalloc <- merge(tree_C[,c(1, 10:16)],alloc_C)

palette(c("blue", "red"))
at.y2 <- seq(.3, by=.1, length.out=5)
at.yleaf <- seq(0, by=.1, length.out = 4)
at.x <- seq(5000, by=5000, length.out=4)

#panel of massfractions
png(filename = "makepngs/massfrac.png", width = 11, height = 8.5, units = "in", res= 400)
par(mfrow=c(3,1), las=1, mgp=c(3,1,0), oma=c(5,6,1,1),cex.axis=1.25, cex.lab=1.75)

#lmf
par(mar=c(0,0,0,0))
with(fracalloc, plot(lmf~standingtreemass, ylab="", axes=FALSE,xlab="", xlim=c(5000,21000), ylim=c(0,.325), type='n'))
ablineclip(lmfmod, x1=min(fracalloc$standingtreemass), x2=max(fracalloc$standingtreemass),lwd=2)
with(fracalloc, points(lmf~standingtreemass, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=2))
axis(1, labels=FALSE, tcl=.5)
axis(2, at=at.yleaf ,labels=TRUE)
mtext(lmflab, side=2, outer=TRUE, line=3.5, at=.85,las=3,cex=1.25)
legend("topright", leglab2, pch=c(19,1, 19, 1), col=c("blue", "blue", "red", "red"), bty='n', cex=1.5)
box()

#smf
par(mar=c(0,0,0,0))
with(fracalloc, plot(smf~standingtreemass,  ylab="", xaxt='n', yaxt='n', xlab="",ylim=c(.4,.7), xlim=c(5000,21000),type='n'))
ablineclip(smfmod, x1=min(fracalloc$standingtreemass), x2=max(fracalloc$standingtreemass),lwd=2)
with(fracalloc, points(smf~standingtreemass, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=2))
mtext(smflab, side=2, outer=TRUE, line=3.5, at=.5,las=3,cex=1.25)
axis(1, labels=FALSE, tcl=.5)
axis(2, at=at.y2 ,labels=TRUE)


#rmf
par(mar=c(0,0,0,0))
with(fracalloc, plot(rmf~standingtreemass,  ylab="", xaxt='n',yaxt='n',xlab= "",ylim=c(.1, .3), xlim=c(5000,21000),type='n'))
ablineclip(rmfmod, x1=min(fracalloc$standingtreemass), x2=max(fracalloc$standingtreemass),lwd=2)
with(fracalloc, points(rmf~standingtreemass, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=2))
axis(2, labels=TRUE)
axis(1, at=at.x ,labels=TRUE, outer=TRUE)
mtext(rmflab, side=2, outer=TRUE, line=3.5, at=.15, las=3,cex=1.25)
mtext(treefluxlab2, side=1, outer=TRUE, line=3, at=.5, cex=1.25)

dev.off()

#panel of allocation----------------------------------------------------------------------------------
png(filename = "makepngs/allocation.png", width = 11, height = 8.5, units = "in", res= 400)
par(mfrow=c(2,1), las=1, mgp=c(3,1,0), oma=c(5,6,1,1),cex.axis=1.25, cex.lab=1.75)

#leaf allocation
par(mar=c(0,0,0,0))
with(fracalloc, plot(leafallocation ~ cflux11, ylab="", axes=FALSE, xlab="", xlim=c(7500,27500), ylim=c(0,.3), type='n'))
ablineclip(leafmod, x1=min(fracalloc$cflux11), x2=max(fracalloc$cflux11),lwd=2)
with(fracalloc, points(leafallocation ~ cflux11, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=2))
mtext(leafalloclab, side=2, outer=TRUE, line=3.5, at=.75,las=3, cex=1.75)
axis(1, labels=FALSE, tcl=.5)
axis(2, at=at.yleaf,labels=TRUE)
box()
legend("topright", leglab2, pch=c(19,1, 19, 1), col=c("blue", "blue", "red", "red"), bty='n', cex=1.25)


#stem allocation
par(mar=c(0,0,0,0))
with(fracalloc, plot(stemallocation~ cflux11,  yaxt='n', ylab="", xlab="",ylim=c(.25,.5), xlim=c(7500,27500),type='n'))
ablineclip(stemmod, x1=min(fracalloc$cflux11), x2=max(fracalloc$cflux11),lwd=2)
with(fracalloc, points(stemallocation~ cflux11, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=2))
mtext(treefluxlab2, side=1, outer=TRUE, line=3, at=.5, cex=1.75)
mtext(stemalloclab, side=2, outer=TRUE, cex=1.75, at=.25, las=3, line=3.5)
axis(2, labels=TRUE)
box()

dev.off()



mtext(treeclab, side=1, outer=TRUE, line=3.5, at=.25)


