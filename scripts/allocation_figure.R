###calculate allocation to compare vs partitioning
source("functions_and_packages/plot_objects.R")
source("functions_and_packages/functions.R")
library(plotrix)

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


###make similar figures to mass fractions----------------------------------------------------------------------------------- 
palette(c("blue", "red"))

windows(7,7)

par(cex.axis=.9, cex.lab=1, las=1,mgp=c(3,1,0),mfrow=c(2,1), oma=c(5, 0, 2,0))  

#leaf allocation
par(mar=c(0,7,0,2))
with(alloc_C, plot(leafallocation ~ cflux11, ylab="", xaxt='n', xlab="", xlim=c(7500,27500), ylim=c(.0,.275), type='n'))
ablineclip(leafmod, x1=min(alloc_C$cflux11), x2=max(alloc_C$cflux11),lwd=2)
with(alloc_C, points(leafallocation ~ cflux11, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5))
title(ylab=leafalloclab, mgp=c(4,1,0))
legend("topleft", leglab2, pch=c(19,1, 19, 1), col=c("blue", "blue", "red", "red"), inset = 0.01,bty='n')
text(x=27500, y=0.275,labels="(a)", cex=1)

#stem allocation
par(mar=c(0,7,0,2))
with(alloc_C, plot(stemallocation~ cflux11,  ylab="", xlab="",ylim=c(.25,.5), xlim=c(7500,27500),type='n'))
ablineclip(stemmod, x1=min(alloc_C$cflux11), x2=max(alloc_C$cflux11),lwd=2)
with(alloc_C, points(stemallocation~ cflux11, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5))
title(ylab=stemalloclab, mgp=c(4,1,0))
text(x=27500, y=0.5,labels="(b)", cex=1)


# dev.copy2pdf(file="master_scripts/paper_figs/c_allocation.pdf")
# dev.off()
