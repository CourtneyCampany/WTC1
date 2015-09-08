### This script creates TBCA from the flux data and the biomass data
source("functions_and_packages/functions.R")
source("functions_and_packages/plot_objects.R")
library(doBy)

#1: Fc,t = GPP - (Rl + Rs,br) = total carbon flux by chamber
#2: TBCA = Fc,t - Mab,t
#3: Mab,t = total mass of aboveground + leaflitter

# read harvest totals of mass and C flux by chmaber
#harvest_C <- read.csv("master_scripts/harvest_chamber.csv")
tree_C <- read.csv("calculated_mass/chamber_carbon.csv")
tree_C$treatment <- with(tree_C, paste(CO2_treatment, Water_treatment, sep="-"))  

##caluclate TBCA-----------------------------------------------------------------------------------------------------

tree_C$TBCA <- with(tree_C, Cflux- treeC)

tree_C$Fs_resid <- with(tree_C, TBCA - (frootC + crootC))

#write.csv(harvest_C[, c(1:2, 7:9, 15:16)], "calculated_mass/TBCA.csv", row.names = FALSE)

# boxplot(TBCA ~treatment, data=tree_C)

below_means <- summaryBy(Cflux+TBCA+Fs_resid ~ treatment, data=tree_C, FUN=c(mean, se))


##simple plotting of tbca and residual ---------------------------------------------------------------------------------

# bar(TBCA, c(treatment), tree_C, col="grey", xlab="",legend=FALSE, ylim=c(0, 15000),
#     half.errbar=FALSE, mar=c(5,5,1,1),las=1,cex.axis=.8, cex.lab=1, cex.names=1,mgp=c(3,1,0))
# 
# bar(Fs_resid, c(treatment), tree_C, col="grey", xlab="",legend=FALSE, ylim=c(0, 15000),
#     half.errbar=FALSE, mar=c(5,5,1,1),las=1,cex.axis=.8, cex.lab=1, cex.names=1,mgp=c(3,1,0))

###bar with both

##Create a two row matrix with tbca and Fs_resid
below_dat <- rbind(below_means$Cflux.mean,below_means$TBCA.mean, below_means$Fs_resid.mean)

se_dat <- c(below_means$Cflux.se,below_means$TBCA.se, below_means$Fs_resid.se)
means_dat <-  c(below_means$Cflux.mean,below_means$TBCA.mean, below_means$Fs_resid.mean)
##reorder
se_dat2 <- se_dat[c(1,5,9,2,6,10,3,7,11,4,8,12)]
means_dat2 <- means_dat[c(1,5,9,2,6,10,3,7,11,4,8,12)]

cols <- c("grey30", "grey55", "grey85")

###plot belowground flux

windows(7,7)
par(mar=c(5,6,1,1),las=1, mgp=c(3,1,0), cex.axis=1, cex.lab = 1.25)

below_bar <- barplot(below_dat, beside=TRUE, names.arg=below_means$treatment, ylim=c(0, 27500), col=cols, 
                     xaxt='n', ylab="")
arrows(below_bar, means_dat2, below_bar, means_dat2+se_dat2, length=0.1, angle=90)
arrows(below_bar, means_dat2, below_bar, means_dat2-se_dat2, length=0.1, angle=90)
box()
legend("topright",belowfluxlab, pch = 22,  bty='n', pt.bg=cols, cex=1.25)
axis(side=1, at=c(2.5, 6.5, 10.5, 14.5), labels=boxlab, padj=.75, cex.axis=1.25)
title(ylab="Carbon  (g)", mgp=c(4,1,0))

dev.copy2pdf(file="master_scripts/paper_figs/belowground_flux_plots.pdf")
dev.off() 


  
  