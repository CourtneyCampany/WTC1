### This script creates TBCA from the flux data and the biomass data
# source("functions_and_packages/functions.R")
# source("functions_and_packages/plot_objects.R")
# library(doBy)

#1: Fc,t = GPP - (Rl + Rs,br) = total carbon flux by chamber
#2: TBCA = Fc,t - Mab,t
#3: Mab,t = total mass of aboveground + leaflitter

# read harvest totals of mass and C flux by chmaber
#harvest_C <- read.csv("master_scripts/harvest_chamber.csv")

tree_C <- read.csv("master_scripts/Cmassflux11.csv")
  tree_C$treatment <- with(tree_C, paste(CO2_treatment, Water_treatment, sep="-"))  
  ##caluclate TBCA and residual flux
  tree_C$TBCA <- with(tree_C, cflux11- Cab)
  tree_C$Fs_resid <- with(tree_C, cflux11 - treeC)
#write.csv(tree_C[, c(1:3, 13:14, 11:12)], "calculated_mass/TBCA.csv", row.names = FALSE)
  
rootmeans <- summaryBy(root11 ~ treatment, data=tree_C, FUN=c(mean, se))

test <- read.csv("calculated_mass/rootallometry.csv")

# boxplot(TBCA ~treatment, data=tree_C)
below_means <- summaryBy(cflux11+TBCA+Fs_resid ~ treatment, data=tree_C, FUN=c(mean, se))

##Create a two row matrix with tbca and Fs_resid
below_dat <- rbind(below_means$cflux11.mean,below_means$TBCA.mean, below_means$Fs_resid.mean)

se_dat <- c(below_means$cflux11.se,below_means$TBCA.se, below_means$Fs_resid.se)
means_dat <-  c(below_means$cflux11.mean,below_means$TBCA.mean, below_means$Fs_resid.mean)
##reorder
se_dat2 <- se_dat[c(1,5,9,2,6,10,3,7,11,4,8,12)]
means_dat2 <- means_dat[c(1,5,9,2,6,10,3,7,11,4,8,12)]

cols <- c("grey30", "grey55", "grey85")

###plot belowground flux

# windows(7,7)
par(mar=c(5,6,1,1),las=1, mgp=c(3,1,0), cex.axis=1, cex.lab = 1.25)

below_bar <- barplot(below_dat, beside=TRUE, names.arg=below_means$treatment, ylim=c(0, 27500), col=cols, 
                     xaxt='n', ylab="")
arrows(below_bar, means_dat2, below_bar, means_dat2+se_dat2, length=0.1, angle=90)
arrows(below_bar, means_dat2, below_bar, means_dat2-se_dat2, length=0.1, angle=90)
box()
legend("topright",belowfluxlab, pch = 22,  bty='n', pt.bg=cols, cex=1.25)
axis(side=1, at=c(2.5, 6.5, 10.5, 14.5), labels=boxlab, padj=.75, cex.axis=1.25)
title(ylab="Carbon  (g)", mgp=c(4,1,0))

# dev.copy2pdf(file="master_scripts/paper_figs/belowground_flux_plots2.pdf")
# dev.off() 


  
  