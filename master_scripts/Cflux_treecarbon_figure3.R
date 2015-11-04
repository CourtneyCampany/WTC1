 # source("functions_and_packages/plot_objects.R")

###need to compare Cflux and mass from same date generation
Cflux <- read.csv("calculated_mass/chamber_C_flux.csv")
Cflux$Date <- as.Date(Cflux$Date)
chamdate <- min(Cflux$Date)

###harvest data
harvest <- read.csv("calculated_mass/chamber_carbon.csv") 
  ##this data frame needs to be adjust to compare with Cflux

##read in allometry interpolated mass and find mass at 'chamdate' that we can compare to chamber flux

###C mass interpolated
allomC <- read.csv("whole_tree_csv/tree_C_flux.csv")
allomC$Date <- as.Date(allomC$Date)

allomfirst <- allomC[allomC$Date == "2008-04-15",c(1:2, 8:12)]
#these starting values will need to be removed from harvest mass

##ROOTS: will have to use R:S relationship to predict root from starting value R:S ratio
rooteq <- read.csv("stats/rootshootmodel.csv")

###to be correct do it with mass pred instead of C and double check results (rooteq based off mass not C)
allomM <- read.csv("whole_tree_csv/tree_mass_Cflux.csv")
allomM$Date <- as.Date(allomM$Date)
massfirst <- allomM[allomM$Date == "2008-04-15",c(1:2, 8:12)]
massfirst$aboveM <- with(massfirst, branch_start+leaf_start+litter_start+bole_start)
massfirst$rootmass_pred <- with(massfirst, rooteq[2,1]*(log10(aboveM)) + rooteq[1,1])
massfirst$rootmass_pred2 <- 10^(massfirst$rootmass_pred)
massfirst$root_start <- massfirst$rootmass_pred2 * .5

##add predicted root C mass back to allomc C dfr
allomstart <- merge(allomfirst, massfirst[, c(1, 11)])

###know we need to subtract start mass from harvest mass b4 we can compare with Cflux
harvest_corr <- merge(harvest, allomstart)

##calculate mass during 11 month period
harvest_corr$bole11 <- with(harvest_corr, boleC-bole_start)
harvest_corr$branch11 <- with(harvest_corr, branchC-branch_start)
harvest_corr$leaf11 <- with(harvest_corr, leafcarbon-leaf_start)
harvest_corr$litter11 <- with(harvest_corr, littercarbon-litter_start)
harvest_corr$root11 <- with(harvest_corr, rootC-root_start)

harvest_corr$cflux11 <- with(harvest_corr, Cflux-CO2start)

###new dataframe with only corrected mass
treeC11 <- harvest_corr[, c(1, 10:11, 19:24)]
treeC11$treeC <- with(treeC11, bole11+branch11+root11+leaf11+litter11)
treeC11$Cab <- with(treeC11, bole11+branch11+leaf11+litter11)

#write.csv(treeC11, "master_scripts/Cmassflux11.csv", row.names = FALSE)

###now compare these (stats and figures) to CO2cham flux over same date---------------------------------------------------------

#linear model to show fit on graph
tree_mod <- lm(treeC ~ cflux11, data = treeC11)
# summary(tree_mod)
#anova(tree_mod)


##plot with total flux---------------------------------------------------------------------------------------------------------
palette (c("blue", "red"))

library(plotrix)

# windows (7,7)
par(mar=c(5,6,1,1),las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0))

plot(1,type='n',xlim = c(0, 30000),ylim = c(0, 30000), ylab="", xlab=treefluxlab2)
legend("topleft", leglab2, pch=c(19,1, 19, 1), col=c("blue", "blue", "red", "red"), inset = 0.01, bty='n')
box()
ablineclip(tree_mod, x1=min(treeC11$cflux11),x2=max(treeC11$cflux11),lwd=2)
abline(0, 1, lty=3, lwd=2)
points(treeC ~ cflux11, data = treeC11,pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5)
title(ylab=treeclab, mgp=c(4,1,0))

# dev.copy2pdf(file= "master_scripts/paper_figs/flux_treecarbon3.pdf")
# dev.off()





