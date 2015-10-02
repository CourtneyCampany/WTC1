
###analyzed mass fractions to Cflux over last 11 months, corrected

source("functions_and_packages/plot_objects.R")
source("functions_and_packages/functions.R")
library(plotrix)

tree_C <- read.csv("master_scripts/Cmassflux11.csv")

##calculated component fractions
  tree_C$lmf <- with(tree_C, (leaf11+litter11)/treeC)
  tree_C$smf <- with(tree_C, (bole11+branch11)/treeC)
  tree_C$rmf <- with(tree_C, (root11)/treeC)

##order by treatment
tree_C$treatment <- with(tree_C, paste(CO2_treatment, Water_treatment, sep="-"))  
tree_C <- tree_C[order(tree_C$treatment),]

#simple mode for abline and sig
lmfmod <- lm(lmf ~ cflux11, data = tree_C)
summary(lmfmod)

smfmod <- lm(smf~ cflux11, data = tree_C)
summary(smfmod)

rmfmod <- lm(rmf ~ cflux11, data = tree_C)
summary(rmfmod)

plot(lmf ~ cflux11, data = tree_C, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5, ylim=c(.1, .4))
plot(smf ~ cflux11, data = tree_C, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5, ylim=c(.5, .8))
plot(rmf ~ cflux11, data = tree_C, pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5, ylim=c(.0, .25))

###lmf negatively related to treeC flux, other unaffected
