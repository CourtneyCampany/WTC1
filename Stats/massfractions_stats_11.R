###stats for mass partitioning using adjusted mass and flux values over final eleven months
source("functions_and_packages/functions.R")
library(visreg)
library(nortest)
library(multcomp)
library(RVAideMemoire)

##need final harvest values (not treatmen means)
tree_C <- read.csv("master_scripts/Cmassflux11.csv")

##calculated component fractions
tree_C$lmf <- with(tree_C, (leaf11+litter11)/treeC)
tree_C$lmf2 <- with(tree_C, (leaf11)/treeC)
tree_C$smf <- with(tree_C, (branch11+bole11)/treeC)
tree_C$rmf <- with(tree_C, (root11)/treeC)

tree_C$treatment <- with(tree_C, as.factor(paste(CO2_treatment, Water_treatment, sep="-")))
tree_C$treatment <-  relevel(tree_C$treatment, ref="ambient-wet")


#LMF----------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$lmf)
with(tree_C, boxplot(lmf~treatment))  

lmf_mod <- lm(lmf ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
anova(lmf_mod)
summary(lmf_mod)
plotresid(lmf_mod)


lmf_co2 <- lm(lmf ~ CO2_treatment, data=tree_C) 
anova(lmf_co2)

(mean(tree_C[tree_C$CO2_treatment=="elevated", "lmf"]) - mean(tree_C[tree_C$CO2_treatment=="ambient", "lmf"]))/mean(tree_C[tree_C$CO2_treatment=="elevated", "lmf"])
###lmf 24.2% higher in elevated CO2, no effect of h2o and no interation

lmf_h20 <- lm(lmf~ Water_treatment, data=tree_C) 
anova(lmf_h20)
  
#LMF2 (no cumulative litter)
lmf2_mod <- lm(lmf2 ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
anova(lmf2_mod)
summary(lmf2_mod)
plotresid(lmf2_mod)

(mean(tree_C[tree_C$CO2_treatment=="elevated", "lmf2"]) - mean(tree_C[tree_C$CO2_treatment=="ambient", "lmf2"]))/mean(tree_C[tree_C$CO2_treatment=="elevated", "lmf2"])


#SMF----------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$smf)
with(tree_C, boxplot(smf~treatment)) 

smf_mod <- lm(smf ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
anova(smf_mod)
summary(smf_mod)
plotresid(smf_mod)

smf_co2 <- lm(smf ~ CO2_treatment, data=tree_C) 
anova(smf_co2)

smf_h20 <- lm(smf ~ Water_treatment, data=tree_C) 
anova(smf_h20)
 
##smf reduced by 9% in eco2, no interaction, no drought
(mean(tree_C[tree_C$CO2_treatment=="ambient", "smf"]) - mean(tree_C[tree_C$CO2_treatment=="elevated", "smf"]))/mean(tree_C[tree_C$CO2_treatment=="ambient", "smf"])


# RMF----------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$rmf)
with(tree_C, boxplot(rmf~treatment)) 

rmf_mod <- lm(rmf ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
anova(rmf_mod)
summary(rmf_mod)
plotresid(rmf_mod)
visreg(rmf_mod)
visreg(rmf_mod, xvar="Water_treatment", by="CO2_treatment", overlay=TRUE)


