###stats for component from final harvest including boles, branch, leaves, fine roots and coarse roots
source("functions_and_packages/functions.R")
library(visreg)
library(nortest)
library(multcomp)
library(doBy)
library(RVAideMemoire)

##need final harvest values (not treatmen means)
#tree_C <- read.csv("master_scripts/harvest_chamber.csv")
tree_C <- read.csv("calculated_mass/chamber_carbon.csv")

##calculated component fractions
tree_C$lmf <- with(tree_C, (leafcarbon+littercarbon)/treeC)
tree_C$smf <- with(tree_C, (branchC+boleC)/treeC)
tree_C$rmf <- with(tree_C, (rootC)/treeC)

##calculate belowground flux
tree_C$tbca <- with(tree_C, Cflux-Cab)
tree_C$Fs_resid <- with(tree_C, Cflux - (Cab+rootC))

tree_C$treatment <- with(tree_C, as.factor(paste(CO2_treatment, Water_treatment, sep="-")))
tree_C$treatment <-  relevel(tree_C$treatment, ref="ambient-wet")

###analyze mass fractions.....individual components are then below

frac_trts <- summaryBy(lmf+smf+rmf ~ treatment, data=tree_C, FUN=c(mean, se))

#LMF----------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$lmf)
with(tree_C, boxplot(lmf~treatment))  

lmf_mod <- lm(lmf ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
anova(lmf_mod)

lmf_co2 <- lm(lmf ~ CO2_treatment, data=tree_C) 
anova(lmf_co2)
tukey_lmfco2<- glht(lmf_co2, linfct = mcp(CO2_treatment= "Tukey"))
lmfco2_siglets<- cld(tukey_lmfco2)
lmfco2_siglets2 <- lmfco2_siglets$mcletters$Letters

(mean(tree_C[tree_C$CO2_treatment=="elevated", "lmf"]) - mean(tree_C[tree_C$CO2_treatment=="ambient", "lmf"]))/mean(tree_C[tree_C$CO2_treatment=="elevated", "lmf"])
###lmf 15.2% higher in elevated CO2, no effect of h2o and no interation

lmf_h20 <- lm(lmf_co2 ~ Water_treatment, data=tree_C) 
anova(lmf_h20)
tukey_lmfh20 <- glht(lmf_h20, linfct = mcp(Water_treatment= "Tukey"))
lmfh20_siglets<- cld(tukey_lmfh20)
lmfh20_siglets2 <- lmfh20_siglets$mcletters$Letters  

##lmf vs co2 flux
fluxlmf_mod <- lm(lmf ~ Cflux, data=tree_C) 
summary(fluxlmf_mod)
anova(fluxlmf_mod)
# fluxlmf_mod2 <- lm(lmf ~ Cflux*CO2_treatment*Water_treatment , data=tree_C)
#  summary(fluxlmf_mod2)
#  anova(fluxlmf_mod2)


#SMF----------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$smf)
with(tree_C, boxplot(smf~treatment)) 

smf_mod <- lm(smf ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
anova(smf_mod)
summary(smf_mod)

smf_co2 <- lm(smf ~ CO2_treatment, data=tree_C) 
anova(smf_co2)
tukey_smfco2<- glht(smf_co2, linfct = mcp(CO2_treatment= "Tukey"))
smfco2_siglets<- cld(tukey_smfco2)
smfco2_siglets2 <- smfco2_siglets$mcletters$Letters

smf_h20 <- lm(smf ~ Water_treatment, data=tree_C) 
anova(smf_h20)
tukey_smfh20 <- glht(smf_h20, linfct = mcp(Water_treatment= "Tukey"))
smfh20_siglets<- cld(tukey_smfh20)
smfh20_siglets2 <- smfh20_siglets$mcletters$Letters  
##smf reduced by 8% in eco2, no interaction, no drought
(mean(tree_C[tree_C$CO2_treatment=="ambient", "smf"]) - mean(tree_C[tree_C$CO2_treatment=="elevated", "smf"]))/mean(tree_C[tree_C$CO2_treatment=="ambient", "smf"])


##smf vs co2 flux
fluxsmf_mod <- lm(smf ~ Cflux, data=tree_C) 
summary(fluxsmf_mod)
anova(fluxsmf_mod)
fluxsmf_mod2 <- lm(smf ~ Cflux*CO2_treatment*Water_treatment , data=tree_C)
summary(fluxsmf_mod2)
anova(fluxsmf_mod2)


(mean(tree_C[tree_C$CO2_treatment=="ambient", "smf"]) - mean(tree_C[tree_C$CO2_treatment=="elevated", "smf"]))/mean(tree_C[tree_C$CO2_treatment=="ambient", "smf"])

# RMF----------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$rmf)
with(tree_C, boxplot(rmf~treatment)) 

rmf_mod <- lm(rmf ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
anova(rmf_mod)
summary(rmf_mod)
visreg(rmf_mod)
visreg(rmf_mod, xvar="Water_treatment", by="CO2_treatment", overlay=TRUE)

##rmf vs co2 flux
fluxrmf_mod <- lm(rmf ~ Cflux, data=tree_C) 
summary(fluxrmf_mod)
anova(fluxrmf_mod)
fluxsmf_mod2 <- lm(smf ~ Cflux*CO2_treatment*Water_treatment , data=tree_C)
summary(fluxsmf_mod2)
anova(fluxsmf_mod2)
