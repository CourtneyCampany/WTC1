###stats for fluxes and leaf area from final harvest
library(visreg)
library(nortest)
library(multcomp)
library(RVAideMemoire)

##need final harvest values (not treatmen means)
tree_C <- read.csv("master_scripts/harvest_chamber.csv")

##need TBCA to add to roots
tbca <- read.csv("calculated_mass/TBCA.csv")

tree_C <- merge(tree_C, tbca)
tree_C$treatment <-  relevel(tree_C$treatment, ref="ambient-wet")


###component stats (test co2, water and then both)

#lm for linear, nlme for not

##1. co2 uptake------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$CO2cum) ##appears normal
with(tree_C, boxplot(CO2cum~treatment))

co2_mod <- lm(CO2cum ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)

anova(co2_mod)
summary(co2_mod)
plotresid(co2_mod)
visreg(co2_mod)

##no interactions so run simple mod for each treatment

co2_co2 <- lm(CO2cum ~ CO2_treatment, data=tree_C) 
  anova(co2_co2)
  tukey_co2co2<- glht(co2_co2, linfct = mcp(CO2_treatment= "Tukey"))
  co2co2_siglets<- cld(tukey_co2co2)
  co2co2_siglets2 <- co2co2_siglets$mcletters$Letters
  ## reductions in cumulative co2 uptake with Eco2 by 30.6%

(mean(tree_C[tree_C$CO2_treatment=="ambient", "CO2cum"]) - mean(tree_C[tree_C$CO2_treatment=="elevated", "CO2cum"]))/mean(tree_C[tree_C$CO2_treatment=="ambient", "CO2cum"])

CO2_h20 <- lm(CO2cum ~ Water_treatment, data=tree_C) 
  anova(CO2_h20)
  visreg(CO2_h20)
  ## no effect of drought treatment on total CO2 uptake
  
  
  
##2. tbca------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$TBCA) ##appears normal
with(tree_C, boxplot(TBCA~treatment))
  
TBCA_mod <- lm(TBCA ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
  anova(TBCA_mod)
  summary(TBCA_mod)
  plotresid(TBCA_mod)
  visreg(TBCA_mod)
  ##no effect of treatments on TBCA
  
tbca_co2 <- lm(TBCA ~ CO2_treatment, data=tree_C) 
  anova(tbca_co2)

tbca_h20 <- lm(TBCA ~ Water_treatment, data=tree_C) 
  anova(tbca_h20)

  
##### from previos stats that show some effects of roots with treatments the no effect of TBCA means ther3
##### is likely an affect of Fs,resdiual that may oppose what we find with roots to then equal no effect
  
##3. Fs,residual------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$Fs_resid) ##appears normal
with(tree_C, boxplot(Fs_resid~treatment))

soilC_mod <- lm(Fs_resid ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
anova(soilC_mod)
summary(soilC_mod)
plotresid(soilC_mod)
visreg(soilC_mod)


###but nothing......



  