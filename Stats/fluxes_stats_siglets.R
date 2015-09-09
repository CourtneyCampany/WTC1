###stats for fluxes and leaf area from final harvest
library(visreg)
library(nortest)
library(multcomp)
library(RVAideMemoire)

##need final harvest values (not treatmen means)
tree_C <- read.csv("calculated_mass/chamber_carbon.csv")
tree_C$TBCA <- with(tree_C, Cflux- Cab)
tree_C$Fs_resid <- with(tree_C, Cflux - treeC)
tree_C$treatment <- with(tree_C, as.factor(paste(CO2_treatment, Water_treatment, sep="-")))
tree_C$treatment <-  relevel(tree_C$treatment, ref="ambient-wet")


###total tree----------------------------------------------------------------------------------------------------------------
ad.test(tree_C$treeC) ##appears normal
with(tree_C, boxplot(treeC~treatment))

tree_mod <- lm(treeC ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
anova(tree_mod)
summary(tree_mod)
plotresid(tree_mod)
visreg(tree_mod)
       
(mean(tree_C[tree_C$CO2_treatment=="ambient", "treeC"]) - mean(tree_C[tree_C$CO2_treatment=="elevated", "treeC"]))/mean(tree_C[tree_C$CO2_treatment=="ambient", "treeC"])

###M,ab----------------------------------------------------------------------------------------------------------------
ad.test(tree_C$Cab) ##appears normal
with(tree_C, boxplot(Cab~treatment))

Cab_mod <- lm(Cab ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
anova(Cab_mod)
summary(Cab_mod)
plotresid(Cab_mod)
visreg(Cab_mod)

(mean(tree_C[tree_C$CO2_treatment=="ambient", "Cab"]) - mean(tree_C[tree_C$CO2_treatment=="elevated", "Cab"]))/mean(tree_C[tree_C$CO2_treatment=="ambient", "Cab"])


##co2 uptake------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$Cflux) ##appears normal
with(tree_C, boxplot(Cflux~treatment))

co2_mod <- lm(Cflux ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)

anova(co2_mod)
summary(co2_mod)
plotresid(co2_mod)
visreg(co2_mod)

##no interactions so run simple mod for each treatment

co2_co2 <- lm(Cflux ~ CO2_treatment, data=tree_C) 
  anova(co2_co2)
  tukey_co2co2<- glht(co2_co2, linfct = mcp(CO2_treatment= "Tukey"))
  co2co2_siglets<- cld(tukey_co2co2)
  co2co2_siglets2 <- co2co2_siglets$mcletters$Letters
  ## reductions in cumulative co2 uptake with Eco2 by 30.6%

(mean(tree_C[tree_C$CO2_treatment=="ambient", "Cflux"]) - mean(tree_C[tree_C$CO2_treatment=="elevated", "Cflux"]))/mean(tree_C[tree_C$CO2_treatment=="ambient", "Cflux"])

CO2_h20 <- lm(Cflux ~ Water_treatment, data=tree_C) 
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



  