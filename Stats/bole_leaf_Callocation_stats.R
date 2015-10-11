###C allocation stats
library(visreg)
library(nortest)
library(multcomp)
library(doBy)
library(RVAideMemoire)

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


##stats and regression lines---------------------------------------------------------------------------------------------
ad.test(alloc_C$stemallocation) ##appears normal


stem_mod <- lm(stemallocation ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=alloc_C)
anova(stem_mod)
summary(stem_mod)
plotresid(stem_mod)
visreg(stem_mod)

#main effects
stem_water<- lm(stemallocation ~ Water_treatment, data=alloc_C) 
anova(stem_water)
visreg(stem_water)

stem_co2<- lm(stemallocation ~ CO2_treatment, data=alloc_C) 
anova(stem_co2)
visreg(stem_co2)


###allocation higher in elevated CO2 but not different


ad.test(alloc_C$leafallocation) ##appears normal

leaf_mod <- lm(leafallocation ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=alloc_C)
anova(leaf_mod)
summary(leaf_mod)
plotresid(leaf_mod)
visreg(leaf_mod)


leaf_co2 <- lm(leafallocation ~ Water_treatment, data=alloc_C) 
anova(leaf_co2)
visreg(leaf_co2)

leaf_water<- lm(leafallocation ~ Water_treatment, data=alloc_C) 
anova(leaf_water)
visreg(leaf_water)

tukey_leafco2<- glht(leaf_co2, linfct = mcp(CO2_treatment= "Tukey"))
leafco2_siglets<- cld(tukey_leafco2)
leafco2_siglets2 <- leafco2_siglets$mcletters$Letters


###allocation higher in elevated CO2 but not different
#leaf allocation higher eCO2 no change with drought


