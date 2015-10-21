###manuscript figure with both allocation and partitioning
source("functions_and_packages/plot_objects.R")
source("functions_and_packages/functions.R")
library(visreg)
library(nortest)
library(multcomp)
library(doBy)
library(RVAideMemoire)

##calculate allocation and regressions-------------------------------------------------------------------------------------

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
alloc_C$treatment <- with(alloc_C, as.factor(paste(CO2_treatment, Water_treatment, sep="-")))
alloc_C$treatment <-  relevel(alloc_C$treatment, ref="ambient-wet")

##stats and regression lines-----------------------------------------------------------------------------------

##leaf C allocation

ad.test(alloc_C$leafallocation)
with(alloc_C, boxplot(leafallocation~treatment))  

leafmod <- lm(leafallocation ~ cflux11, data = alloc_C)
anova(leafmod)
summary(leafmod)

leafmod2 <- lm(leafallocation ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=alloc_C)
anova(leafmod2)
summary(leafmod2)

leafmod3 <- glm(leafallocation ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, family = quasibinomial(logit),
                data = alloc_C)
summary(leafmod3)
anova(leafmod3)

(mean(alloc_C[alloc_C$CO2_treatment=="elevated", "leafallocation"]) - mean(alloc_C[alloc_C$CO2_treatment=="ambient", "leafallocation"]))/mean(alloc_C[alloc_C$CO2_treatment=="elevated", "leafallocation"])
###lmf 28.2% higher in elevated CO2, no effect of h2o and no interation

leafmod_co2 <- lm(leafallocation ~ CO2_treatment, data=alloc_C) 
anova(leafmod_co2)

leafmod_h20 <- lm(leafallocation ~ Water_treatment, data=alloc_C) 
anova(leafmod_h20)
  

####Stem C allocation
ad.test(alloc_C$stemallocation)
with(alloc_C, boxplot(stemallocation~treatment))  

stemmod <- lm(stemallocation ~ cflux11, data = alloc_C)
summary(stemmod)

stemmod2 <- lm(stemallocation ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=alloc_C)
anova(stemmod2)
summary(stemmod2)
