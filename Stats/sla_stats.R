###SLA at harvest
source("functions_and_packages/functions.R")
library(visreg)
library(nortest)
library(multcomp)
library(doBy)
library(RVAideMemoire)

harvest <- read.csv("raw csv/HFE final harvest biomass by layer.csv")

# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

harvest2 <- merge(harvest, chambersumm)

sla_mod <- lm(SLA ~ layerno, data=harvest2)
summary(sla_mod)

sla_mod2 <- lm(SLA ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=harvest2)
summary(sla_mod2)

sla_mod3 <- lm(SLA ~ CO2_treatment, data=harvest2)
summary(sla_mod3)
visreg(sla_mod3)


sla_wtc <- summaryBy(SLA~ chamber, data=harvest2, FUN=c(mean,se))
sla_wtc2 <- merge(sla_wtc, chambersumm)

plot(SLA.mean ~ CO2_treatment, data=sla_wtc2)



####recalculate with weighted mean

sla_sp <- split(harvest2, harvest2$chamber)

sla_mean <- sapply(sla_sp, function(x) weighted.mean(x$SLA, w = x$layerno))
sla_wm <- data.frame(chamber=names(sla_mean), sla_wm=as.vector(sla_mean) )  

##rerun model for CO2 effects on SLA 
sla_cham <- merge(sla_wm, chambersumm)

#normality
ad.test(sla_cham$sla_wm)

###run model with weighted mean of sla by chamberlayerno
slawm_mod <- lm(sla_wm ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=sla_cham)
plotresid(slawm_mod)
visreg(slawm_mod)
anova(slawm_mod)
summary(slawm_mod)

##CO2 effect 0.053 (11%)
(mean(sla_cham[sla_cham$CO2_treatment=="ambient", "sla_wm"]) - mean(sla_cham[sla_cham$CO2_treatment=="elevated", "sla_wm"]))/mean(sla_cham[sla_cham$CO2_treatment=="ambient", "sla_wm"])
slawm_agg <- summaryBy(sla_wm ~ CO2_treatment, data=sla_cham, FUN=mean)

