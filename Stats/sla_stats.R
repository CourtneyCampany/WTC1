###SLA at harvest
source("functions_and_packages/functions.R")
library(doBy)
library(visreg)

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
