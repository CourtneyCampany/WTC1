###stats correlations between the variables plotted in flux, leafarea, tbca

library(lme4)
library(lmerTest)
library(effects)
library(LMERConvenienceFunctions)
source("functions_and_packages/rsquared_glmm.R")
#runthe new r2 function to get r2 and p values for the model
library(doBy)
library(RVAideMemoire)
library(visreg)
library(nortest)

## treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

##read in and format Leaf Area
leafarea <- read.csv("raw csv/HFE LA estimates alldates.csv")
  leafarea$Date <- as.Date(leafarea$Date)
  leafarea <- merge(leafarea, chambersumm)
  ##order by date
  leafarea <- leafarea[order(leafarea$Date),]
  ##add unique treamtent
  leafarea$treatment <- with(leafarea, paste(CO2_treatment, Water_treatment, sep="-"))

##read in chamber flux and calculate TBCA
treeflux <- read.csv("calculated_mass/chamber_C_flux.csv")
  treeflux$Date <- as.Date(treeflux$Date)


##merge flux data and leaf area 
flux_la <- merge(leafarea, treeflux, all=F)

flux_final <- flux_la[flux_la$Date == max(flux_la$Date),]

fluxla_mod <- lm(CO2cum ~ LAestlin, data=flux_final)
  summary(fluxla_mod)
  plotresid(fluxla_mod)

visreg(fluxla_mod)

fluxla_mod2 <- lm(CO2cum ~ LAestlin*CO2_treatment*Water_treatment , data=flux_final)
  summary(fluxla_mod2)

  

