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
  treeC$Date <- as.Date(treeC$Date)


##merge flux data and leaf area 
flux_la <- merge(leafarea, treeflux, all=F)

flux_final <- flux_la[flux_la$Date == max(flux_la$Date),]

fluxla_mod <- lm(CO2cum ~ LAestlin, data=flux_final)
  summary(fluxla_mod)
  plotresid(fluxla_mod)

visreg(fluxla_mod)

fluxla_mod2 <- lm(CO2cum ~ LAestlin+CO2_treatment+Water_treatment+LAestlin:CO2_treatment:Water_treatment , data=flux_final)
  summary(fluxla_mod2)

###i dont think there is any interaction with the relationship between leafarea and co2 flux


###tbca with CO2 flux and leaf area
##need final harvest values (not treatmen means)
tree_C <- read.csv("master_scripts/harvest_chamber.csv")

##need TBCA to add to roots
tbca <- read.csv("calculated_mass/TBCA.csv")

tree_C <- merge(tree_C, tbca)
  tree_C$treatment <-  relevel(tree_C$treatment, ref="ambient-wet")
#merge with fluxla
tree_final <- merge(tree_C[,c(1, 3:5,15:16)], flux_final)

###tbca and leaf area
tbcala_mod <- lm(TBCA ~ LAestlin, data=tree_final)
  summary(tbcala_mod)
  anova(tbcala_mod)
  plotresid(tbcala_mod)
  visreg(tbcala_mod)
###apparently not related
  
  
  
###tbca and co2 flux
tbcaflux_mod <- lm(TBCA ~ CO2cum, data=tree_final)
  summary(tbcaflux_mod)
  anova(tbcaflux_mod)
  plotresid(tbcaflux_mod)
  visreg(tbcaflux_mod)  
  
  
##yes so look at treatments
tbcaflux_mod2 <- lm(TBCA ~ CO2cum+CO2_treatment+Water_treatment+CO2cum:CO2_treatment:Water_treatment 
                      , data=tree_final)
  summary(tbcaflux_mod2)
  
##apparently no treatment effects on the model
  