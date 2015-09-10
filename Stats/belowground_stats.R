###tbca with CO2 flux
library(RVAideMemoire)
library(visreg)
library(nortest)
library(doBy)

## treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

##need TBCA to add to roots
tree_C <- read.csv("calculated_mass/chamber_carbon.csv")
  tree_C$treatment <- with(tree_C, as.factor(paste(CO2_treatment, Water_treatment, sep="-")))
  tree_C$treatment <-  relevel(tree_C$treatment, ref="ambient-wet")
  tree_C$TBCA <- with(tree_C, Cflux- Cab)
  tree_C$Fs_resid <- with(tree_C, Cflux - treeC)

##leaves
leafarea <- read.csv("raw csv/HFE LA estimates alldates.csv")
  leafarea$Date <- as.Date(leafarea$Date)

###plot needs to have mean leaf area over all dates on yaxis  
la_agg <- summaryBy(LAestlin ~ chamber, data=leafarea, FUN=mean)
  la_agg <- merge(la_agg, chambersumm)


belowflux <- merge(tree_C, la_agg[, c(1,2)])

####stats on TBCA, Fsr and then correlations

##Fsr---------------------------------------------------------------------------------------------------------------------
ad.test(belowflux$Fs_resid)
with(belowflux, boxplot(Fs_resid~treatment))  

resid_mod <- lm(Fs_resid ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=belowflux)
anova(resid_mod)
summary(resid_mod)
plotresid(resid_mod)

##TBCA--------------------------------------------------------------------------------------------------------------------
ad.test(belowflux$TBCA)
with(belowflux, boxplot(TBCA~treatment))  

tbca_mod <- lm(TBCA ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=belowflux)
anova(tbca_mod)
summary(tbca_mod)
plotresid(tbca_mod)

###correlations------------------------------------------------------------------------------------------------------------

###tbca and flux
fluxtbca_mod <- lm(TBCA~ Cflux, data=belowflux) 
summary(fluxtbca_mod)
anova(fluxtbca_mod)
visreg(fluxtbca_mod)
# fluxTBCA_mod2 <- lm(TBCA ~ CO2cum*CO2_treatment*Water_treatment , data=belowflux)
#  summary(fluxTBCA_mod2)
#  anova(fluxTBCA_mod2)

###Fsresid and flux
fluxresid_mod <- lm(Fs_resid~ Cflux, data=belowflux) 
summary(fluxresid_mod)
anova(fluxresid_mod)
visreg(fluxresid_mod)
# fluxTBCA_mod2 <- lm(TBCA ~ CO2cum*CO2_treatment*Water_treatment , data=belowflux)
#  summary(fluxTBCA_mod2)
#  anova(fluxTBCA_mod2)

###tbca and la
latbca_mod <- lm(TBCA~ LAestlin.mean, data=belowflux) 
summary(latbca_mod)
anova(latbca_mod)
plotresid(latbca_mod)
visreg(latbca_mod)
##weak postive correlation

lafsr_mod <- lm(Fs_resid~ LAestlin.mean, data=belowflux) 
summary(lafsr_mod)
anova(lafsr_mod)
plotresid(lafsr_mod)
visreg(lafsr_mod)
##no correlation

