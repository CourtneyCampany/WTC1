###tbca with CO2 flux
tree_C <- read.csv("master_scripts/harvest_chamber.csv")

library(RVAideMemoire)
library(visreg)
library(nortest)

##need TBCA to add to roots
tbca <- read.csv("calculated_mass/TBCA.csv")

##leaves
leafarea <- read.csv("calculated_mass/leafarea_final.csv")


belowflux <- merge(tree_C[,c(1, 11)], tbca)
belowflux <- merge(belowflux, leafarea[, c(1,7)])

####stats on TBCA, Fsr and then correlations

##Fsr---------------------------------------------------------------------------------------------------------------------
ad.test(belowflux$Fs_resid)
with(belowflux, boxplot(Fs_resid~treatment))  

resid_mod <- lm(Fs_resid ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=belowflux)
anova(resid_mod)


##TBCA--------------------------------------------------------------------------------------------------------------------
ad.test(belowflux$TBCA)
with(belowflux, boxplot(TBCA~treatment))  

tbca_mod <- lm(TBCA ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=belowflux)
anova(tbca_mod)

###correlations------------------------------------------------------------------------------------------------------------

###tbca and flux
fluxtbca_mod <- lm(TBCA~ CO2cum, data=belowflux) 
summary(fluxtbca_mod)
anova(fluxtbca_mod)
# fluxTBCA_mod2 <- lm(TBCA ~ CO2cum*CO2_treatment*Water_treatment , data=belowflux)
#  summary(fluxTBCA_mod2)
#  anova(fluxTBCA_mod2)

###Fsresid and flux
fluxresid_mod <- lm(Fs_resid~ CO2cum, data=belowflux) 
summary(fluxresid_mod)
anova(fluxresid_mod)
# fluxTBCA_mod2 <- lm(TBCA ~ CO2cum*CO2_treatment*Water_treatment , data=belowflux)
#  summary(fluxTBCA_mod2)
#  anova(fluxTBCA_mod2)

###tbca and flux
latbca_mod <- lm(TBCA~ LAestlin, data=belowflux) 
summary(latbca_mod)
anova(latbca_mod)
plotresid(latbca_mod)
visreg(latbca_mod)


