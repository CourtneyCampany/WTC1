###SLA at harvest
source("functions_and_packages/functions.R")
library(visreg)
library(nortest)
library(multcomp)
library(doBy)
library(RVAideMemoire)
library(doBy)

harvest <- read.csv("raw csv/HFE final harvest biomass by layer.csv")

# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

harvest2 <- merge(harvest, chambersumm)
  harvest2$treatment <- with(harvest2, as.factor(paste(CO2_treatment, Water_treatment, sep="-")))
  harvest2$treatment <-  relevel(harvest2$treatment, ref="ambient-wet")


##STATS------------------------------------------------------------------------------------------------------------------------
# 
# sla_mod <- lm(SLA ~ layerno, data=harvest2)
# summary(sla_mod)
# 
# sla_mod2 <- lm(SLA ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=harvest2)
# summary(sla_mod2)
# 
# sla_mod3 <- lm(SLA ~ CO2_treatment, data=harvest2)
# summary(sla_mod3)
# visreg(sla_mod3)
# 
# 
# sla_wtc <- summaryBy(SLA~ chamber, data=harvest2, FUN=c(mean,se))
# sla_wtc2 <- merge(sla_wtc, chambersumm)
# 
# plot(SLA.mean ~ CO2_treatment, data=sla_wtc2)


####recalculate with weighted mean

sla_sp <- split(harvest2, harvest2$chamber)

sla_mean <- sapply(sla_sp, function(x) weighted.mean(x$SLA, w = x$layerno))
sla_wm <- data.frame(chamber=names(sla_mean), sla_wm=as.vector(sla_mean) )  

##rerun model for CO2 effects on SLA 
sla_cham <- merge(sla_wm, chambersumm)
  sla_cham$treatment <- with(sla_cham, as.factor(paste(CO2_treatment, Water_treatment, sep="-")))
  sla_cham$treatment <-  relevel(sla_cham$treatment, ref="ambient-wet")
  
with(sla_cham, boxplot(sla_wm~treatment))   
  
##save a means,se of SLA for datatable----------------------------------------------------------------------------------------
  sla_agg <- summaryBy(sla_wm~ treatment, data=sla_cham, FUN=c(mean,se))
  sla_agg2 <- sla_agg[c(2,1,3,4),]
  write.csv(sla_agg2, "calculated_mass/sla_means.csv", row.names=FALSE)  

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
slawm_co2 <- summaryBy(sla_wm ~ CO2_treatment, data=sla_cham, FUN=mean)
##CO2 effect 0.089 (9%) reduction in drought
(mean(sla_cham[sla_cham$Water_treatment=="wet", "sla_wm"]) - mean(sla_cham[sla_cham$Water_treatment=="dry", "sla_wm"]))/mean(sla_cham[sla_cham$Water_treatment=="wet", "sla_wm"])
slawm_h20 <- summaryBy(sla_wm ~ Water_treatment, data=sla_cham, FUN=mean)

###sig letters for table

slawm_mod2 <- lm(sla_wm ~ treatment, data=sla_cham) 
  anova(slawm_mod2)
  summary(slawm_mod2)
  
tukey_sla <- glht(slawm_mod2, linfct = mcp(treatment= "Tukey"))
sla_siglets<- cld(tukey_sla)
sla_siglets2 <- sla_siglets$mcletters$Letters

###p values and sigletters for table
Psla <- anova(slawm_mod)[[5]][3]
Psla_co2 <- anova(slawm_mod)[[5]][1]
Psla_h20 <- anova(slawm_mod)[[5]][2]  

sla_siglets3 <- data.frame(sla = sla_siglets2)

write.csv(Psla, "Stats/p_sigs/sla/P_interactions_sla.csv", row.names = FALSE)
write.csv(Psla_co2, "Stats/p_sigs/sla/P_co2_sla.csv", row.names = FALSE)
write.csv(Psla_h20, "Stats/p_sigs/sla/P_h20_sla.csv", row.names = FALSE)

write.csv(sla_siglets3, "Stats/p_sigs/sla/sigletters_sla.csv", row.names = FALSE)

