###stats for component from final harvest including boles, branch, leaves, fine roots and coarse roots
library(visreg)
library(nortest)
library(multcomp)
library(RVAideMemoire)

##need final harvest values (not treatmen means)
leafarea <- read.csv("raw csv/HFE LA estimates alldates.csv")
  leafarea$Date <- as.Date(leafarea$Date)

leafarea_final <- leafarea[leafarea$Date == max(leafarea$Date),]

# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

#merge leaf area with treatments
leafarea_final <- merge(leafarea_final, chambersumm)
leafarea_final$treatments <- with(leafarea_final, paste(CO2_treatment, Water_treatment, sep="-"))
  
###component stats (test co2, water and then both)

##1. leafarea---------------------------------------------------------------------------------------------------------------------
ad.test(leafarea_final$LAestlin) ##appears normal
with(leafarea_final, boxplot(LAestlin~treatments))  

la_mod <- lm(LAestlin ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=leafarea_final)
anova(la_mod)
summary(la_mod)
plotresid(la_mod)
visreg(la_mod)

##no interactions so run simple mod for each treatment
la_co2 <- lm(LAestlin ~ CO2_treatment, data=leafarea_final) 
  anova(la_co2)
  tukey_laco2<- glht(la_co2, linfct = mcp(CO2_treatment= "Tukey"))
  laco2_siglets<- cld(tukey_laco2)
  laco2_siglets2 <- laco2_siglets$mcletters$Letters
  ## reductions in leafarea in Eco2 by 31.3%

la_h20 <- lm(LAestlin ~ Water_treatment, data=leafarea_final) 
  anova(la_h20)
  tukey_lah20 <- glht(la_h20, linfct = mcp(Water_treatment= "Tukey"))
  lah20_siglets<- cld(tukey_lah20)
  lah20_siglets2 <- lah20_siglets$mcletters$Letters  
  ## no difference in drought treatment with leaf area
  
  
  
  