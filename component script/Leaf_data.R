###harvest leaf mass
source("functions_and_packages/functions.R")
library(doBy)

#chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
chambersumm <- droplevels(chambersumm[,1:3])

#final harvest mass
harvest_mass <- read.csv("raw csv/HFE final harvest biomass by layer.csv")
harvest_mass <- subset(harvest_mass, chamber %in% unique(chambersumm$chamber))
harvest_mass <- droplevels(harvest_mass)

leafmass <- harvest_mass[, c(1:3)]
leafmass_cham <- summaryBy(Wleaf~chamber, data=leafmass, FUN=sum, keep.names = TRUE)

#missing the extra leaf material (add back)
extra_mass <- read.csv("raw csv/HFE extra plant mass.csv")
extra_mass$xleaf <- with(extra_mass,damage_leaf+ removed_leaf)

##add extra mass
leafmass_cham <- merge(leafmass_cham, extra_mass[, c(1, 10)])
leafmass_cham$leafmass <- with(leafmass_cham, Wleaf+xleaf)

###load %C and calculate litter carbon
leafpercC <- read.csv("calculated_mass/leaf_percent_carbon.csv")

leafmass_cham <- merge(leafmass_cham, leafpercC)

leafmass_cham$leafcarbon <- with(leafmass_cham, leafmass * (leafpercC/100))


##write this harvest data for use in table and stats  
write.csv(leafmass_cham[,c(1, 4,7)], "calculated_mass/leafcarbon_data.csv", row.names = FALSE)


##compare treatment means to what i already have
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
chambersumm <- droplevels(chambersumm[,1:3])

leafmass_cham <- merge(leafmass_cham,chambersumm)
leafmass_cham$treatments <- with(leafmass_cham, interaction(CO2_treatment, Water_treatment))

leaf_agg <- summaryBy(leafcarbon~treatments, data=leafmass_cham, FUN=c(mean,se))



### run stats
library(visreg)
library(nortest)
library(multcomp)
library(doBy)
library(RVAideMemoire)

ad.test(leafmass_cham$leafcarbon) ##appears normal

leaf_mod <- lm(leafcarbon ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=leafmass_cham)
anova(leaf_mod)
summary(leaf_mod)

##no interaction
leaf_mod2 <- lm(leafcarbon ~ treatments, data=leafmass_cham)
tukey_leaf<- glht(leaf_mod2, linfct = mcp(treatments= "Tukey"))
leaf_siglets<- cld(tukey_leaf)
leaf_siglets2 <- leaf_siglets$mcletters$Letters

