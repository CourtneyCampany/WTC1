##need to have litter and leaf carbon from harvest
source("functions_and_packages/functions.R")
library(doBy)

litter <- read.csv("raw csv/HFE leaf litter 2008-2009.csv")
litter_harvest <- read.csv("raw csv/HFE Litter after final harvest.csv")
names(litter_harvest)[2] <-"leaflitter_harvest"

litter_cham <- summaryBy(leaflitter ~ chamber, data=litter, FUN=mean, keep.names = TRUE)

litter_cham2 <- merge(litter_cham, litter_harvest[, 1:2])

litter_cham2$leaflitter_total <- with(litter_cham2, leaflitter+leaflitter_harvest)

###load %C and calculate litter carbon
leafpercC <- read.csv("calculated_mass/leaf_percent_carbon.csv")

litter_cham3 <- merge(litter_cham2, leafpercC)
litter_cham3$littercarbon <- with(litter_cham3, leaflitter_total * (leafpercC/100))


write.csv(litter_cham3[,c(1, 4,7)], "calculated_mass/litterfallcarbon.csv", row.names = FALSE)



### run stats
library(visreg)
library(nortest)
library(multcomp)
library(doBy)
library(RVAideMemoire)

# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
chambersumm <- droplevels(chambersumm[,1:3])

litter_stats <- merge(litter_cham3[,c(1, 4,7)], chambersumm)
litter_stats$treatments <- with(litter_stats, interaction(CO2_treatment, Water_treatment))


ad.test(litter_stats$littercarbon) ##appears normal

litter_mod <- lm(littercarbon ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=litter_stats)
anova(litter_mod)
summary(litter_mod)

##no interaction
litter_mod2 <- lm(littercarbon ~ treatments, data=litter_stats)
tukey_litter<- glht(litter_mod2, linfct = mcp(treatments= "Tukey"))
litter_siglets<- cld(tukey_litter)
litter_siglets2 <- litter_siglets$mcletters$Letters

