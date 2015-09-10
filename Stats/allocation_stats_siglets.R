###stats for component from final harvest including boles, branch, leaves, fine roots and coarse roots
source("functions_and_packages/functions.R")
library(visreg)
library(nortest)
library(multcomp)
library(doBy)
library(RVAideMemoire)

##need final harvest values (not treatmen means)
#tree_C <- read.csv("master_scripts/harvest_chamber.csv")
tree_C <- read.csv("calculated_mass/chamber_carbon.csv")

##calculated component fractions
tree_C$lmf <- with(tree_C, (leafcarbon+littercarbon)/treeC)
tree_C$smf <- with(tree_C, (branchC+boleC)/treeC)
tree_C$rmf <- with(tree_C, (rootC)/treeC)

##calculate belowground flux
tree_C$tbca <- with(tree_C, Cflux-Cab)
tree_C$Fs_resid <- with(tree_C, Cflux - (Cab+rootC))

tree_C$treatment <- with(tree_C, as.factor(paste(CO2_treatment, Water_treatment, sep="-")))
tree_C$treatment <-  relevel(tree_C$treatment, ref="ambient-wet")

###analyze mass fractions.....individual components are then below

frac_trts <- summaryBy(lmf+smf+rmf ~ treatment, data=tree_C, FUN=c(mean, se))

#LMF----------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$lmf)
with(tree_C, boxplot(lmf~treatment))  

lmf_mod <- lm(lmf ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
anova(lmf_mod)

lmf_co2 <- lm(lmf ~ CO2_treatment, data=tree_C) 
  anova(lmf_co2)
  tukey_lmfco2<- glht(lmf_co2, linfct = mcp(CO2_treatment= "Tukey"))
  lmfco2_siglets<- cld(tukey_lmfco2)
  lmfco2_siglets2 <- lmfco2_siglets$mcletters$Letters
  
(mean(tree_C[tree_C$CO2_treatment=="elevated", "lmf"]) - mean(tree_C[tree_C$CO2_treatment=="ambient", "lmf"]))/mean(tree_C[tree_C$CO2_treatment=="elevated", "lmf"])
###lmf 15.2% higher in elevated CO2, no effect of h2o and no interation

lmf_h20 <- lm(lmf_co2 ~ Water_treatment, data=tree_C) 
  anova(lmf_h20)
  tukey_lmfh20 <- glht(lmf_h20, linfct = mcp(Water_treatment= "Tukey"))
  lmfh20_siglets<- cld(tukey_lmfh20)
  lmfh20_siglets2 <- lmfh20_siglets$mcletters$Letters  

##lmf vs co2 flux
fluxlmf_mod <- lm(lmf ~ Cflux, data=tree_C) 
summary(fluxlmf_mod)
anova(fluxlmf_mod)
# fluxlmf_mod2 <- lm(lmf ~ Cflux*CO2_treatment*Water_treatment , data=tree_C)
#  summary(fluxlmf_mod2)
#  anova(fluxlmf_mod2)


#SMF----------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$smf)
with(tree_C, boxplot(smf~treatment)) 

smf_mod <- lm(smf ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
anova(smf_mod)
summary(smf_mod)

smf_co2 <- lm(smf ~ CO2_treatment, data=tree_C) 
  anova(smf_co2)
  tukey_smfco2<- glht(smf_co2, linfct = mcp(CO2_treatment= "Tukey"))
  smfco2_siglets<- cld(tukey_smfco2)
  smfco2_siglets2 <- smfco2_siglets$mcletters$Letters

smf_h20 <- lm(smf ~ Water_treatment, data=tree_C) 
  anova(smf_h20)
  tukey_smfh20 <- glht(smf_h20, linfct = mcp(Water_treatment= "Tukey"))
  smfh20_siglets<- cld(tukey_smfh20)
  smfh20_siglets2 <- smfh20_siglets$mcletters$Letters  
##smf reduced by 8% in eco2, no interaction, no drought
(mean(tree_C[tree_C$CO2_treatment=="ambient", "smf"]) - mean(tree_C[tree_C$CO2_treatment=="elevated", "smf"]))/mean(tree_C[tree_C$CO2_treatment=="ambient", "smf"])
  
  
##smf vs co2 flux
fluxsmf_mod <- lm(smf ~ Cflux, data=tree_C) 
  summary(fluxsmf_mod)
  anova(fluxsmf_mod)
fluxsmf_mod2 <- lm(smf ~ Cflux*CO2_treatment*Water_treatment , data=tree_C)
  summary(fluxsmf_mod2)
  anova(fluxsmf_mod2)


(mean(tree_C[tree_C$CO2_treatment=="ambient", "smf"]) - mean(tree_C[tree_C$CO2_treatment=="elevated", "smf"]))/mean(tree_C[tree_C$CO2_treatment=="ambient", "smf"])

# RMF----------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$rmf)
with(tree_C, boxplot(rmf~treatment)) 
  
rmf_mod <- lm(rmf ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
  anova(rmf_mod)
  summary(rmf_mod)
  visreg(rmf_mod)
  visreg(rmf_mod, xvar="Water_treatment", by="CO2_treatment", overlay=TRUE)
  
##rmf vs co2 flux
  fluxrmf_mod <- lm(rmf ~ Cflux, data=tree_C) 
  summary(fluxrmf_mod)
  anova(fluxrmf_mod)
  fluxsmf_mod2 <- lm(smf ~ Cflux*CO2_treatment*Water_treatment , data=tree_C)
  summary(fluxsmf_mod2)
  anova(fluxsmf_mod2)

##Bole---------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$boleC) ##appears normal
boxplot(tree_C$boleC~tree_C$treatment)

bole_mod <- lm(boleC ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
  anova(bole_mod)
  summary(bole_mod)
  plotresid(bole_mod)
  visreg(bole_mod)
  confint(bole_mod)
  
###use one-way anova to test each pairwise comparisons  
ambdry_ambwet <- tree_C[tree_C$treatment == "ambient-dry" |tree_C$treatment == "ambient-wet", c("boleC", "treatment")]
ambdry_eledry <- tree_C[tree_C$treatment == "ambient-dry" |tree_C$treatment == "elevated-dry", c("boleC", "treatment")]
ambdry_elewet <- tree_C[tree_C$treatment == "ambient-dry"|tree_C$treatment == "elevated-wet", c("boleC", "treatment")]
elewet_eledry <- tree_C[tree_C$treatment == "elevated-wet"|tree_C$treatment == "elevated-dry", c("boleC", "treatment")]
elewet_ambwet <- tree_C[tree_C$treatment == "elevated-wet"|tree_C$treatment == "ambient-wet", c("boleC", "treatment")]
eledry_ambwet <- tree_C[tree_C$treatment == "elevated-dry"|tree_C$treatment == "ambient-wet", c("boleC", "treatment")]

###eCO2 effects

##within wet treatments: YES
AwetEwet <- lm(boleC ~ treatment, data=elewet_ambwet)
anova(AwetEwet) 
confint(AwetEwet)
P1 <- getP(AwetEwet)

##within dry treatments: NO
AdryEdry <- lm(boleC ~ treatment, data=ambdry_eledry)
anova(AdryEdry) 
P2 <- getP(AdryEdry)

####Drought
##within eCO2: NO
EdryEwet_mod <- lm(boleC ~ treatment, data=elewet_eledry)
anova(EdryEwet_mod) 
P3 <- getP(EdryEwet_mod)

##within aCO2: YES  
AdryAwet_mod <- lm(boleC ~ treatment, data=ambdry_ambwet)
anova(AdryAwet_mod) ###yes
confint(AdryAwet_mod)
P4 <- getP(AdryAwet_mod)

P_raw <- c(P1, P2, P3, P4)
##adjust pvalue with BH corrections
P_corr <- p.adjust(P_raw, method="BH")


##Attempt to do post hoc compairsons on interactions
bole_mod2<-lm(boleC~treatment, data=tree_C)
anova(bole_mod2)
tukey_bole<- glht(bole_mod2, linfct = mcp(treatment= "Tukey"))
bole_siglets<- cld(tukey_bole)
bole_siglets2 <- bole_siglets$mcletters$Letters




library(phia)
bole.means <- interactionMeans(bole_mod)
plot(bole.means)
##test simple main effects
testInteractions(bole_mod,  fixed = "CO2_treatment", across="Water_treatment") 
testInteractions(bole_mod,  fixed = "Water_treatment", across="CO2_treatment")
testInteractions(bole_mod, pairwise="CO2_treatment", across ="Water_treatment")

testInteractions(bole_mod)

custom.contr <- contrastCoefficients(
  CO2_treatment ~ ambient - elevated,
  Water_treatment ~ dry - wet,
  data=tree_C, normailize=TRUE
)
custom.contr$CO2_treatment
custom.contr$Water_treatment

names(custom.contr$CO2_treatment) <- "ambient vs elevated"
names(custom.contr$Water_treatment) <- "dry vs wet"
  
testInteractions(bole_mod, custom=custom.contr)

##2. branch---------------------------------------------------------------------------------------------------------------------  
ad.test(tree_C$branchC) ##appears normal
  
branch_mod <- lm(branchC ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
  anova(branch_mod)
  summary(branch_mod)
  plotresid(branch_mod)
  visreg(branch_mod)
  
##no interactions so run simple mod for each treatment
with(tree_C, boxplot(branchC~treatment))  

branch_co2 <- lm(branchC ~ CO2_treatment, data=tree_C) 
  anova(branch_co2)
  tukey_branchco2<- glht(branch_co2, linfct = mcp(CO2_treatment= "Tukey"))
  branchco2_siglets<- cld(tukey_branchco2)
  branchco2_siglets2 <- branchco2_siglets$mcletters$Letters
  ## no difference in branch carbon with eco2

branch_h20 <- lm(branchC ~ Water_treatment, data=tree_C) 
  anova(branch_h20)
  tukey_branchh20 <- glht(branch_h20, linfct = mcp(Water_treatment= "Tukey"))
  branchh20_siglets<- cld(tukey_branchh20)
  branchh20_siglets2 <- branchh20_siglets$mcletters$Letters  
## branch C not different by drought treatment


##3. leafC---------------------------------------------------------------------------------------------------------------------  
ad.test(tree_C$leafcarbon) ##appears normal
  
leaf_mod <- lm(leafcarbon ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
  anova(leaf_mod)
  summary(leaf_mod)

  
##no interactions so run simple mod for sig letters
with(tree_C, boxplot(leafcarbon~treatment))  

leaf_co2 <- lm(leafcarbon ~ CO2_treatment, data=tree_C) 
  anova(leaf_co2)
  tukey_leafco2<- glht(leaf_co2, linfct = mcp(CO2_treatment= "Tukey"))
  leafco2_siglets<- cld(tukey_leafco2)
  leafco2_siglets2 <- leafco2_siglets$mcletters$Letters
  ## no difference in branch carbon with eco2

leaf_h20 <- lm(leafcarbon ~ Water_treatment, data=tree_C) 
  anova(leaf_h20)
  tukey_leafh20 <- glht(leaf_h20, linfct = mcp(Water_treatment= "Tukey"))
  leafh20_siglets<- cld(tukey_leafh20)
  leafh20_siglets2 <- leafh20_siglets$mcletters$Letters  
  ## leaf C reducted by 39.8% in drought
  
  
##3. litter C---------------------------------------------------------------------------------------------------------------------  
ad.test(tree_C$littercarbon) ##appears normal
  
leaf_mod <- lm(littercarbon ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
  anova(leaf_mod)
  summary(leaf_mod)
  
##no interactions so run simple mod for sig letters
litter_co2 <- lm(littercarbon ~ CO2_treatment, data=tree_C) 
  tukey_litterco2<- glht(litter_co2, linfct = mcp(CO2_treatment= "Tukey"))
  litterco2_siglets<- cld(tukey_litterco2)
  litterco2_siglets2 <- litterco2_siglets$mcletters$Letters

  
litter_h20 <- lm(littercarbon ~ Water_treatment, data=tree_C) 
  tukey_litterh20 <- glht(litter_h20, linfct = mcp(Water_treatment= "Tukey"))
  litterh20_siglets<- cld(tukey_litterh20)
  litterh20_siglets2 <- litterh20_siglets$mcletters$Letters  
  
  

##3. Root---------------------------------------------------------------------------------------------------------------------  
  ##all roots pooled
ad.test(tree_C$rootC) ##appears normal
with(tree_C, boxplot(rootC~treatment))  
  
root_mod <- lm(rootC ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
  anova(root_mod)
  summary(root_mod)
  visreg(root_mod)
  plotresid(root_mod)

  root_co2 <- lm(rootC ~ CO2_treatment, data=tree_C)  
root_h20 <- lm(rootC ~ Water_treatment, data=tree_C)
summary(root_co2)
summary(root_h20)
  
###no effect of either treatment on coarse root C mass

  