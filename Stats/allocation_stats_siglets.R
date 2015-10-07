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


##calculate belowground flux
tree_C$tbca <- with(tree_C, Cflux-Cab)
tree_C$Fs_resid <- with(tree_C, Cflux - (Cab+rootC))
tree_C$stemC <- with(tree_C, boleC + branchC)

tree_C$treatment <- with(tree_C, as.factor(paste(CO2_treatment, Water_treatment, sep="-")))
tree_C$treatment <-  relevel(tree_C$treatment, ref="ambient-wet")


##Bole---------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$boleC) ##appears normal
boxplot(tree_C$boleC~tree_C$treatment)

bole_mod <- lm(boleC ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
  anova(bole_mod)
  summary(bole_mod)
  plotresid(bole_mod)
  visreg(bole_mod)
  confint(bole_mod)
  
Pbo <- anova(bole_mod)[[5]][1]
Pbo_co2 <- anova(bole_mod)[[5]][2]
Pbo_h20 <- anova(bole_mod)[[5]][2]   
  
  
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
  
  
  ##no interaction
branch_mod2 <- lm(branchC ~ treatment, data=tree_C)
  tukey_branch<- glht(branch_mod2, linfct = mcp(treatment= "Tukey"))
  branch_siglets<- cld(tukey_branch)
  branch_siglets2 <- branch_siglets$mcletters$Letters  
  
  
## branch C not different by drought treatment
  
Pbr <- anova(branch_mod)[[5]][1]
Pbr_co2 <- anova(branch_mod)[[5]][2]
Pbr_h20 <- anova(branch_mod)[[5]][2]
  
##3. leafC---------------------------------------------------------------------------------------------------------------------  
ad.test(tree_C$leafcarbon) ##appears normal
  
leaf_mod <- lm(leafcarbon ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
  anova(leaf_mod)
  summary(leaf_mod)

  
##no interactions so run simple mod for sig letters
with(tree_C, boxplot(leafcarbon~treatment))  

##no interaction
leaf_mod2 <- lm(leafcarbon ~ treatment, data=tree_C)
tukey_leaf<- glht(leaf_mod2, linfct = mcp(treatment= "Tukey"))
leaf_siglets<- cld(tukey_leaf)
leaf_siglets2 <- leaf_siglets$mcletters$Letters

  
Pl <- anova(leaf_mod)[[5]][1]
Pl_co2 <- anova(leaf_mod)[[5]][2]
Pl_h20 <- anova(leaf_mod)[[5]][2]  
  
##3. litter C---------------------------------------------------------------------------------------------------------------------  
ad.test(tree_C$littercarbon) ##appears normal
  
  litter_mod <- lm(littercarbon ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
  anova(litter_mod)
  summary(litter_mod)
  
  ##no interaction
  litter_mod2 <- lm(littercarbon ~ treatment, data=tree_C)
  tukey_litter<- glht(litter_mod2, linfct = mcp(treatment= "Tukey"))
  litter_siglets<- cld(tukey_litter)
  litter_siglets2 <- litter_siglets$mcletters$Letters
  
Pli <- anova(litter_mod)[[5]][1]
Pli_co2 <- anova(litter_mod)[[5]][2]
Pli_h20 <- anova(litter_mod)[[5]][2]  

##3. Root---------------------------------------------------------------------------------------------------------------------  
  ##all roots pooled
ad.test(tree_C$rootC) ##appears normal
with(tree_C, boxplot(rootC~treatment))  
  
root_mod <- lm(rootC ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
  anova(root_mod)
  summary(root_mod)
  visreg(root_mod)
  plotresid(root_mod)

roots_mod2 <- lm(Cflux ~ treatment, data=tree_C) 
  anova(roots_mod2)
  summary(roots_mod2)
  tukey_root <- glht(roots_mod2, linfct = mcp(treatment= "Tukey"))
  root_siglets<- cld(tukey_root)
  root_siglets2 <- root_siglets$mcletters$Letters
  
(mean(tree_C[tree_C$CO2_treatment=="ambient", "rootC"]) - mean(tree_C[tree_C$CO2_treatment=="elevated", "rootC"]))/mean(tree_C[tree_C$CO2_treatment=="ambient", "rootC"])
  

###use treatments to get sig letters when there is not interaction


Pr <- anova(root_mod)[[5]][1]
Pr_co2 <- anova(root_mod)[[5]][2]
Pr_h20 <- anova(root_mod)[[5]][2]   


##Cflux-------------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$Cflux) ##appears normal
with(tree_C, boxplot(Cflux~treatment))  

flux_mod <- lm(Cflux ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
anova(flux_mod)
summary(flux_mod)

Pf <- anova(flux_mod)[[5]][1]
Pf_co2 <- anova(flux_mod)[[5]][2]
Pf_h20 <- anova(flux_mod)[[5]][2]  

flux_mod2 <- lm(Cflux ~ treatment, data=tree_C) 
anova(flux_mod2)
summary(flux_mod2)
tukey_flux <- glht(flux_mod2, linfct = mcp(treatment= "Tukey"))
flux_siglets<- cld(tukey_flux)
flux_siglets2 <- flux_siglets$mcletters$Letters  


####table P values and sig letters-----------------------------------------------------------------------------------------------

###need tree vector of p values for water, co2, and interaction
###need vector of sig letters 

###variable order = bole, br, leaf, litter, root, flux
Pinter <- c(Pbo, Pbr, Pl, Pli, Pr, Pf)
Pco2 <- c(Pbo_co2, Pbr_co2, Pl_co2, Pli_co2, Pr_co2, Pf_co2)
Ph20 <- c(Pbo_h20, Pbr_h20, Pl_h20, Pli_h20, Pr_h20, Pf_co2)

sigletters<- data.frame(bole = bole_siglets2, branch = branch_siglets2,leaf = leaf_siglets2,
                         litterfall=litter_siglets2,root = root_siglets2,cflux = flux_siglets2)


write.csv(Pinter, "Stats/p_sigs/P_interactions.csv", row.names = FALSE)
write.csv(Pco2, "Stats/p_sigs/P_co2.csv", row.names = FALSE)
write.csv(Ph20, "Stats/p_sigs/P_h20.csv", row.names = FALSE)

write.csv(sigletters, "Stats/p_sigs/sigletters.csv", row.names = FALSE)


####wood (boles+branches)
ad.test(tree_C$stem) ##appears normal

stem_mod <- lm(stemC ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
anova(stem_mod)
summary(stem_mod)
plotresid(stem_mod)
visreg(stem_mod)

##no interactions so run simple mod for each treatment
with(tree_C, boxplot(stemC~treatment))  

stem_co2 <- lm(stemC ~ CO2_treatment, data=tree_C) 
anova(stem_co2)


stem_h20 <- lm(stemC ~ Water_treatment, data=tree_C) 
anova(stem_h20)



