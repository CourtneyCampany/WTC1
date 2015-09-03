###stats for component from final harvest including boles, branch, leaves, fine roots and coarse roots
library(visreg)
library(nortest)
library(multcomp)

##need final harvest values (not treatmen means)
tree_C <- read.csv("master_scripts/harvest_chamber.csv")

##calculated component fractions
tree_C$tree_total <- with(tree_C, branchC+boleC+leafC+litterC+frootC_all+CrootC)

tree_C$lmf <- with(tree_C, leafC_litterC/tree_total)
tree_C$smf <- with(tree_C, (branchC+boleC)/tree_total)
tree_C$rmf <- with(tree_C, (frootC_all + CrootC)/tree_total)

##need TBCA to add to roots
tbca <- read.csv("calculated_mass/TBCA.csv")

tree_C <- merge(tree_C, tbca)
tree_C$treatment <-  relevel(tree_C$treatment, ref="ambient-wet")

###analyze mass fractions.....individual components are then below


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

lmf_h20 <- lm(lmf_co2 ~ Water_treatment, data=tree_C) 
  anova(lmf_h20)
  tukey_lmfh20 <- glht(lmf_h20, linfct = mcp(Water_treatment= "Tukey"))
  lmfh20_siglets<- cld(tukey_lmfh20)
  lmfh20_siglets2 <- lmfh20_siglets$mcletters$Letters  

##lmf vs co2 flux
fluxlmf_mod <- lm(lmf ~ CO2cum, data=tree_C) 
summary(fluxlmf_mod)
anova(fluxlmf_mod)
# fluxlmf_mod2 <- lm(lmf ~ CO2cum*CO2_treatment*Water_treatment , data=tree_C)
# summary(fluxlmf_mod2)
# anova(fluxlmf_mod2)


#SMF----------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$smf)
with(tree_C, boxplot(smf~treatment)) 

smf_mod <- lm(smf ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
anova(smf_mod)

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
  
##lmf vs co2 flux
fluxsmf_mod <- lm(smf ~ CO2cum, data=tree_C) 
  summary(fluxsmf_mod)
  anova(fluxsmf_mod)
fluxsmf_mod2 <- lm(smf ~ CO2cum*CO2_treatment*Water_treatment , data=tree_C)
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
  
  ##Attempt to do post hoc compairsons on interactions
  tree_C$CW<-interaction(tree_C$CO2_treatment,tree_C$Water_treatment)
  rmf_mod2<-lm(rmf~CW, data=tree_C)
  anova(rmf_mod2)
  tukey_rmf<- glht(rmf_mod2, linfct = mcp(CW= "Tukey"))
  rmf_siglets<- cld(tukey_rmf)
  rmf_siglets2 <- rmf_siglets$mcletters$Letters
  
  library(phia)
  testInteractions(rmf_mod, pairwise="Water_treatment", across="CO2_treatment")
  testInteractions(rmf_mod)
  testFactors(rmf_mod, levels=c("Water_treatment","CO2_treatment"))
  
##rmf vs co2 flux
  fluxrmf_mod <- lm(rmf ~ CO2cum, data=tree_C) 
  summary(fluxrmf_mod)
  anova(fluxrmf_mod)
  fluxsmf_mod2 <- lm(smf ~ CO2cum*CO2_treatment*Water_treatment , data=tree_C)
  summary(fluxsmf_mod2)
  anova(fluxsmf_mod2)

##Bole---------------------------------------------------------------------------------------------------------------------
ad.test(tree_C$boleC) ##appears normal

bole_mod <- lm(boleC ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
  anova(bole_mod)
  summary(bole_mod)
  plot(bole_mod)
  visreg(bole_mod)
  
##no interactions so run simple mod for each treatment
with(tree_C, boxplot(boleC~treatment))  

bole_co2 <- lm(boleC ~ CO2_treatment, data=tree_C) 
  anova(bole_co2)
  tukey_boleco2<- glht(bole_co2, linfct = mcp(CO2_treatment= "Tukey"))
  boleco2_siglets<- cld(tukey_boleco2)
  boleco2_siglets2 <- boleco2_siglets$mcletters$Letters
  ## reductions in bole mass with Eco2 by 34.1%

bole_h20 <- lm(boleC ~ Water_treatment, data=tree_C) 
  anova(bole_h20)
  tukey_boleh20 <- glht(bole_h20, linfct = mcp(Water_treatment= "Tukey"))
  boleh20_siglets<- cld(tukey_boleh20)
  boleh20_siglets2 <- boleco2_siglets$mcletters$Letters  
  ## bole C reduced in drought treatments by 22.7%
  

##2. branch---------------------------------------------------------------------------------------------------------------------  
ad.test(tree_C$branchC) ##appears normal
  
branch_mod <- lm(branchC ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
  anova(branch_mod)
  summary(branch_mod)
  plot(branch_mod)
  visreg(branch_mod)
  
##no interactions so run simple mod for each treatment
with(tree_C, boxplot(branchC~treatment))  

branch_co2 <- lm(branchC ~ CO2_treatment, data=tree_C) 
  anova(branchC_co2)
  tukey_branchco2<- glht(branchC_co2, linfct = mcp(CO2_treatment= "Tukey"))
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
ad.test(tree_C$leafC) ##appears normal
  
leaf_mod <- lm(leafC ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
  anova(leaf_mod)
  summary(leaf_mod)

  
##no interactions so run simple mod for sig letters
with(tree_C, boxplot(leafC~treatment))  

leaf_co2 <- lm(leafC ~ CO2_treatment, data=tree_C) 
  anova(leaf_co2)
  tukey_leafco2<- glht(leaf_co2, linfct = mcp(CO2_treatment= "Tukey"))
  leafco2_siglets<- cld(tukey_leafco2)
  leafco2_siglets2 <- leafco2_siglets$mcletters$Letters
  ## no difference in branch carbon with eco2

leaf_h20 <- lm(leafC ~ Water_treatment, data=tree_C) 
  anova(leaf_h20)
  tukey_leafh20 <- glht(leaf_h20, linfct = mcp(Water_treatment= "Tukey"))
  leafh20_siglets<- cld(tukey_leafh20)
  leafh20_siglets2 <- leafh20_siglets$mcletters$Letters  
  ## leaf C reducted by 39.8% in drought
  
  
##3. litter C---------------------------------------------------------------------------------------------------------------------  
ad.test(tree_C$litterC) ##appears normal
  
leaf_mod <- lm(litterC ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
  anova(leaf_mod)
  summary(leaf_mod)
  
##no interactions so run simple mod for sig letters
litter_co2 <- lm(litterC ~ CO2_treatment, data=tree_C) 
  tukey_litterco2<- glht(litter_co2, linfct = mcp(CO2_treatment= "Tukey"))
  litterco2_siglets<- cld(tukey_litterco2)
  litterco2_siglets2 <- litterco2_siglets$mcletters$Letters

  
litter_h20 <- lm(litterC ~ Water_treatment, data=tree_C) 
  tukey_litterh20 <- glht(litter_h20, linfct = mcp(Water_treatment= "Tukey"))
  litterh20_siglets<- cld(tukey_litterh20)
  litterh20_siglets2 <- litterh20_siglets$mcletters$Letters  
  
  

##3. Coarse Root---------------------------------------------------------------------------------------------------------------------  
ad.test(tree_C$CrootC) ##appears normal
with(tree_C, boxplot(CrootC~treatment))  
  
Croot_mod <- lm(CrootC ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
  anova(Croot_mod)
  summary(Croot_mod)
  
Croot_co2 <- lm(CrootC ~ CO2_treatment, data=tree_C)  
Croot_h20 <- lm(CrootC ~ Water_treatment, data=tree_C)
summary(Croot_co2)
summary(Croot_h20)
  
###no effect of either treatment on coarse root C mass


##3. Fine Root---------------------------------------------------------------------------------------------------------------------  
ad.test(tree_C$frootC_all) ##appears normal
with(tree_C, boxplot(frootC_all~treatment))  

froot_mod <- lm(frootC_all ~ CO2_treatment+Water_treatment+CO2_treatment:Water_treatment, data=tree_C)
  anova(froot_mod)
  summary(froot_mod)
  plot(froot_mod)
  visreg(froot_mod)  

Froot_co2 <- lm(frootC_all ~ CO2_treatment, data=tree_C)  
  summary(Froot_co2)

Froot_h20 <- lm(frootC_all ~ Water_treatment, data=tree_C)
  summary(Froot_h20)
  anova(Froot_h20)
  tukey_frooth20 <- glht(Froot_h20, linfct = mcp(Water_treatment= "Tukey"))
  frooth20_siglets<- cld(tukey_leafh20)
  frooth20_siglets2 <- frooth20_siglets$mcletters$Letters  
##no effect of elevated CO2 on fine root C mass
##fine root C reduced by 24.8% in drought treatments
  
#(mean(tree_C[tree_C$Water_treatment=="wet", "frootC_all"]) - mean(tree_C[tree_C$Water_treatment=="dry", "frootC_all"]))/mean(tree_C[tree_C$Water_treatment=="wet", "frootC_all"])
  
  