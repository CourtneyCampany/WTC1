### This script creates TBCA from the flux data and the biomass data

#1: Fc,t = GPP - (Rl + Rs,br) = total carbon flux by chamber
#2: TBCA = Fc,t - Mab,t
#3: Mab,t = total mass of aboveground + leaflitter

# read harvest totals of mass and C flux by chmaber
harvest_C <- read.csv("master_scripts/harvest_chamber.csv")

#read in Leaf Area


##caluclate TBCA-----------------------------------------------------------------------------------------------------

harvest_C$TBCA <- with(harvest_C, CO2cum - treeC)

harvest_C$Fs_resid <- with(harvest_C, TBCA - (frootC_all + CrootC))



  
  