#read in calculated aboveground component masses, Cfluxes, and plot summary

bole <- read.csv("calculated_mass/bole_mass_pred.csv")
bole$Date <- as.Date(bole$Date)

Cflux <- read.csv("calculated_mass/chamber_C_flux.csv")
Cflux$Date <- as.Date(Cflux$Date)

leafcarbon <- read.csv("calculated_mass/leaf_carbon.csv")
leafcarbon$Date <- as.Date(leafcarbon$Date)

branch <- read.csv("calculated_mass/branch_mass_pred.csv")
branch$Date <- as.Date(branch$Date)

# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])


#C MASS vs C FLUX-----------------------------------------------------------------------------------------------------------
  
#new dataframe 'treemass' with all components and chamber flux over all dates, used for linear interpolation
tree_mass <- merge(branch, leafcarbon, by = c("Date", "chamber"), all=TRUE)
tree_mass <- merge(tree_mass, bole, by = c("Date", "chamber"), all=TRUE)

#add Chamber carbon flux
tree_mass <- merge(tree_mass, Cflux, by = c("Date", "chamber"), all=TRUE)

#subset the last year of the experiment
tree_mass$Date <- as.Date(tree_mass$Date)
tree_mass_yr <- subset(tree_mass, tree_mass$Date >= "2008-03-16")


#subset data where leaf and branch mass surveys began
treemass_pred <- subset(tree_mass_yr, Date >= "2008-04-15")

###clean and have only predicted mass
mass_pred <- treemass_pred[, c("Date", "chamber", "branch_pred",  "leafmass",  "littermass", "bole_mass_adj",  "CO2cum")]
carbon_pred <- treemass_pred[, c("Date", "chamber", "branch_carbon",  "leafcarbon", "littercarbon",  "bole_carbon", "CO2cum")]

###subset start values so can reset to 0 mass
mass_start <- subset(mass_pred, Date =="2008-04-15")
carbon_start <- subset(carbon_pred, Date =="2008-04-15")

names(mass_start)[3:7]<- c("branch_start", "leaf_start", "litter_start",  "bole_start", "CO2start")
names(carbon_start)[3:7]<- c("branch_start", "leaf_start", "litter_start",  "bole_start", "CO2start")


###merge back with pred and then substract this number from every value, should reset
mass_pred_yr <- merge(mass_pred, mass_start[,2:7], by="chamber")  ###remove Date from start value
mass_pred_yr <- mass_pred_yr[order(mass_pred_yr$Date),]

mass_pred_yr$bole_yr <- mass_pred_yr$bole_mass_adj - mass_pred_yr$bole_start 
mass_pred_yr$branch_yr <-  with(mass_pred_yr, branch_pred - branch_start) 
mass_pred_yr$leaf_yr <-  with(mass_pred_yr, leafmass - leaf_start) 
mass_pred_yr$litter_yr <-  with(mass_pred_yr, littermass - litter_start) 
mass_pred_yr$fluxC <-  with(mass_pred_yr, CO2cum - CO2start) 

##do the same for carbon

carbon_pred_yr <- merge(carbon_pred, carbon_start[,2:7], by="chamber")  ###remove Date from start value
carbon_pred_yr <- carbon_pred_yr[order(carbon_pred_yr$Date),]

  carbon_pred_yr$boleC <- with(carbon_pred_yr, bole_carbon - bole_start) 
  carbon_pred_yr$branchC <-  with(carbon_pred_yr, branch_carbon - branch_start) 
  carbon_pred_yr$leafC <-  with(carbon_pred_yr, leafcarbon - leaf_start) 
  carbon_pred_yr$litterC <-  with(carbon_pred_yr, littercarbon - litter_start) 
  carbon_pred_yr$fluxC <-  with(carbon_pred_yr, CO2cum - CO2start) 
  

#treemass_pred_yr <- merge(treemass_pred[,c(1:2, 7,10, 12:15)], mass_start[,2:6], by="chamber")


#carbon data set
write.csv(carbon_pred_yr, file = "whole_tree_csv/tree_C_flux.csv", row.names=FALSE)
#mass data set
write.csv(mass_pred_yr, file = "whole_tree_csv/tree_mass_Cflux.csv", row.names=FALSE)
