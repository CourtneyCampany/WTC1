#read in calculated aboveground component masses, Cfluxes, and plot summary

bole <- read.csv("calculated mass/bole310 mass.csv")
branch <- read.csv("calculated mass/branch mass.csv")
leaf <- read.csv("calculated mass/leaf mass.csv")
Cflux <- read.csv("calculated mass/chamber C flux.csv")
leafpercC <- read.csv("calculated mass/leaf percent carbon.csv")

# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
chambersumm <- droplevels(chambersumm[,1:3])


#C MASS vs C FLUX
#new dataframe 'treemass' with all components and chamber flux over all dates, used for linear interpolation
tree_mass <- merge(branch, leaf, by = c("Date", "chamber"), all=TRUE)
tree_mass <- merge(tree_mass, bole, by = c("Date", "chamber"), all=TRUE)

#add Chamber carbon flux
tree_mass <- merge(tree_mass, Cflux, by = c("Date", "chamber"), all=TRUE)

#subset the last year of the experiment
tree_mass$Date <- as.Date(tree_mass$Date)
tree_mass_yr <- subset(tree_mass, tree_mass$Date >= "2008-03-14")


#LINEAR INTERPOLATION of boles and branches between allometry surveys (1 year)

#FIRST create dfr with all days for interpolation

#empty list
datels <- list()
#date seq loop
datemaker <- for (i in unique(chambersumm$chamber)){
  datels[[i]] <- data.frame(Date = seq(from = as.Date("2008-3-14"), to = as.Date("2009-3-16"), by = "day"), 
                            chamber = i)} #bole_mass = "")}

# row-bind everything together:
dateseq <- do.call(rbind,datels)

#SECOND merge bole and branch dataset with dateseq dfr and interpolate
treemass_alldays <- merge(dateseq, tree_mass_yr, by = c("Date", "chamber"), all=TRUE)

#use dateseq for interpolation
treemass_sp <- split(treemass_alldays, treemass_alldays$chamber)
treemass_sp <- lapply(treemass_sp, function(z){
  
  apfun_bo310 <- approxfun(x=z$Date, y=z$bole_mass)
  apfun_br <- approxfun(x=z$Date, y=z$branch_mass)
  
  z$bole_pred <- apfun_bo310(z$Date)
  z$branch_pred <- apfun_br(z$Date)
  
  return(z)
})
treemass_pred <- do.call(rbind, treemass_sp)

#subset data where leaf and branch mass surveys began
treemass_pred <- subset(treemass_pred, Date >= "2008-04-15")

treemass_test <- treemass_pred[,c(1:2, 7,10, 12:15)]

mass_start <- subset(treemass_pred[,c(1:3, 7, 10, 14)], Date =="2008-04-15")
names(mass_start)[3:6]<- c("branch_start", "litter_start", "leaf_start", "bole_start")

treemass_pred_yr <- merge(treemass_pred[,c(1:2, 7,10, 12:15)], mass_start[,2:6], by="chamber")
#--------------------------------------------------------------------------------------------------
#need to remove biomass before 2008-04-15 so subtract the amount from this date from everydate.
remove_mass <- function(dfr){

  #create objects of start mass for leaf, litter, bole, branch
  mass_start <- subset(dfr[,c(1:3, 7, 10, 14)], Date =="2008-04-15")
  names(mass_start)[3:6]<- c("branch_start", "litter_start", "leaf_start", "bole_start")
  
  mass_calc <- merge(dfr[,c(1:2, 7,10, 12:15)], mass_start[,2:6], by="chamber")
  
  #now remove theses values from interpolated data over time, sets 2008-04-15 =0 
  mass_calc$bole_yr <- with(mass_calc, bole_pred - bole_start) 
  mass_calc$branch_yr <-  with(mass_calc, branch_pred - branch_start) 
  mass_calc$leaf_yr <-  with(mass_calc, leafmass - leaf_start) 
  mass_calc$litter_yr <-  with(mass_calc, LAlittercumlin - litter_start) 
  
  treemass_final <- mass_calc[,c(1:2,5:6, 13:16)]
}

treemass_final <- remove_mass(treemass_pred )
#------------------------------------------------------------------------------------------------------
#CALCULATE TREE CARBON, assume 50% for all components

tree_carbon_calc <- function(dfr, Cleaf_perc){

  dfr$branchC <- (dfr$branch_yr * .5)
  dfr$boleC <- (dfr$bole_yr * .5)
  
  #add measured leaf percent carbon and convert
  tree_carbon <- merge(dfr, Cleaf_perc, by = "chamber")
  tree_carbon$leafC <- with(tree_carbon, leaf_yr * (leafpercC/100))
  tree_carbon$litterC <- with(tree_carbon, litter_yr * (leafpercC/100))
  
  #order data, clean up, and write
  tree_carbon <- tree_carbon[order(as.Date(tree_carbon$Date, format="%d/%m/%Y")),]

}
  
tree_carbon <-tree_carbon_calc(treemass_final, leafpercC)

#carbon data set
write.csv(tree_carbon[, c(1:4, 9:10, 13:14)], file = "whole tree csv/tree_C_flux.csv", row.names=FALSE)
#mass data set
write.csv(tree_carbon[, 1:8], file = "whole tree csv/tree_mass_Cflux.csv", row.names=FALSE)
