library(doBy)
library(plyr)

#all raw data here:--------------------------------------------------------------------------------------------------
#source("HFE chamber read data.R")
# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

#final harvest component biomasses
harvest_mass <- read.csv("raw csv/HFE final harvest biomass by layer.csv")
  harvest_mass <- subset(harvest_mass, chamber %in% unique(chambersumm$chamber))
  harvest_mass <- droplevels(harvest_mass)

#branch allometry through time
branch_allometry <- read.csv("raw csv/HFE branch diameter length.csv")
  branch_allometry$Date <- as.character(branch_allometry$Date)
  branch_allometry$Date <- as.Date(branch_allometry$Date)

#Sample branches from harvest, with dry mass as seperate dataframe for twig mass
branches <- read.csv("raw csv/HFE final harvest Sample Branches.csv", na.strings="???")
  branches <- subset(branches, chamber %in% unique(chambersumm$chamber))
  branches <- droplevels(branches)

DWbranches <- read.csv("raw csv/HFE final harvest Sample Branches dry weight.csv")
  DWbranches <- subset(DWbranches, chamber %in% unique(chambersumm$chamber))
  DWbranches <- droplevels(DWbranches)
  
#extra biomas from CWD, damage, or removal
extra_mass <- read.csv("raw csv/HFE extra plant mass.csv")
  extra_mass$cwd <- with(extra_mass, damage_branch + branch_litter + bark_litter + harvest_branch_litter +
                           harvest_bark_litter + removed_branch)

#BRANCH MASS estimations-------------------------------------------------------------------------------------------------


#mass is sum of harvest branches with diameter < & > than 1cm 
branchM <- subset(harvest_mass, select =c("chamber",  "layerno", "Wbrgt1" ,"Wbrlt1"))
  branchM$br_mass <- with(branchM, Wbrgt1 + Wbrlt1)


#calculate branch volume from harvest branch diameterm length, and.75 shape factor
br_volume_calc<- function(dfr) {
  volume_calc <- subset(dfr, dfr$Date == "2009-03-16")
  volume_calc <- subset(volume_calc, select = c("Date", "chamber", "stemnumber", "branchnumber", "diameter", "length", "branchBA"))
  volume_calc <- volume_calc[complete.cases(volume_calc),]
  #add 5cm to height, assuming no difference taper btw base and insertion diam
  volume_calc$Volume <- (volume_calc$branchBA*(volume_calc$length))*.75 
  row.names(volume_calc)<-NULL
  return(volume_calc)
}
br_vol <- br_volume_calc(branch_allometry)

#total harvest branch mass(by layer) an volume (branch#)
br_mass_tot <- ddply(branchM, c("chamber"), function(x) data.frame(br_mass=sum(x$br_mass)))
br_vol_tot <- ddply(br_vol, c("chamber"), function(x) data.frame(vol_tot=sum(x$Volume)))

###now I add CWD from harvest (extra mass) and see if this improves measurements
br_mass_tot$br_mass2 <- br_mass_tot$br_mass + extra_mass$cwd


#calculate branch density
br_density_calc <- function(volume, mass) {
  density_calc <- merge(volume, mass, by = "chamber")
  density_calc$branch_density <- density_calc$br_mass / density_calc$vol_tot
  #assume no difference between bark and wood density of branches
  density_calc <- subset(density_calc, select = c("chamber" , "branch_density"))
}

br_density <- br_density_calc(br_vol_tot, br_mass_tot[c(1,3)])


#branch mass across dates
br_mass_dates_calc <- function(allometry, density){
  allom_dates <- subset(allometry, select = c("Date",  "chamber",  "stemnumber",  "branchnumber",
                                              "diameter",	"length",	"branchBA"))
  mass_dates <- merge(allom_dates, density, by = "chamber")
  mass_dates$branch_mass <- ((mass_dates$branchBA*(mass_dates$length))
                              *.75)*mass_dates$branch_density
  #as before assumes shape factor of .75 and adds 5cm to length w/ no taper
  return(mass_dates)                           
}

br_mass_dates <-  br_mass_dates_calc(branch_allometry, br_density)

#total branch mass by chamber for each date
branch_mass_total <- aggregate(branch_mass ~ Date + chamber, data = br_mass_dates, FUN = sum)

range(branch_mass_total$Date)


###linear interpolate between dates-------------------------------------------------------------------------------

#empty list
datels <- list()
#date seq loop
datemaker <- for (i in unique(chambersumm$chamber)){
  datels[[i]] <- data.frame(Date = seq(from = as.Date("2008-04-15"), to = as.Date("2009-3-16"), by = "day"), 
                            chamber = i)}

# row-bind everything together:
dateseq <- do.call(rbind,datels)

#SECOND merge bole and branch dataset with dateseq dfr and interpolate
branchmass_alldays <- merge(dateseq, branch_mass_total, by = c("Date", "chamber"), all=TRUE)

#use dateseq for interpolation
brmass_sp <- split(branchmass_alldays, branchmass_alldays$chamber)
brmass_sp <- lapply(brmass_sp, function(z){
  
  apfun_br <- approxfun(x=z$Date, y=z$branch_mass)
  z$branch_pred <- apfun_br(z$Date)
  
  return(z)
})
brmass_pred <- do.call(rbind, brmass_sp)

brmass_pred$branch_carbon <- brmass_pred$branch_pred * .5


#write to calcualted mass subfolder
write.csv(branch_mass_total, file = "calculated_mass/branch_mass_cwd.csv", row.names=FALSE)
write.csv(brmass_pred, file = "calculated_mass/branch_mass_pred.csv", row.names=FALSE)
