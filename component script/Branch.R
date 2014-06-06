
#BRANCH MASS

#all raw data here:
source("HFE chamber read data.R")
library(plyr)

#mass is sum of harvest branches with diameter < & > than 1cm 
branchM <- subset(harvest_mass, select =c("chamber",  "layerno", "Wbrgt1" ,"Wbrlt1"))
branchM$br_mass <- with(branchM, Wbrgt1 + Wbrlt1)

#caculate branch volume from harvest branch diameterm length, and.75 shape factor
br_volume_calc<- function(dfr) {
  volume_calc <- subset(dfr, dfr$Date == "2009-03-16")
  volume_calc <- subset(volume_calc, select = c("Date", "chamber", "stemnumber", "branchnumber", "diameter", "length", "branchBA"))
  volume_calc <- volume_calc[complete.cases(volume_calc),]
  #add 5cm to height, assuming no difference taper btw base and insertion diam
  volume_calc$Volume <- (volume_calc$branchBA*(volume_calc$length+5))*.75 
  row.names(volume_calc)<-NULL
  return(volume_calc)
}
br_vol <- br_volume_calc(branch_allometry)

#total harvest branch mass(by layer) an volume (branch#)
br_mass_tot <- ddply(branchM, c("chamber"), function(x) data.frame(br_mass=sum(x$br_mass)))
br_vol_tot <- ddply(br_vol, c("chamber"), function(x) data.frame(vol_tot=sum(x$Volume)))

#calculate branch density
br_density_calc <- function(volume, mass) {
  density_calc <- merge(volume, mass, by = "chamber")
  density_calc$branch_density <- density_calc$br_mass / density_calc$vol_tot
  #assume no difference between bark and wood density of branches
  density_calc <- subset(density_calc, select = c("chamber" , "branch_density"))
}

br_density <- br_density_calc(br_vol_tot, br_mass_tot)

#branch mass across dates
br_mass_dates_calc <- function(allometry, density){
  allom_dates <- subset(allometry, select = c("Date",  "chamber",  "stemnumber",  "branchnumber",
                                              "diameter",	"length",	"branchBA"))
  mass_dates <- merge(allom_dates, density, by = "chamber")
  mass_dates$branch_mass <- ((mass_dates$branchBA*(mass_dates$length+5))
                              *.75)*mass_dates$branch_density
  #as before assumes shape factor of .75 and adds 5cm to length w/ no taper
  return(mass_dates)                           
}

br_mass_dates <-  br_mass_dates_calc(branch_allometry, br_density)
 

#total branch mass by chamber for each date
branch_mass_total <- aggregate(branch_mass ~ chamber + Date, data = br_mass_dates, FUN = sum)

#brd <- ddply(br_mass_dates, .(chamber,Date), function(x) data.frame(branch_mass=sum(x$branch_mass)))

#plot(branch_mass_total$Date, branch_mass_total$branch_mass)

#write to calcualted mass subfolder
write.csv(branch_mass_date, file = "calculated mass/branch mass.csv", row.names=FALSE)
