#this script caluclates a density metric to measure branch volmue through time with final harvest data
#only inlcudes branches (>1cm), twigs are estimated seperately

setwd("C:/Users/90919620/Google Drive/HFE Database")

# chamber treatments
chambersumm <- read.csv("HFE chamber treatments.csv")

##write in and merge harvest branch mass with volume
#only use brannches (>1cm)
branchM <- read.csv("HFE final harvest biomass by layer.csv")
branchM <- subset(branchM, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
branchM <- subset(branchM, select =c("chamber",  "layerno", "Wbrgt1"))
names(branchM)[3] <- "br_mass"

#use branch data from harvest date to get density (Pb) form mass and volume equations
###assuming branches as cyclinders, adj with form factor of .75(cone= 2/3's of cyldiner(1))
branch_diam <- read.csv("HFE branch diameter length.csv")
branch_diam<-subset(branch_diam, branch_diam$Date == "2009-03-16")
branch_diam <- subset(branch_diam, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
branch_diam <- subset(branch_diam, select = c("Date", "chamber", "stemnumber", "branchnumber", "diameter", "length", "branchBA"))
branch_diam <- branch_diam[complete.cases(branch_diam),]
branch_diam$Date <- as.character(branch_diam$Date)
branch_diam$Date <- as.Date(branch_diam$Date)
branch_diam$startlength <- 5
branch_diam$Volume <- (branch_diam$branchBA*(branch_diam$length+5))*.75 # add 5cm to height, assuming no difference between base and insertion diam
row.names(branch_diam)<-NULL
write.csv(branch_diam, file = "HFE branch volume_2009-03-16_cec.csv")

#sum data for harvest branch mass(by layer) an volume (branch#)
BRmass_mean<- aggregate(br_mass ~ chamber, data = branchM, FUN = sum)
BRmass_mean <- subset(BRmass_mean, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])

branch_volume <- aggregate(Volume ~ chamber, data = branch_diam, FUN = sum)
branch_volume <- merge(branch_volume, chambersumm, by = "chamber")

#calculate chamber branch denisty from Mass/Volume (shape factor within volume equation)
#assumer no difference between bark and wood density of branches
branchD <- merge(branch_volume, BRmass_mean)
branchD$branch_density <- branchD$br_mass / branchD$Volume
branchD <- subset(branchD, select = c("chamber" , "branch_density"))
write.csv(branchD, file = "HFE branch density_harvest_cec.csv")

#calucalte branch mass through time
#as before assumer shape factor of .75 and add 5cm to length(assumer no taper)
branch_allom <- read.csv("HFE branch diameter length.csv")
branch_allom <- subset(branch_allom, select = c("Date",  "chamber",	"stemnumber",	"branchnumber",	"diameter",	"length",	"branchBA"))
branch_allom <- merge(branch_allom, branchD, by = "chamber")
branch_allom$branch_mass <- ((branch_allom$branchBA*(branch_allom$length+5))*.75) * branch_allom$branch_density
#sum data
branch_mass_total <- aggregate(branch_mass ~ chamber + Date, data= branch_allom, FUN = sum)
write.csv(branch_mass_total, file = "HFE branch mass allometry_cec.csv")



