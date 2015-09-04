source("functions_and_packages/load_packages.R")
source("functions_and_packages/functions.R")

#read calculated data for each component----------------------------------------------------------------------------------------

# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

#interpolated aboveground components and fluxes
fluxmass <- read.csv("whole_tree_csv/tree_C_flux.csv")

#roots
rootmass <- read.csv("calculated_mass/root_mass.csv")
rootmass$Date <- as.Date("2009-03-16")


#CALCULATE treatment means for each tree component and cumulative chamber flux on harvest date----------------------------------

#merge data with treatment summary and add unique treatment factor
fluxmass <- merge(fluxmass, chambersumm, by = "chamber")
  fluxmass$treatment <- as.factor(paste(fluxmass$CO2_treatment, fluxmass$Water_treatment, sep="-"))

rootC <- rootmass[, c(1,7:8)]

#Date order for ease of viewing
chamberorder<-order(fluxmass$chamber, by=fluxmass$Date)
fluxmass <- fluxmass[chamberorder,]

#sum treeC 
fluxmass$treeC <- with(fluxmass, boleC+branchC+leafC+litterC)

#--------------------------------------------------------------------------------------------------------
######chamber flux ends on mar6, probably due to harvest, so use that dates cumsum to go with harvest data
flux_last <- subset(fluxmass, Date=="2009-03-06", select = c("chamber", "CO2cum"))

final_fluxmass <- fluxmass[fluxmass$Date == "2009-03-16", -(3:4)]
  final_fluxmass <- merge(final_fluxmass, flux_last)
  final_fluxmass$leafC_litterC <- with(final_fluxmass, leafC+litterC)
  final_fluxmass <- merge(final_fluxmass, rootC)
  
###write these chamber finals for TBCA
write.csv(final_fluxmass, "master_scripts/harvest_chamber.csv", row.names = FALSE) 


#summary tables
treefluxC_means <- ddply(final_fluxmass, .(treatment), summarize,
       boleC = round(mean(boleC), 2),                    
       branchC = round(mean(branchC), 2),
       leafC = round(mean(leafC_litterC), 2),
       treeC = round(mean(treeC), 2),
       CO2 = round(mean(CO2cum), 2),
       FrootC = round(mean(frootC_all),2),
       CrootC = round(mean(CrootC), 2))
       
treefluxC_se <- ddply(final_fluxmass, .(treatment), summarize,
        boleC_se = round(se(boleC), 3), 
        branchC_se = round(se(branchC), 3),
        leafC_se = round(se(leafC_litterC), 3),
        treeC_se = round(se(treeC), 3),
        CO2_se = round(se(CO2cum), 3),
        FrootC_se = round(se(frootC_all),3),
        CrootC_se = round(se(CrootC), 3))

treeC_stats <- merge(treefluxC_means, treefluxC_se)
 
write.csv(treeC_stats, "master_scripts/harvest_trt_means.csv", row.names = FALSE)

