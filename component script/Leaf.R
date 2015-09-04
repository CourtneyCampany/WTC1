###read in chamber summary, harvest mass with leaf mass, leaf area est--------------------------------------------------------

#chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

#final harvest mass
harvest_mass <- read.csv("raw csv/HFE final harvest biomass by layer.csv")
  harvest_mass <- subset(harvest_mass, chamber %in% unique(chambersumm$chamber))
  harvest_mass <- droplevels(harvest_mass)
  
#leaf area estimates
leafA_est <- read.csv("raw csv/HFE LA estimates alldates.csv")
  leafA_est$Date <- as.Date(leafA_est$Date)
  
#LEAF MASS over final year-------------------------------------------------------------------------------------------
  
# from harvest calculate SLA, assume SLA is constant through time to calculate mass from area
leafM <- subset(harvest_mass, select =c("chamber",  "layerno", "LA", "Wleaf"))
  names(leafM)[3:4] <- c("leafarea", "leafmass")

##tree totals(sum layers)
leafM_agg <- aggregate(cbind(leafarea, leafmass) ~ chamber, FUN = sum, data = leafM)
leafM_agg <-  merge(leafM_agg, chambersumm)

# SLA in m2kg-1
leafM_agg$SLA <- with(leafM_agg, leafarea / (10^-3 * leafmass))
sladfr <- leafM_agg[,c("chamber","SLA")]

#calculate leaf mass through time
leafA_est <- merge(leafA_est, sladfr, by="chamber")
leafA_est$leafmass <- with(leafA_est, (LAestlin / SLA)*1000)


#write leaf mass to subfolder
write.csv(leafA_est, file = "calculated_mass/Leaf Mass.csv", row.names=FALSE)
