###read in chamber summary, harvest mass with leaf mass, leaf area est--------------------------------------------------------

#chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

#final harvest mass
harvest_mass <- read.csv("raw csv/HFE final harvest biomass by layer.csv")
  harvest_mass <- subset(harvest_mass, chamber %in% unique(chambersumm$chamber))
  harvest_mass <- droplevels(harvest_mass)
  
#missing the extra leaf material (add back)
extra_mass <- read.csv("raw csv/HFE extra plant mass.csv")
  extra_mass$xleaf <- with(extra_mass,damage_leaf+ removed_leaf)
  
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

########add in extra mass to total
leafM_agg2 <- merge(leafM_agg, extra_mass[, c(1,10)])
leafM_agg2$leafmass2 <- with(leafM_agg2, leafmass+xleaf)

# SLA in m2kg-1
leafM_agg2$SLA <- with(leafM_agg2, leafarea / (10^-3 * leafmass2))
sladfr <- leafM_agg2[,c("chamber","SLA")]

#calculate leaf mass through time
leafA_est <- merge(leafA_est, sladfr, by="chamber")
  leafA_est$leafmass <- with(leafA_est, (LAestlin / SLA)*1000)
  leafA_est$leafmass_pot <- with(leafA_est, (LApotlin / SLA)*1000)
  #calculate leaf litter mass with sla
  leafA_est$littermass <- with(leafA_est, (LAlittercumlin / SLA)*1000)

##check against harvest
 leaflast <- leafA_est[leafA_est$Date == max(leafA_est$Date),]
 ##equal so did it correctly

#write leaf mass to subfolder
write.csv(leafA_est, file = "calculated_mass/leaf_mass.csv", row.names=FALSE)
