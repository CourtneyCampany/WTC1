#all raw data here:
source("HFE chamber read data.R")

#LEAF MASS
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
write.csv(leafA_est, file = "calculated mass/Leaf Mass.csv", row.names=FALSE)
