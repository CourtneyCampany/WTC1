leafcarbon<- read.csv("raw csv/HFE preharvest leaf samples NCSLA.csv")

#remove missing values
leafcarbon <- subset(leafcarbon, !is.na(Cperc))

#get means for each tree (assume that canopy layers and inside/outside are equal)
leafC_agg <- aggregate(cbind(SLA,Cperc) ~ chamber, data = leafcarbon, FUN = mean)

#renames and keep only chambers 1-12
names(leafC_agg)[3]<- "leafpercC"
leafC_agg <- droplevels(leafC_agg[1:12, ])

write.csv(leafC_agg, "calculated mass/leaf percent carbon.csv", row.names=FALSE)




