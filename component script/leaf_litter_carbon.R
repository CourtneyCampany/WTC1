###calculate leaf and litter percent carbon

leaf <- read.csv("calculated_mass/leaf_mass.csv")
leafpercC <- read.csv("calculated_mass/leaf_percent_carbon.csv")

##from before this there is no leaf count, sla was calcualated on a canopy basis from havest mass and area
##then used to calculated leaf mass from leaf area estimates through time

##here we use that mass * leaf C% and use same leaf C percent on litter because they were collected fresh

leaf_carbon <- merge(leaf, leafpercC[, c(1,3)])
leaf_carbon$Date <- as.Date(leaf_carbon$Date)

leaf_carbon$leafcarbon <- with(leaf_carbon, leafmass *(leafpercC/100))
leaf_carbon$leafpot_carbon <- with(leaf_carbon, leafmass_pot *(leafpercC/100))
leaf_carbon$littercarbon <- with(leaf_carbon, littermass *(leafpercC/100))

#order by date
leaf_carbon <-leaf_carbon[order(leaf_carbon$Date),]

##save in calculated mass, same as leaf mass csv just now has leaf litter C
litter_carbon <- leaf_carbon[, c(1:2,6,11,15)]

write.csv(leaf_carbon, "calculated_mass/leaf_carbon.csv", row.names=FALSE)
write.csv(litter_carbon, "calculated_mass/litter_carbon.csv", row.names=FALSE)
