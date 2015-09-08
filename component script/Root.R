library(plyr)
library(doBy)

#read data
# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

#harvest dry mass totals
treemass <- read.csv("raw csv/HFE final DM totals.csv")


######Here we redo root mass by summing harvest roots and adding back cores-----------------------------------------

###first get standing root crop from harvest
root_mass <- treemass[, c(1,15)]
names(root_mass)[2] <- "root_harvest"

###the roots sampled from cores (we will not be scaling)
root_cores <- read.csv("raw csv/HFE Fine root cores final harvest.csv")
  root_cores <- subset(root_cores, chamber %in% unique(chambersumm$chamber))
  root_cores <- droplevels(root_cores)  
  root_cores$root_total <- with(root_cores, veryfine+wrlt2+wr25+wr510+wrgt10)  

##sum roots by core and depth to get chamber

root_core_sum <- summaryBy(root_total ~ chamber, data=root_cores, FUN=sum, keep.names = TRUE)

###merge havest and sample roots from cores
root_cham <- merge(root_mass, root_core_sum)
root_cham <- merge(root_cham, chambersumm)
root_cham$treatment <- with(root_cham, paste(CO2_treatment, Water_treatment, sep="-"))  
root_cham$root_mass <- with(root_cham, root_harvest+root_total)
 

write.csv(root_cham[,c(1,7,4:6)], file = "calculated_mass/root_mass_simple.csv", row.names=FALSE)

