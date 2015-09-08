source("functions_and_packages/functions.R")
library(doBy)


##take last year flux data and clean it up and make treatment avgs

## treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

Cyear <- read.csv("whole_tree_csv/tree_C_flux.csv")
Cyear$Date <- as.Date(Cyear$Date)
Cyear <- merge(Cyear, chambersumm)

chamberorder<-order(Cyear$chamber, by=Cyear$Date)
Cyear <- Cyear[chamberorder,]



##extract the columns that are reset to zero
Cyear2 <- Cyear[, c(1:2, 13:19)] 
Cyear2$aboveC <- with(Cyear2, branchC+leafC+boleC+litterC)

#test <- Cyear2[Cyear2$chamber == "ch01",]

##treatment means by Date
Cyear_trt <- summaryBy(. ~ Date+CO2_treatment+Water_treatment, data=Cyear2, FUN=mean, keep.names=TRUE)


#last year carbon data
write.csv(Cyear2, file = "master_scripts/Cflux_day.csv", row.names=FALSE)
#last year carbon data treatment means
write.csv(Cyear_trt, file = "master_scripts/Cflux_day_trt.csv", row.names=FALSE)
