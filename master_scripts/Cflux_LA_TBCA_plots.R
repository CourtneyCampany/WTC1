##this scrpt visualizes the relationship between leaf area and TBCA and Cflux by treatment
source("functions_and_packages/plot_objects.R")
library(doBy)

## treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
chambersumm <- droplevels(chambersumm[,1:3])

##read in and format Leaf Area
leafarea <- read.csv("raw csv/HFE LA estimates alldates.csv")
  leafarea$Date <- as.Date(leafarea$Date)
  leafarea <- merge(leafarea, chambersumm)
  ##order by date
  leafarea <- leafarea[order(leafarea$Date),]
  ##add unique treamtent
  leafarea$treatment <- with(leafarea, paste(CO2_treatment, Water_treatment, sep="-"))

##summary leaf area by treatment  
leafarea_agg <- summaryBy(LAestlin ~ Date+treatment, data=leafarea, FUN=mean, keep.names=TRUE)
  

##read in chamber flux and calculate TBCA
  treeC <- read.csv("calculated_mass/treeC_day.csv")
  treeC$Date <- as.Date(treeC$Date)
  treeC$TBCA <- with(treeC, CO2cum-aboveC)
  treeC$leafC <- with(treeC, aboveC- bolebranch)
  
##subset a simplificed dfr
fluxC <- treeC[, c(1:3, 11:12)]

##merge flux data and leaf area 
flux_la <- merge(fluxC, leafarea_agg, all=FALSE)


###plot relationships of TBCA and leaf area with CO2 flux
palette(c("black", "blue", "red", "forestgreen"))

windows(7,7)

plot(LAestlin~CO2cum, data=flux_la,  type='n',ylab=leaflab, xlab=treefluxlab)

for(i in unique(flux_la$treatment)){
  points(LAestlin~CO2cum, data=flux_la[flux_la$treatment == i,], col=treatment, type='l', lwd=2)
}





