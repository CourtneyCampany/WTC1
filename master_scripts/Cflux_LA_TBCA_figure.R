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
  flux_la$CO2_treatment <- gsub("-dry", "", flux_la$treatment)
  flux_la$CO2_treatment <- gsub("-wet", "", flux_la$CO2_treatment)
  flux_la$CO2_treatment <- as.factor(flux_la$CO2_treatment)
  flux_la$Water_treatment <- gsub("ambient-", "", flux_la$treatment)
  flux_la$Water_treatment <- gsub("elevated-", "", flux_la$Water_treatment)
  flux_la$Water_treatment <- as.factor(flux_la$Water_treatment)
  
write.csv(flux_la, "Stats/co2flux_leafarea.csv", row.names = FALSE)
    
###plot relationships of TBCA and leaf area with CO2 flux---------------------------------------------------------------------
palette(c("black", "blue"))

windows(7,7)
#1. leaf area and CO2uptake
par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(LAestlin~CO2cum, data=flux_la,  type='n',ylab=leaflab, xlab=treefluxlab)

for(i in unique(flux_la$treatment)){
  points(LAestlin~CO2cum, data=flux_la[flux_la$treatment == i,], col=as.factor(CO2_treatment), 
         type='l', lwd=2.5, lty=c(1,2)[as.factor(Water_treatment)])
}

legend("topleft", trtlab, pch=c(15, 15, -1, -1), lty=c(-1, -1, 1,2), col=c("black", "blue", "black", "black"), 
       bty='n', inset=0.01)
dev.copy2pdf(file="master_scripts/paper_figs/leafarea_cflux.pdf")
dev.off() 

#2. Leaf area and TBCA
windows(7,7)
par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(TBCA~LAestlin, data=flux_la,  type='n',ylab=tbcalab, xlab=leaflab)

for(i in unique(flux_la$treatment)){
  points(TBCA~LAestlin, data=flux_la[flux_la$treatment == i,], col=as.factor(CO2_treatment), 
         type='l', lwd=2.5, lty=c(1,2)[as.factor(Water_treatment)])
}

legend("topleft", trtlab, pch=c(15, 15, -1, -1), lty=c(-1, -1, 1,2), col=c("black", "blue", "black", "black"), 
       bty='n', inset=0.01)
dev.copy2pdf(file="master_scripts/paper_figs/leafarea_tbca.pdf")
dev.off() 

#3. TBCA and CO2 flux
windows(7,7)
par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(TBCA~CO2cum, data=flux_la,  type='n',ylab=tbcalab, xlab=treefluxlab)

for(i in unique(flux_la$treatment)){
  points(TBCA~CO2cum, data=flux_la[flux_la$treatment == i,], col=as.factor(CO2_treatment), 
         type='l', lwd=2.5, lty=c(1,2)[as.factor(Water_treatment)])
}

legend("topleft", trtlab, pch=c(15, 15, -1, -1), lty=c(-1, -1, 1,2), col=c("black", "blue", "black", "black"), 
       bty='n', inset=0.01)

dev.copy2pdf(file="master_scripts/paper_figs/tbca_cflux.pdf")
dev.off() 
