##this scrpt visualizes the relationship between leaf area and TBCA and Cflux by treatment
source("functions_and_packages/plot_objects.R")
source("functions_and_packages/functions.R")
library(doBy)
library(scales)

##read in chamber flux and calculate TBCA
#treeC <- read.csv("calculated_mass/treeC_day.csv")
treeC <- read.csv("master_scripts/Cflux_day_trt.csv")
  treeC$Date <- as.Date(treeC$Date)
    
  treeC$TBCA <- with(treeC, fluxC-aboveC)
  
  treeC$treatment <-with(treeC, paste(CO2_treatment, Water_treatment, sep="-"))
  
# treeC2 <- treeC[complete.cases(treeC),]  
  
# drystart <- treeC[treeC$Date == "2008-10-01" & treeC$treatment == "ambient-dry","fluxC"]
# dryend <- treeC[treeC$Date == "2009-02-28"& treeC$treatment == "ambient-dry","fluxC"]
  
###how constat for abstract  
# treeC2 <- treeC[complete.cases(treeC),]  
# treeC3 <- treeC2[treeC2$TBCA > 0,]
# treeC3$tbcaperc <- with(treeC3, (fluxC-TBCA)/fluxC) 
# tbca_agg <- summaryBy(tbcaperc ~ treatment, data=treeC3, FUN=c(mean,se))
# mean(treeC3$tbcaperc)
     
###plot relationships of TBCA and leaf area with CO2 flux---------------------------------------------------------------------
palette(c("blue", "red"))
droughtcol = alpha("lightslategrey", alpha=0.275)

# windows(7,7)
par(mar=c(5,6,1,1),las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0))
plot(TBCA ~ fluxC, data=treeC,  type='n',ylab="", xlab=treefluxlab2, yaxs="i", xaxs="i", ylim=c(0, 12500), xlim=c(0, 25500))
for(i in unique(treeC$treatment)){
  points(TBCA ~ fluxC, data=treeC[treeC$treatment == i,], col=as.factor(CO2_treatment), 
         type='l', lwd=2.5, lty=c(2,1)[as.factor(Water_treatment)])
}


title(ylab=tbcalab, mgp=c(4,1,0))
legend("topleft", trtlab,  lty=c(1, 1, 1,2), col=c("blue", "red", "black", "black"), bty='n', inset=0.01, lwd=2)
#abline(0, .5, lty=3, lwd=2.5)

 # dev.copy2pdf(file="master_scripts/paper_figs/tbca_cflux.pdf")
 # dev.off()



###all WTC-----------------
##can use "master_scripts/Cflux_day.csv")
# tbca_wtc <- read.csv("master_scripts/Cflux_day.csv")
#   tbca_wtc$Date <- as.Date(tbca_wtc$Date)
#   tbca_wtc$TBCA <- with(tbca_wtc, fluxC-aboveC)
#   tbca_wtc$treatment <-with(tbca_wtc, paste(CO2_treatment, Water_treatment, sep="-"))
# 
# windows(7,7)
# par(mar=c(5,6,1,1),las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0))
# plot(TBCA~fluxC, data=tbca_wtc,  type='n',ylab="", xlab=treefluxlab,yaxs="i", xaxs="i", ylim=c(0, 12500), xlim=c(0, 25500))
# 
# for(i in unique(tbca_wtc$treatment)){
#   points(TBCA~fluxC, data=tbca_wtc[tbca_wtc$treatment == i,], col=as.factor(CO2_treatment),
#          type='l', lwd=2.5, lty=c(1,2)[as.factor(Water_treatment)])
# }
# 
# title(ylab=tbcalab, mgp=c(4,1,0))
# legend("topleft", trtlab,  lty=c(1, 1, 1,2), col=c("blue", "red", "black", "black"), bty='n', inset=0.01, lwd=2)
# 
