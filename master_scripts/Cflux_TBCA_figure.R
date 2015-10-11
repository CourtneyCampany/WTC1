##this scrpt visualizes the relationship between leaf area and TBCA and Cflux by treatment
source("functions_and_packages/plot_objects.R")
source("functions_and_packages/functions.R")
library(doBy)

##read in chamber flux and calculate TBCA
#treeC <- read.csv("calculated_mass/treeC_day.csv")
treeC <- read.csv("master_scripts/Cflux_day_trt.csv")
  treeC$Date <- as.Date(treeC$Date)
    
  treeC$TBCA <- with(treeC, fluxC-aboveC)
  
  treeC$treatment <-with(treeC, paste(CO2_treatment, Water_treatment, sep="-"))
  
    
###plot relationships of TBCA and leaf area with CO2 flux---------------------------------------------------------------------
palette(c("blue", "red"))
 
# windows(7,7)
par(mar=c(5,6,1,1),las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0))
plot(TBCA ~ fluxC, data=treeC,  type='n',ylab="", xlab=treefluxlab2)

for(i in unique(treeC$treatment)){
  points(TBCA ~ fluxC, data=treeC[treeC$treatment == i,], col=as.factor(CO2_treatment), 
         type='l', lwd=2.5, lty=c(2,1)[as.factor(Water_treatment)])
}

title(ylab=tbcalab, mgp=c(4,1,0))
legend("topleft", trtlab,  lty=c(1, 1, 1,2), col=c("blue", "red", "black", "black"), bty='n', inset=0.01, lwd=2)
abline(0, 1, lty=3, lwd=2)

# dev.copy2pdf(file="master_scripts/paper_figs/tbca_cflux.pdf")
# dev.off()


##can use "master_scripts/Cflux_day.csv")
###all WTC-----------------
# windows(7,7)
# par(mar=c(5,6,1,1),las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0))
# plot(TBCA~CO2cum, data=flux_la,  type='n',ylab="", xlab=treefluxlab)
# 
# for(i in unique(flux_la$treatment)){
#   points(TBCA~CO2cum, data=flux_la[flux_la$treatment == i,], col=as.factor(CO2_treatment), 
#          type='l', lwd=2.5, lty=c(1,2)[as.factor(Water_treatment)])
# }
# 
# title(ylab=tbcalab, mgp=c(4,1,0))
# legend("topleft", trtlab,  lty=c(1, 1, 1,2), col=c("blue", "red", "black", "black"), bty='n', inset=0.01, lwd=2)
# abline(0, 1, lty=2, lwd=2)

