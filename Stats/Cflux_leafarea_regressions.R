# source("functions_and_packages/plot_objects.R")
# source("functions_and_packages/functions.R")
library(plotrix)
library(doBy)

## treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])
  
##Read in total chamber flux and leaf area
treeC <- read.csv("calculated_mass/chamber_carbon.csv")

leafarea <- read.csv("raw csv/HFE LA estimates alldates.csv")
  leafarea$Date <- as.Date(leafarea$Date)
  
###plot needs to have mean leaf area over all dates on yaxis  
la_agg <- summaryBy(LAestlin ~ chamber, data=leafarea, FUN=mean, keep.names = TRUE)
la_agg <- merge(la_agg, chambersumm)

###merge leaf area with chamber flux
la_flux <- merge(la_agg,treeC[, c(1:2)] )  

leaffluxmod_amb <- lm(Cflux~LAestlin, data = la_flux[la_flux$CO2_treatment == "ambient",])
leaffluxmod_elev <- lm(Cflux~LAestlin, data = la_flux[la_flux$CO2_treatment == "elevated",])

amb_dat <- la_flux[la_flux$CO2_treatment == "ambient",]
amb_new <- seq(min(amb_dat$LAestlin), max(amb_dat$LAestlin), length=101)
amb_pred <- predict(leaffluxmod_amb, newdata=data.frame(LAestlin=amb_new), se.fit=TRUE)

ambupr <- amb_pred$fit + (2*amb_pred$se.fit)
amblwr <- amb_pred$fit - (2*amb_pred$se.fit)


ele_dat <- la_flux[la_flux$CO2_treatment == "elevated",]
ele_new <- seq(min(ele_dat$LAestlin), max(ele_dat$LAestlin), length=101)
ele_pred <- predict(leaffluxmod_elev, newdata=data.frame(LAestlin=ele_new), se.fit=TRUE)

eleupr <- ele_pred$fit + (2*ele_pred$se.fit)
elelwr <- ele_pred$fit - (2*ele_pred$se.fit)



##plot LA with total flux-----------------------------------------------------------------------------
palette (c("blue", "red"))

windows (7,7)
par(mar=c(5,6,1,1),las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0))

plot(1,type='n', ylab = "",xlab=meanlalab,ylim = c(5000, 30000), xlim = c(0, 50))

title(ylab=treefluxlab2, mgp=c(4,1,0))
legend("topleft", leglab2, pch=c(19,1, 19, 1), col=c("blue", "blue", "red", "red"), inset = 0.01, bty='n')
ablineclip(leaffluxmod_amb, x1=min(amb_dat$LAestlin),x2=max(amb_dat$LAestlin),lwd=2, col="blue")
ablineclip(leaffluxmod_elev, x1=min(ele_dat$LAestlin),x2=max(ele_dat$LAestlin),lwd=2, col="red")

lines(ele_new, eleupr, lty=2, lwd=2,col="red")
lines(ele_new, elelwr, lty=2, lwd=2,col="red")
lines(ele_new, ele_pred$fit, lty=1, lwd=2,col="red")

lines(amb_new, ambupr, lty=2, lwd=2,col="blue")
lines(amb_new, amblwr, lty=2, lwd=2,col="blue")
lines(amb_new, amb_pred$fit, lty=1, lwd=2,col="blue")


points(Cflux ~ LAestlin, data = la_flux,pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5)

dev.copy2pdf(file= "stats/flux_leafarea_co2.pdf")
dev.off()





  