source("functions_and_packages/plot_objects.R")
source("functions_and_packages/functions.R")


##need to determine final leaves mass for correlations with GPP and TBCA

leafarea <- read.csv("raw csv/HFE LA estimates alldates.csv")
leafarea$Date <- as.Date(leafarea$Date)

## treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
chambersumm <- droplevels(chambersumm[,1:3])

leafarea <- merge(leafarea, chambersumm)

leafarea_final <- leafarea[leafarea$Date == max(leafarea$Date),]

##Read in total chamber flux
treeC <- read.csv("master_scripts/harvest_chamber.csv")
  treeC$Date <- as.Date(treeC$Date)
  
###merge leaf area with chamber flux

leafflux <- merge(leafarea_final[,c(1,7:9)],treeC[, c(1,11)] )  

#   library(nortest) 
#   ad.test(leafflux$LAestlin)
leaffluxlmodel <- lm(LAestlin ~ CO2cum, data = leafflux)
getP(leaffluxlmodel)


##plot LA with total flux-----------------------------------------------------------------------------
palette (c("blue", "red"))

#windows (8,6)
par(mar=c(5,6,1,1),las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0))

plot(1,type='n', ylab = "",
     xlab=treefluxlab,
     xlim = c(0, 30000),
     ylim = c(0, 100))

title(ylab=leaflab, mgp=c(4,1,0))
legend("topleft", leglab2, pch=c(19,1, 19, 1), col=c("blue", "blue", "red", "red"), inset = 0.01)
abline(leaffluxlmodel, lwd=2)
points(LAestlin ~ CO2cum, data = leafflux,pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5)

mtext("p =0.001, R = 0.60", side = 1, line = -3, cex = 1)

# dev.copy2pdf(file= "master_scripts/paper_figs/flux_leafarea.pdf")
# dev.off()





  