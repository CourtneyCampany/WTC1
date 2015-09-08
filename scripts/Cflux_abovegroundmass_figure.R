# source("functions_and_packages/plot_objects.R")
# source("functions_and_packages/functions.R")
# library(plotrix)

treeC <- read.csv("master_scripts/harvest_chamber.csv")
  treeC$Date <- as.Date(treeC$Date)

  
diurnalmodel <- lm(treeC ~ CO2cum, data = treeC)
  summary(diurnalmodel)
  anova(diurnalmodel)
  
# getP(diurnalmodel)
  
##plot with total flux
palette (c("blue", "red"))
  
# windows (7,7)
par(mar=c(5,6,1,1),las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0))
  
plot(1,type='n', ylab = "",xlab=treefluxlab,xlim = c(0, 30000),ylim = c(0, 30000))
  
title(ylab=Mablab, mgp=c(4,1,0))
legend("topleft", leglab2, pch=c(19,1, 19, 1), col=c("blue", "blue", "red", "red"), inset = 0.01, bty='n')
ablineclip(diurnalmodel, x1=min(treeC$CO2cum), x2=max(treeC$CO2cum),lwd=2)
abline(0, 1, lty=3, lwd=2)
points(treeC ~ CO2cum, data = treeC,pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5)
  
# dev.copy2pdf(file= "master_scripts/paper_figs/flux_abovemass.pdf")
# dev.off()
  