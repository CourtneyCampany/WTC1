source("functions_and_packages/plot_objects.R")
source("functions_and_packages/functions.R")

treeC <- read.csv("master_scripts/harvest_chamber.csv")
  treeC$Date <- as.Date(treeC$Date)

  
#linear model to show fit on graph
#   library(nortest) 
#   ad.test(treeC$treeC)
#   ad.test(treeC$CO2cum)  
  
diurnalmodel <- lm(treeC ~ CO2cum, data = treeC)
  summary(diurnalmodel)
  anova(diurnalmodel)
  
# modeltrt<- lm(treeC ~ CO2cum*CO2_treatment*Water_treatment, data = treeC)
#   anova(modeltrt)  
###no treatment affects on correlation between co2flux and mass above ground
  
getP(diurnalmodel)
  
##plot with total flux
palette (c("blue", "red"))
  
 # windows (8,6)
par(mar=c(5,6,1,1),las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0))
  
plot(1,type='n', ylab = "",
       xlab=treefluxlab,
       xlim = c(0, 30000),
       ylim = c(0, 30000))
  
title(ylab=Mablab, mgp=c(4,1,0))
legend("topleft", leglab2, pch=c(19,1, 19, 1), col=c("blue", "blue", "red", "red"), inset = 0.01)
abline(diurnalmodel, lwd=2)
abline(0, 1, lty=3, lwd=2)
points(treeC ~ CO2cum, data = treeC,pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5)
  
mtext("p < 0.0001, R = 0.69", side = 1, line = -3, cex = 1)
  
# dev.copy2pdf(file= "master_scripts/paper_figs/flux_abovemass.pdf")
# dev.off()
  