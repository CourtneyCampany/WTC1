# source("functions_and_packages/plot_objects.R")
# source("functions_and_packages/functions.R")
# library(plotrix)

#read data for cumulative C flux and harvest mass
treeC <- read.csv("calculated_mass/chamber_carbon.csv")


#linear model to show fit on graph
tree_mod <- lm(treeC ~ Cflux, data = treeC)
  summary(tree_mod)
  anova(tree_mod)

mab_mod <- lm(Cab ~ Cflux, data = treeC)
  summary(mab_mod)
  anova(mab_mod)


##plot with total flux
palette (c("blue", "red"))

#CO2 flux to harvest mass scatter plot--------------------------------------------------------------------------------------------

# windows (7,7)
par(mar=c(5,6,1,1),las=1, cex.axis=.8,  mgp=c(3,1,0), mfrow=c(2,1), oma=c(5,6,1,1))

#whole tree
par(mar=c(0,0,0,0))
plot(1,type='n', axes=FALSE, ann=FALSE,xlim = c(5000, 30000),ylim = c(5000, 30000))
  mtext(treeclab, side=2,line=4, outer=TRUE, at=.75,las=0, cex=1)
  legend("topleft", leglab2, pch=c(19,1, 19, 1), col=c("blue", "blue", "red", "red"), inset = 0.01, bty='n')
  box()
  axis(2,labels=TRUE, outer=TRUE)
  ablineclip(tree_mod, x1=min(treeC$Cflux),x2=max(treeC$Cflux),lwd=2)
  abline(0, 1, lty=3, lwd=2)
  points(treeC ~ Cflux, data = treeC,pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5)
  text(x=30000, y=27500, labels="(a)", cex=1.25)

#Mab
par(mar=c(0,0,0,0))
plot(1,type='n', ylab = "",xlab="",xlim = c(5000, 30000),ylim = c(5000, 30000))
  mtext(Mablab, side=2,line=4, outer=TRUE, at=.25,las=0, cex=1)
  mtext(treefluxlab, side=1,line=3, outer=TRUE, las=0, cex=1)
  box()
  ablineclip(mab_mod, x1=min(treeC$Cflux), x2=max(treeC$Cflux),lwd=2)
  abline(0, 1, lty=3, lwd=2)
  axis(1,labels=TRUE, outer=TRUE)
  axis(2,labels=TRUE, outer=TRUE)
  points(Cab ~ Cflux, data = treeC,pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5)
  text(x=30000, y=27500, labels="(b)", cex=1.25)
 
 # dev.copy2pdf(file= "master_scripts/paper_figs/flux_treecarbon.pdf")
 # dev.off()


