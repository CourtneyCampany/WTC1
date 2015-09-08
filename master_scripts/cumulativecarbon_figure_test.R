source("functions_and_packages/plot_objects.R")
library(doBy)

## treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

#treeC <- read.csv("calculated_mass/treeC_day.csv")
#carbon data set
carbon_pred <- read.csv("whole_tree_csv/tree_C_flux.csv")

carbon_pred2 <- carbon_pred[,1:7]
  carbon_pred2$Date <- as.Date(carbon_pred2$Date)
  carbon_pred2$bolebranch <- with(carbon_pred2, bole_carbon+branch_carbon)
  carbon_pred2$aboveC <- with(carbon_pred2, leafcarbon+littercarbon+bolebranch)
  
  carbon_pred2 <- merge(carbon_pred2, chambersumm)
  carbon_pred2 <- carbon_pred2[order(carbon_pred2$Date),]

carbon_pred_agg <- summaryBy(.~ CO2_treatment+Water_treatment+Date, data= carbon_pred2, FUN=mean, keep.names = TRUE)
carbon_pred_agg$treatment <- with(carbon_pred_agg, as.factor(paste(CO2_treatment,Water_treatment, sep="-")))

##for ease of plotting seperate data by trt
ambdry <- carbon_pred_agg[carbon_pred_agg$treatment == "ambient-dry",]
  ambdry$Date <- as.Date(ambdry$Date)

ambwet <- carbon_pred_agg[carbon_pred_agg$treatment == "ambient-wet",]
  ambwet$Date <- as.Date(ambwet$Date)

elevdry <- carbon_pred_agg[carbon_pred_agg$treatment == "elevated-dry",]
  elevdry$Date <- as.Date(elevdry$Date)

elevwet <- carbon_pred_agg[carbon_pred_agg$treatment == "elevated-wet",]
  elevwet$Date <- as.Date(elevwet$Date)
  
  
##add roots and calculate final mass + fine and coarse roots
roots <- read.csv("master_scripts/harvest_trt_means.csv")
  roots$Date <- as.Date("2009-03-16")
  roots$treeC <- with(roots, aboveC+rootC)
  
lasttree <- carbon_pred_agg[carbon_pred_agg$Date == max(carbon_pred_agg$Date),]
lasttree2 <- merge(lasttree[,c(1:3,10:11)], roots[, c(1,7)])
lasttree2$treeC <- with(lasttree2, aboveC+rootC)

##plot bits
xAT <- seq.Date(from=as.Date("2008-4-1"), length=13, by="month")
LWD <- 2

##plot four panel with treatment means---------------------------------------------------------------------------------
windows (7,10)

par(mfrow=c(2,2), las=1, mgp=c(3,1,0), oma=c(4,6,1,1))

#1: ambient-wet
par(mar=c(0,0,0,0))
plot(CO2cum ~ Date, data = ambwet,axes=FALSE, ylab="Carbon  (g)", ylim=c(0, 25000),lwd=LWD, type = 'l')
  points(bole_carbon ~ Date,  data = ambwet,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = ambwet, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = ambwet, lwd=LWD,type = 'l',lty=5)
box()
legend(x=13975, y=23500, dayClab, lty = c(1, 3, 2, 5, -1), pch = c(-1, -1, -1, -1, 21),lwd=LWD,  
        bty='n', pt.cex=1,cex=1, pt.bg=c(-1, -1, -1, -1,"grey"))
text(x=14025, y=25000, label = leglab2[1], cex=1.25, font=2) 
##add roots on last date
points(treeC ~ Date , data = lasttree2,subset = treatment == "ambient-wet", pch = 21, bg = "grey", cex=2)

axis(2, labels=TRUE, outer=FALSE)
mtext("Carbon  (g)", side=2, line=4, outer=TRUE, las=0, at=.75)

#2: ambient-dry
par(mar=c(0,0,0,0))
plot(CO2cum ~ Date, data = ambdry,axes = FALSE, ann = FALSE, ylim=c(0, 25000),lwd=LWD, type = 'l')
  points(bole_carbon ~ Date,  data = ambdry,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = ambdry, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = ambdry, lwd=LWD,type = 'l',lty=5)
box()
text(x=14025, y=25000, label = leglab2[2],cex=1.25, font=2)

points(treeC ~ Date , data = lasttree2,subset = treatment == "ambient-dry", pch = 21, bg = "grey", cex=2)

#3: elevated-dry
par(mar=c(0,0,0,0))
plot(CO2cum ~ Date, data = elevdry, axes=FALSE, xlab="", ylab="Carbon  (g)", ylim=c(0, 25000), lwd=LWD, type = 'l')
  points(bole_carbon ~ Date,  data = elevdry,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = elevdry, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = elevdry, lwd=LWD,type = 'l',lty=5)
box()
text(x=14025, y=25000, label = leglab2[3],cex=1.25, font=2)
axis.Date(1, at = xAT, labels = TRUE, outer=FALSE)
axis(2, labels=TRUE, outer=FALSE)
mtext("Carbon  (g)", side=2, line=4, outer=TRUE, las=0, at=.25)

points(treeC ~ Date , data = lasttree2,subset = treatment == "elevated-dry", pch = 21, bg = "grey", cex=2)

#4: elevated-wet
par(mar=c(0,0,0,0))
plot(CO2cum ~ Date, data = elevwet,axes = FALSE, ylab="", xlab="", ylim=c(0, 25000),lwd=LWD, type = 'l')
  points(bole_carbon ~ Date,  data = elevwet,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = elevwet, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = elevwet, lwd=LWD,type = 'l',lty=5)
axis.Date(1, at = xAT, labels = TRUE)
box()
text(x=14025, y=25000, label = leglab2[4],cex=1.25, font=2)

points(treeC ~ Date , data = lasttree2,subset = treatment == "elevated-wet", pch = 21, bg = "grey", cex=2)
axis.Date(1, at = xAT, labels = TRUE, outer=FALSE)

dev.copy2pdf(file= "master_scripts/paper_figs/treecarbon_daily_notreset.pdf")
dev.off()

