source("functions_and_packages/plot_objects.R")
library(doBy)

#treeC <- read.csv("calculated_mass/treeC_day.csv")
treeC <- read.csv("master_scripts/Cflux_day_trt.csv")
  treeC$Date <- as.Date(treeC$Date)
  treeC$bolebranch <- with(treeC, boleC+branchC)

treeC$treatment <- with(treeC, paste(CO2_treatment,Water_treatment, sep="-"))


##Read pre-calculated root mass over 11 months from root predict component script
roots <- read.csv("calculated_mass/rootallometry.csv")  

## treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

roots <- merge(roots, chambersumm)  
roots$treatment <- with(roots, paste(CO2_treatment,Water_treatment, sep="-"))
roots_agg <- summaryBy(. ~ treatment, data=roots, FUN=mean, keep.names = TRUE)

finalC <- subset(treeC, Date=="2009-03-16")
  mab_final <- merge(roots_agg[,c(1,4)], finalC[,c(1,9,11)])
  mab_final$treeC <- with(mab_final, root11+aboveC)  
  

##for ease of plotting seperate data by trt
ambdry <- treeC[treeC$treatment == "ambient-dry",]
  ambdry$Date <- as.Date(ambdry$Date)

ambwet <- treeC[treeC$treatment == "ambient-wet",]
  ambwet$Date <- as.Date(ambwet$Date)

elevdry <- treeC[treeC$treatment == "elevated-dry",]
  elevdry$Date <- as.Date(elevdry$Date)

elevwet <- treeC[treeC$treatment == "elevated-wet",]
  elevwet$Date <- as.Date(elevwet$Date)
  
###what was the average percentage of biomass to C flux (use for results) 
  
# totals <- mab_final[,c(1,4:5)]
# fluxtotal <- treeC[treeC$Date=="2009-03-06", c("fluxC", "treatment")]
# totals2 <- merge(totals, fluxtotal)
# 
# totals2$percmass <- with(totals2, 1-((fluxC-treeC)/fluxC))
# totals2$percshoot <- with(totals2, 1-((fluxC-aboveC)/fluxC))
# totals2$tbca <- with(totals2, (fluxC-aboveC)/fluxC)
# ##tbca 
# mean(totals2$tbca)
# se(totals2$tbca)
# range(totals2$tbca)
# 
# ##shoots
# mean(totals2$percshoot)
# se(totals2$percshoot)
# 
# ##whole tree mass
# mean(totals2$percmass)
# se(totals2$percmass)
# 
# ###all dates perc
# tree2 <- treeC[treeC$Date != "2008-04-15", c("Date", "fluxC", "aboveC", "CO2_treatment", "Water_treatment")]
#   tree2 <- tree2[complete.cases(tree2),]
#   tree2$percshoot <- with(tree2, 1-((fluxC-aboveC)/fluxC))
#   tree2 <- tree2[tree2$percshoot <= 1,]
# 
# boxplot(tree2$percshoot)
# mean(tree2$percshoot)
# se(tree2$percshoot)
# with(tree2, plot(percshoot~Date, pch=c(1,19)[Water_treatment],col=CO2_treatment))
# legend("topright", leglab2, pch=c(19,1, 19, 1), col=c("blue", "blue", "red", "red"), inset = 0.01, bty='n')

##plot four panel with treatment means---------------------------------------------------------------------------------

##plot bits
xAT <- seq.Date(from=as.Date("2008-4-1"), length=13, by="month")
LWD <- 2 
  
  
# windows (7,10)

par(mfrow=c(2,2), las=1, mgp=c(3,1,0), oma=c(4,6,1,1))

#1: ambient-wet
par(mar=c(0,0,0,0))
plot(fluxC ~ Date, data = ambwet,axes=FALSE, ylab="Carbon  (g)", ylim=c(0, 25000),lwd=LWD, type = 'l')
  points(boleC ~ Date,  data = ambwet,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = ambwet, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = ambwet, lwd=LWD,type = 'l',lty=5)
box()
legend(x=13975, y=23500, dayClab, lty = c(1, 3, 2, 5, -1), pch = c(-1, -1, -1, -1, 21),lwd=LWD,  
        bty='n', pt.cex=1,cex=1, pt.bg=c(-1, -1, -1, -1,"grey"))
text(x=14035, y=25000, label = leglab2[1], cex=1.25, font=2) 
##add roots on last date
points(treeC ~ Date , data = mab_final,subset = treatment == "ambient-wet", pch = 21, bg = "grey", cex=2)
axis(1, labels=FALSE, tcl=.5)
axis(2, labels=TRUE, outer=FALSE)
mtext("Carbon  (g)", side=2, line=4, outer=TRUE, las=0, at=.75)

#2: ambient-dry
par(mar=c(0,0,0,0))
plot(fluxC ~ Date, data = ambdry,axes = FALSE, ann = FALSE, ylim=c(0, 25000),lwd=LWD, type = 'l')
  points(boleC ~ Date,  data = ambdry,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = ambdry, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = ambdry, lwd=LWD,type = 'l',lty=5)
box()
text(x=14035, y=25000, label = leglab2[2],cex=1.25, font=2)
axis(1, labels=FALSE,tcl=.5)
axis(2, labels=FALSE,tcl=.5)
points(treeC ~ Date , data = mab_final,subset = treatment == "ambient-dry", pch = 21, bg = "grey", cex=2)

#3: elevated-dry
par(mar=c(0,0,0,0))
plot(fluxC ~ Date, data = elevdry, axes=FALSE, xlab="", ylab="Carbon  (g)", ylim=c(0, 25000), lwd=LWD, type = 'l')
  points(boleC ~ Date,  data = elevdry,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = elevdry, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = elevdry, lwd=LWD,type = 'l',lty=5)
box()
text(x=14035, y=25000, label = leglab2[3],cex=1.25, font=2)
axis.Date(1, at = xAT, labels = TRUE, outer=FALSE)
axis(2, labels=TRUE, outer=FALSE)
mtext("Carbon  (g)", side=2, line=4, outer=TRUE, las=0, at=.25)

points(treeC ~ Date , data = mab_final,subset = treatment == "elevated-dry", pch = 21, bg = "grey", cex=2)

#4: elevated-wet
par(mar=c(0,0,0,0))
plot(fluxC ~ Date, data = elevwet,axes = FALSE, ylab="", xlab="", ylim=c(0, 25000),lwd=LWD, type = 'l')
  points(boleC ~ Date,  data = elevwet,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = elevwet, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = elevwet, lwd=LWD,type = 'l',lty=5)
axis.Date(1, at = xAT, labels = TRUE)
box()
text(x=14035, y=25000, label = leglab2[4],cex=1.25, font=2)

points(treeC ~ Date , data = mab_final,subset = treatment == "elevated-wet", pch = 21, bg = "grey", cex=2)
axis.Date(1, at = xAT, labels = TRUE, outer=FALSE)
axis(2, labels=FALSE,tcl=.5)

# dev.copy2pdf(file= "master_scripts/paper_figs/treecarbon_daily2.pdf")
# dev.off()

