source("functions_and_packages/plot_objects.R")

treeC <- read.csv("calculated_mass/treeC_day.csv")

##for ease of plotting seperate data by trt
ambdry <- treeC[treeC$treatment == "ambient-dry",]
  ambdry$Date <- as.Date(ambdry$Date)

ambwet <- treeC[treeC$treatment == "ambient-wet",]
  ambwet$Date <- as.Date(ambwet$Date)

elevdry <- treeC[treeC$treatment == "elevated-dry",]
  elevdry$Date <- as.Date(elevdry$Date)

elevwet <- treeC[treeC$treatment == "elevated-wet",]
  elevwet$Date <- as.Date(elevwet$Date)
  
##add roots
roots <- read.csv("master_scripts/harvest_trt_means.csv")


##plot bits
xAT <- seq.Date(from=as.Date("2008-4-1"), length=13, by="month")
LWD <- 2
CEX <- 2 

axis.Date(1, at = xAT, labels = T)
mtext("Carbon (g)", side = 2, outer=TRUE, line = -2.5, cex = 1.3)

##plot four panel with treatment means---------------------------------------------------------------------------------
windows (7,10)

par(mfrow=c(2,2), las=1, mgp=c(3.5,1,0))

#1: ambient-dry
par(mar=c(0,5,2,0))
plot(CO2cum ~ Date, data = ambdry,xaxt='n', ylab="Carbon  (g)", ylim=c(0, 20000),lwd=LWD, type = 'l')
  points(boleC ~ Date,  data = ambdry,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = ambdry, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = ambdry, lwd=LWD,type = 'l',lty=5)
box()
legend(x=13975, y=19000,dayClab, lty = c(1, 3, 2, 5, NA, NA), pch = c(NA, NA, NA, NA, 21, 21),lwd=LWD,  
        bty='n', pt.cex=1)
text(x=14050, y=20000, label = boxlab[1]) 

points(FrootC ~ Date , data = roots,subset = treatment == "ambient-dry", pch = 21)
points(CrootC ~ Date , data = roots,subset = treatment == "ambient-dry",pch = 21)



#2: ambient-wet
par(mar=c(0,0,2,2))
plot(CO2cum ~ Date, data = ambwet,axes = FALSE, ann = FALSE, ylim=c(0, 20000),lwd=LWD, type = 'l')
  points(boleC ~ Date,  data = ambwet,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = ambwet, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = ambwet, lwd=LWD,type = 'l',lty=5)
box()
text(x=14050, y=20000, label = boxlab[2])

#3: elevated-dry
par(mar=c(2,5,0,0))
plot(CO2cum ~ Date, data = elevdry, xaxt='n', ylab="Carbon  (g)", ylim=c(0, 20000), lwd=LWD, type = 'l')
  points(boleC ~ Date,  data = elevdry,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = elevdry, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = elevdry, lwd=LWD,type = 'l',lty=5)
axis.Date(1, at = xAT, labels = T)
box()
text(x=14050, y=20000, label = boxlab[3])

#4: elevated-wet
par(mar=c(2,0,0,2))
plot(CO2cum ~ Date, data = elevwet,axes = FALSE, ylab="", xlab="", ylim=c(0, 20000),lwd=LWD, type = 'l')
  points(boleC ~ Date,  data = elevwet,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = elevwet, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = elevwet, lwd=LWD,type = 'l',lty=5)
axis.Date(1, at = xAT, labels = TRUE)
box()
text(x=14050, y=20000, label = boxlab[4])







