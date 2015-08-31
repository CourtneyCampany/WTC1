source("functions_and_packages/plot_objects.R")

treeC <- read.csv("calculated_mass/treeC_day.csv")
treeC$Date <- as.Date(treeC$Date)

##for ease of plotting seperate data by trt
ambdry <- treeC[treeC$treatment == "ambient-dry",]
  ambdry$Date <- as.Date(ambdry$Date)

ambwet <- treeC[treeC$treatment == "ambient-wet",]
  ambwet$Date <- as.Date(ambwet$Date)

elevdry <- treeC[treeC$treatment == "elevated-dry",]
  elevdry$Date <- as.Date(elevdry$Date)

elevwet <- treeC[treeC$treatment == "elevated-wet",]
  elevwet$Date <- as.Date(elevwet$Date)
  
  
##add roots and calculate final mass + fine and coarse roots
  roots <- read.csv("master_scripts/harvest_trt_means.csv")
  roots$Date <- as.Date("2009-03-16")
  roots$aboveplusfine <- with(roots, treeC+FrootC)
  roots$allC <- with(roots, aboveplusfine +CrootC)


##plot bits
xAT <- seq.Date(from=as.Date("2008-4-1"), length=13, by="month")
LWD <- 2
 
#axis.Date(1, at = xAT, labels = T)
#mtext("Carbon (g)", side = 2, outer=TRUE, line = -2.5, cex = 1.3)

##plot four panel with treatment means---------------------------------------------------------------------------------
#windows (7,10)

par(mfrow=c(2,2), las=1, mgp=c(3.5,1,0))

#1: ambient-wet
par(mar=c(0,5,2,0))
plot(CO2cum ~ Date, data = ambwet,xaxt='n', ylab="Carbon  (g)", ylim=c(0, 25000),lwd=LWD, type = 'l')
  points(boleC ~ Date,  data = ambwet,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = ambwet, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = ambwet, lwd=LWD,type = 'l',lty=5)
box()
legend(x=13975, y=23500,dayClab, lty = c(1, 3, 2, 5, -1, -1), pch = c(-1, -1, -1, -1, 21, 23),lwd=LWD,  
        bty='n', pt.cex=1.5, pt.bg=c(-1, -1, -1, -1,"grey", "grey"))
text(x=14060, y=25000, label = boxlab[1], cex=1.25, font=2) 
##add roots on last date
points(aboveplusfine ~ Date , data = roots,subset = treatment == "ambient-wet", pch = 21, bg = "grey", cex=2)
points(allC ~ Date , data = roots,subset = treatment == "ambient-wet",pch = 23, bg="grey", cex=2)


#2: ambient-dry
par(mar=c(0,0,2,2))
plot(CO2cum ~ Date, data = ambdry,axes = FALSE, ann = FALSE, ylim=c(0, 25000),lwd=LWD, type = 'l')
  points(boleC ~ Date,  data = ambdry,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = ambdry, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = ambdry, lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=25000, label = boxlab[2],cex=1.25, font=2)

points(aboveplusfine ~ Date , data = roots,subset = treatment == "ambient-dry", pch = 21, bg = "grey", cex=2)
points(allC ~ Date , data = roots,subset = treatment == "ambient-dry",pch = 23, bg="grey", cex=2)

#3: elevated-dry
par(mar=c(4,5,0,0))
plot(CO2cum ~ Date, data = elevdry, xaxt='n', xlab="", ylab="Carbon  (g)", ylim=c(0, 25000), lwd=LWD, type = 'l')
  points(boleC ~ Date,  data = elevdry,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = elevdry, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = elevdry, lwd=LWD,type = 'l',lty=5)
axis.Date(1, at = xAT, labels = T)
box()
text(x=14060, y=25000, label = boxlab[3],cex=1.25, font=2)

points(aboveplusfine ~ Date , data = roots,subset = treatment == "elevated-dry", pch = 21, bg = "grey", cex=2)
points(allC ~ Date , data = roots,subset = treatment == "elevated-dry",pch = 23, bg="grey", cex=2)

#4: elevated-wet
par(mar=c(4,0,0,2))
plot(CO2cum ~ Date, data = elevwet,axes = FALSE, ylab="", xlab="", ylim=c(0, 25000),lwd=LWD, type = 'l')
  points(boleC ~ Date,  data = elevwet,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = elevwet, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = elevwet, lwd=LWD,type = 'l',lty=5)
axis.Date(1, at = xAT, labels = TRUE)
box()
text(x=14040, y=25000, label = boxlab[4],cex=1.25, font=2)

points(aboveplusfine ~ Date , data = roots,subset = treatment == "elevated-wet", pch = 21, bg = "grey", cex=2)
points(allC ~ Date , data = roots,subset = treatment == "elevated-wet",pch = 23, bg="grey", cex=2)

# dev.copy2pdf(file= "master_scripts/paper_figs/treecarbon_daily.pdf")
# dev.off()


