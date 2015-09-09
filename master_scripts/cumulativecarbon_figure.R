# source("functions_and_packages/plot_objects.R")

#treeC <- read.csv("calculated_mass/treeC_day.csv")
treeC <- read.csv("master_scripts/Cflux_day_trt.csv")
  treeC$Date <- as.Date(treeC$Date)
  treeC$bolebranch <- with(treeC, boleC+branchC)

treeC$treatment <- with(treeC, paste(CO2_treatment,Water_treatment, sep="-"))

###use Mab to predict roots since roots are not reset to zeo
rooteq <- read.csv("stats/rootshootmodel.csv")

mab_final <- treeC[treeC$Date == max(treeC$Date), c(1:3,9,11)]
  mab_final$rootmass_pred <- with(mab_final, rooteq[2,1]*(log10(aboveC*2)) + rooteq[1,1])
  mab_final$rootmass_pred2 <- 10^(mab_final$rootmass_pred)
  mab_final$rootC_pred <- mab_final$rootmass_pred2 * .5
  mab_final$treeC <- with(mab_final, rootC_pred+aboveC)

##add roots to double check predicted values
# roots <- read.csv("master_scripts/harvest_trt_means.csv")


##for ease of plotting seperate data by trt
ambdry <- treeC[treeC$treatment == "ambient-dry",]
  ambdry$Date <- as.Date(ambdry$Date)

ambwet <- treeC[treeC$treatment == "ambient-wet",]
  ambwet$Date <- as.Date(ambwet$Date)

elevdry <- treeC[treeC$treatment == "elevated-dry",]
  elevdry$Date <- as.Date(elevdry$Date)

elevwet <- treeC[treeC$treatment == "elevated-wet",]
  elevwet$Date <- as.Date(elevwet$Date)
  
  
##plot bits
xAT <- seq.Date(from=as.Date("2008-4-1"), length=13, by="month")
LWD <- 2

##plot four panel with treatment means---------------------------------------------------------------------------------
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
text(x=14025, y=25000, label = leglab2[1], cex=1.25, font=2) 
##add roots on last date
points(treeC ~ Date , data = mab_final,subset = treatment == "ambient-wet", pch = 21, bg = "grey", cex=2)

axis(2, labels=TRUE, outer=FALSE)
mtext("Carbon  (g)", side=2, line=4, outer=TRUE, las=0, at=.75)

#2: ambient-dry
par(mar=c(0,0,0,0))
plot(fluxC ~ Date, data = ambdry,axes = FALSE, ann = FALSE, ylim=c(0, 25000),lwd=LWD, type = 'l')
  points(boleC ~ Date,  data = ambdry,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = ambdry, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = ambdry, lwd=LWD,type = 'l',lty=5)
box()
text(x=14025, y=25000, label = leglab2[2],cex=1.25, font=2)

points(treeC ~ Date , data = mab_final,subset = treatment == "ambient-dry", pch = 21, bg = "grey", cex=2)

#3: elevated-dry
par(mar=c(0,0,0,0))
plot(fluxC ~ Date, data = elevdry, axes=FALSE, xlab="", ylab="Carbon  (g)", ylim=c(0, 25000), lwd=LWD, type = 'l')
  points(boleC ~ Date,  data = elevdry,lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = elevdry, lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = elevdry, lwd=LWD,type = 'l',lty=5)
box()
text(x=14025, y=25000, label = leglab2[3],cex=1.25, font=2)
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
text(x=14025, y=25000, label = leglab2[4],cex=1.25, font=2)

points(treeC ~ Date , data = mab_final,subset = treatment == "elevated-wet", pch = 21, bg = "grey", cex=2)
axis.Date(1, at = xAT, labels = TRUE, outer=FALSE)

# dev.copy2pdf(file= "master_scripts/paper_figs/treecarbon_daily.pdf")
# dev.off()

