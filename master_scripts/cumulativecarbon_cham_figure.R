# source("functions_and_packages/plot_objects.R")

treeC <- read.csv("master_scripts/Cflux_day.csv")
  treeC$Date <- as.Date(treeC$Date)
  treeC$bolebranch <- with(treeC, boleC+branchC)

##use harvest roots to make sure pred roots are in the right ballpark (these should be slightly higher)    
# roots <- read.csv("calculated_mass/root_mass_simple.csv")
#   roots$Date <- as.Date("2009-03-16")
#   roots$root_carbon <- roots$root_mass*.5

rooteq <- read.csv("stats/rootshootmodel.csv")
  
#root values need to be predicted and added to aboveground C 
  finalC <- subset(treeC, Date=="2009-03-16")
  mab_final <- subset(finalC, select = c("chamber", "Date", "aboveC"))
  mab_final$rootmass_pred <- with(mab_final, rooteq[2,1]*(log10(aboveC*2)) + rooteq[1,1])
  mab_final$rootmass_pred2 <- 10^(mab_final$rootmass_pred)
  mab_final$rootC_pred <- mab_final$rootmass_pred2 * .5
  mab_final$treeC <- with(mab_final, rootC_pred+aboveC)

##plot bits
xAT <- seq.Date(from=as.Date("2008-4-1"), length=13, by="month")
LWD <- 2
 
#axis.Date(1, at = xAT, labels = T)

##plot 12 panel with treatment means---------------------------------------------------------------------------------
# windows (8,11)

par(mfrow=c(4,3), las=1, mgp=c(3.5,1,0), oma=c(4,6,1,1),mar=c(0,0,0,0))

#1: chamber1
plot(fluxC ~ Date, data = treeC[treeC$chamber == "ch01",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
  points(boleC ~ Date,  data = treeC[treeC$chamber == "ch01",],lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch01",], lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = treeC[treeC$chamber == "ch01",], lwd=LWD,type = 'l',lty=5)
box()
legend(x=13975, y=26050,dayClab, lty = c(1, 3, 2, 5, -1), pch = c(-1, -1, -1, -1, 21),lwd=.8,  
        bty='n', pt.cex=.8, pt.bg=c(-1, -1, -1, -1,"grey"), cex=.8)
text(x=14040, y=27500, label = "Chamber 1", cex=1.25, font=2) 
##add roots on last date
points(treeC ~ Date , data = mab_final[mab_final$chamber == "ch01",], pch = 21, bg = "grey", cex=2)

axis(2, labels=TRUE, outer=TRUE)

#2: chamber2
plot(fluxC ~ Date, data = treeC[treeC$chamber == "ch02",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch02",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch02",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch02",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 2", cex=1.25, font=2)

points(treeC ~ Date , data = mab_final[mab_final$chamber == "ch02",], pch = 21, bg = "grey", cex=2)

#3: chamber3
plot(fluxC ~ Date, data = treeC[treeC$chamber == "ch03",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch03",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch03",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch03",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 3", cex=1.25, font=2) 

points(treeC ~ Date , data = mab_final[mab_final$chamber == "ch03",], pch = 21, bg = "grey", cex=2)

#4: chamber4
plot(fluxC ~ Date, data = treeC[treeC$chamber == "ch04",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch04",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch04",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch04",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 4", cex=1.25, font=2) 
mtext("Carbon  (g)", side=2, line=4, outer=TRUE, las=0, at=.75)
mtext("Carbon  (g)", side=2, line=4, outer=TRUE, las=0, at=.25)

points(treeC ~ Date , data = mab_final[mab_final$chamber == "ch04",], pch = 21, bg = "grey", cex=2)
axis(2, labels=TRUE, outer=FALSE)

#5: chamber5
plot(fluxC ~ Date, data = treeC[treeC$chamber == "ch05",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch05",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch05",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch05",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 5", cex=1.25, font=2) 

points(treeC ~ Date , data = mab_final[mab_final$chamber == "ch05",], pch = 21, bg = "grey", cex=2)

#6: chamber6
plot(fluxC ~ Date, data = treeC[treeC$chamber == "ch06",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch06",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch06",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch06",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 6", cex=1.25, font=2) 

points(treeC ~ Date , data = mab_final[mab_final$chamber == "ch06",], pch = 21, bg = "grey", cex=2)

#7: chmaber7
plot(fluxC ~ Date, data = treeC[treeC$chamber == "ch07",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch07",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch07",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch07",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 7", cex=1.25, font=2) 

points(treeC ~ Date , data = mab_final[mab_final$chamber == "ch07",], pch = 21, bg = "grey", cex=2)
axis(2, labels=TRUE, outer=FALSE)

#8: chmaber8
plot(fluxC ~ Date, data = treeC[treeC$chamber == "ch08",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch08",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch08",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch08",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 8", cex=1.25, font=2) 

points(treeC ~ Date , data = mab_final[mab_final$chamber == "ch08",], pch = 21, bg = "grey", cex=2)

#9: chmaber9
plot(fluxC ~ Date, data = treeC[treeC$chamber == "ch09",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch09",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch09",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch09",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 9", cex=1.25, font=2) 

points(treeC ~ Date , data = mab_final[mab_final$chamber == "ch09",], pch = 21, bg = "grey", cex=2)

#10: chmaber10
plot(fluxC ~ Date, data = treeC[treeC$chamber == "ch10",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch10",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch10",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch10",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 10", cex=1.25, font=2) 

points(treeC ~ Date , data = mab_final[mab_final$chamber == "ch10",], pch = 21, bg = "grey", cex=2)
axis.Date(1, at = xAT, labels = TRUE, outer=FALSE)
axis(2, labels=TRUE, outer=FALSE)

#11: chmaber11
plot(fluxC ~ Date, data = treeC[treeC$chamber == "ch11",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch11",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch11",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch11",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 11", cex=1.25, font=2) 

points(treeC ~ Date , data = mab_final[mab_final$chamber == "ch11",], pch = 21, bg = "grey", cex=2)
axis.Date(1, at = xAT, labels = TRUE, outer=FALSE)


#12: chmaber12
plot(fluxC ~ Date, data = treeC[treeC$chamber == "ch12",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch12",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch12",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch12",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 12", cex=1.25, font=2) 

points(treeC ~ Date , data = mab_final[mab_final$chamber == "ch12",], pch = 21, bg = "grey", cex=2)
axis.Date(1, at = xAT, labels = TRUE, outer=FALSE)

# dev.copy2pdf(file= "master_scripts/paper_figs/treecarbon_daily_chambers.pdf")
# dev.off()


