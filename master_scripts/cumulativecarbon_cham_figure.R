source("functions_and_packages/plot_objects.R")

treeC <- read.csv("whole_tree_csv/tree_C_flux.csv")
  treeC$Date <- as.Date(treeC$Date)
  treeC$bolebranch <- with(treeC, boleC+branchC)
  treeC$aboveC <- with(treeC, bolebranch + leafC + litterC)
  
#test both root data sets (with or without very fine roots)
root <- read.csv("calculated_mass/root_mass.csv")
  root$Date <- as.Date(root$Date)  
  
#root values need to be additive to aboveground C 
finalC <- subset(treeC, Date=="2009-03-16")
  finalC <- subset(finalC, select = c("chamber", "Date", "aboveC"))
  finalC <- merge(finalC, root)
  
  finalC$aboveplusfine <- with(finalC, aboveC+frootC_all)
  finalC$allC <- with(finalC, aboveplusfine +CrootC)

##plot bits
xAT <- seq.Date(from=as.Date("2008-4-1"), length=13, by="month")
LWD <- 2
 
#axis.Date(1, at = xAT, labels = T)

##plot 12 panel with treatment means---------------------------------------------------------------------------------
windows (8,11)

par(mfrow=c(4,3), las=1, mgp=c(3.5,1,0), oma=c(5,5,1,1),mar=c(0,0,0,0))

#1: chamber1
plot(CO2cum ~ Date, data = treeC[treeC$chamber == "ch01",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
  points(boleC ~ Date,  data = treeC[treeC$chamber == "ch01",],lwd=LWD,type = 'l',lty=3)
  points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch01",], lwd=LWD,type = 'l',lty=2)
  points(aboveC ~ Date , data = treeC[treeC$chamber == "ch01",], lwd=LWD,type = 'l',lty=5)
box()
legend(x=13975, y=26050,dayClab, lty = c(1, 3, 2, 5, -1, -1), pch = c(-1, -1, -1, -1, 21, 23),lwd=.8,  
        bty='n', pt.cex=.8, pt.bg=c(-1, -1, -1, -1,"grey", "grey"), cex=.8)
text(x=14040, y=27500, label = "Chamber 1", cex=1.25, font=2) 
##add roots on last date
points(aboveplusfine ~ Date , data = finalC[finalC$chamber == "ch01",], pch = 21, bg = "grey", cex=2)
points(allC ~ Date , data = finalC[finalC$chamber == "ch01",],pch = 23, bg="grey", cex=2)
axis(2, labels=TRUE, outer=TRUE)

#2: chamber2
plot(CO2cum ~ Date, data = treeC[treeC$chamber == "ch02",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch02",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch02",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch02",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 2", cex=1.25, font=2)

points(aboveplusfine ~ Date , data = finalC[finalC$chamber == "ch02",], pch = 21, bg = "grey", cex=2)
points(allC ~ Date , data = finalC[finalC$chamber == "ch02",],pch = 23, bg="grey", cex=2)

#3: chamber3
plot(CO2cum ~ Date, data = treeC[treeC$chamber == "ch03",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch03",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch03",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch03",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 3", cex=1.25, font=2) 

points(aboveplusfine ~ Date , data = finalC[finalC$chamber == "ch03",], pch = 21, bg = "grey", cex=2)
points(allC ~ Date , data = finalC[finalC$chamber == "ch03",],pch = 23, bg="grey", cex=2)

#4: chamber4
plot(CO2cum ~ Date, data = treeC[treeC$chamber == "ch04",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch04",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch04",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch04",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 4", cex=1.25, font=2) 

points(aboveplusfine ~ Date , data = finalC[finalC$chamber == "ch04",], pch = 21, bg = "grey", cex=2)
points(allC ~ Date , data = finalC[finalC$chamber == "ch04",],pch = 23, bg="grey", cex=2)
axis(2, labels=TRUE, outer=FALSE)

#5: chamber5
plot(CO2cum ~ Date, data = treeC[treeC$chamber == "ch05",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch05",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch05",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch05",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 5", cex=1.25, font=2) 

points(aboveplusfine ~ Date , data = finalC[finalC$chamber == "ch05",], pch = 21, bg = "grey", cex=2)
points(allC ~ Date , data = finalC[finalC$chamber == "ch05",],pch = 23, bg="grey", cex=2)

#6: chmaber6
plot(CO2cum ~ Date, data = treeC[treeC$chamber == "ch06",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch06",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch06",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch06",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 6", cex=1.25, font=2) 

points(aboveplusfine ~ Date , data = finalC[finalC$chamber == "ch06",], pch = 21, bg = "grey", cex=2)
points(allC ~ Date , data = finalC[finalC$chamber == "ch06",],pch = 23, bg="grey", cex=2)

#7: chmaber7
plot(CO2cum ~ Date, data = treeC[treeC$chamber == "ch07",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch07",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch07",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch07",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 7", cex=1.25, font=2) 

points(aboveplusfine ~ Date , data = finalC[finalC$chamber == "ch07",], pch = 21, bg = "grey", cex=2)
points(allC ~ Date , data = finalC[finalC$chamber == "ch07",],pch = 23, bg="grey", cex=2)
axis(2, labels=TRUE, outer=FALSE)

#8: chmaber8
plot(CO2cum ~ Date, data = treeC[treeC$chamber == "ch08",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch08",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch08",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch08",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 8", cex=1.25, font=2) 

points(aboveplusfine ~ Date , data = finalC[finalC$chamber == "ch08",], pch = 21, bg = "grey", cex=2)
points(allC ~ Date , data = finalC[finalC$chamber == "ch08",],pch = 23, bg="grey", cex=2)

#9: chmaber9
plot(CO2cum ~ Date, data = treeC[treeC$chamber == "ch09",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch09",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch09",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch09",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 9", cex=1.25, font=2) 

points(aboveplusfine ~ Date , data = finalC[finalC$chamber == "ch09",], pch = 21, bg = "grey", cex=2)
points(allC ~ Date , data = finalC[finalC$chamber == "ch09",],pch = 23, bg="grey", cex=2)

#10: chmaber10
plot(CO2cum ~ Date, data = treeC[treeC$chamber == "ch10",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch10",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch10",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch10",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 10", cex=1.25, font=2) 

points(aboveplusfine ~ Date , data = finalC[finalC$chamber == "ch10",], pch = 21, bg = "grey", cex=2)
points(allC ~ Date , data = finalC[finalC$chamber == "ch10",],pch = 23, bg="grey", cex=2)
axis.Date(1, at = xAT, labels = TRUE, outer=FALSE)
axis(2, labels=TRUE, outer=FALSE)

#11: chmaber11
plot(CO2cum ~ Date, data = treeC[treeC$chamber == "ch11",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch11",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch11",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch11",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 11", cex=1.25, font=2) 

points(aboveplusfine ~ Date , data = finalC[finalC$chamber == "ch11",], pch = 21, bg = "grey", cex=2)
points(allC ~ Date , data = finalC[finalC$chamber == "ch11",],pch = 23, bg="grey", cex=2)
axis.Date(1, at = xAT, labels = TRUE, outer=FALSE)


#12: chmaber12
plot(CO2cum ~ Date, data = treeC[treeC$chamber == "ch12",], axes=FALSE, ylab="", xlab="",ylim=c(0, 27500),lwd=LWD, type = 'l')
points(boleC ~ Date,  data = treeC[treeC$chamber == "ch12",],lwd=LWD,type = 'l',lty=3)
points(bolebranch ~ Date , data = treeC[treeC$chamber == "ch12",], lwd=LWD,type = 'l',lty=2)
points(aboveC ~ Date , data = treeC[treeC$chamber == "ch12",], lwd=LWD,type = 'l',lty=5)
box()
text(x=14040, y=27500, label = "Chamber 12", cex=1.25, font=2) 

points(aboveplusfine ~ Date , data = finalC[finalC$chamber == "ch12",], pch = 21, bg = "grey", cex=2)
points(allC ~ Date , data = finalC[finalC$chamber == "ch12",],pch = 23, bg="grey", cex=2)
axis.Date(1, at = xAT, labels = TRUE, outer=FALSE)


dev.copy2pdf(file= "master_scripts/paper_figs/treecarbon_daily_chambers.pdf")
dev.off()


