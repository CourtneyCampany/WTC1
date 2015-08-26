source("functions_and_packages/plot_objects.R")

##need to determine final leaves mass for correlations with GPP and TBCA

leafarea <- read.csv("raw csv/HFE LA estimates alldates.csv")
leafarea$Date <- as.Date(leafarea$Date)

## treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

leafarea <- merge(leafarea, chambersumm)

##order by date
leafarea <- leafarea[order(leafarea$Date),]
  
###plot bits
startday <- as.Date(strptime("2008-04-15", format = "%Y-%m-%d", tz="UTC"))

xlim1 <- as.Date(strptime("2008-04-15", format = "%Y-%m-%d", tz="UTC"))

xlim2 <- as.Date(strptime("2009-03-16", format = "%Y-%m-%d", tz="UTC"))

xAT <- seq.Date(startday, by="month", length=12, format = "%Y-%m-%d")

xlimdays <- c(xlim1, xlim2)

ltys <- c(1,5)
cols <- c("black", "red")

palette(c("black", "blue"))

###plot leaf area----------------------------------------------------------------------------------------------------------

windows(7,7)

par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))

plot(LAestlin ~ Date, data=leafarea, type='n',ylab=leaflab,  xaxt='n', xlab="", xlim=xlimdays, ylim=c(0, 70))  
  axis.Date(1, at=xAT, labels=TRUE) #axis needs no labels
  #points(LAestlin ~ Date, data=leafarea, type='l', subset=chamber=="ch04", col="green")
  
  for (i in unique(leafarea$chamber)){
    points(LAestlin ~ Date, data=leafarea[leafarea$chamber == i,], col=CO2_treatment, type='l', lwd=2, lty=c(1,2)[Water_treatment])
  }
  
legend("topleft", trtlab, pch=c(15, 15, -1, -1), lty=c(-1, -1, 1,2), col=c("black", "blue", "black", "black"), bty='n', inset=0.01)
  
dev.copy2pdf(file="master_scripts/paper_figs/leafarea.pdf")
dev.off() 
  

