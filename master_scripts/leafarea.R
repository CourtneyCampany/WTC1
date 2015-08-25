##need to determine final leaves mass for correlations with GPP and TBCA

leafarea <- read.csv("raw csv/HFE LA estimates alldates.csv")
leafarea$Date <- as.Date(leafarea$Date)

## treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

leafarea <- merge(leafarea, chambersumm)


order()
  
###plot bits
startday <- as.Date(strptime("2008-04-15", format = "%Y-%m-%d", tz="UTC"))

xlim1 <- as.Date(strptime("2008-04-15", format = "%Y-%m-%d", tz="UTC"))

xlim2 <- as.Date(strptime("2009-03-16", format = "%Y-%m-%d", tz="UTC"))

xAT <- seq.Date(startday, by="month", length=12, format = "%Y-%m-%d")

xlimdays <- c(xlim1, xlim2)

ltys <- c(1,5)
cols <- c("black", "red")

palette(c("black", "blue"))

windows(7,7)
plot(LAestlin ~ Date, data=leafarea, type='n',ylab="Leaf Area",  xaxt='n', xlab="", xlim=xlimdays, ylim=c(0, 75))  
  axis.Date(1, at=xAT, labels=TRUE) #axis needs no labels
  points(LAestlin ~ Date, data=leafarea, type='l', subset=chamber=="ch04", col="green")
  
  for (i in unique(leafarea$chamber)){
    points(LAestlin ~ Date, data=leafarea[leafarea$chamber == i,], col=CO2_treatment)
  }

