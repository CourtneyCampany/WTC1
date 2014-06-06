
setwd("C:/Users/90919620/Google Drive/HFE Database")

chambersumm <- read.csv("HFE Chamber treatments.csv")

#calcualte cumulative sum of soil respiraton from chambers
collars<-read.csv("HFE-I Soil Respiration Collars.csv")
collars$Date <- as.Date(as.character(collars$Date))
collars <- collars[,1:6]

# average two readings
collars2 <- aggregate(cbind(SoilCO2Efflux,Tsoil,VWC) ~ chamber+Date, data=collars, FUN=mean)

with(collars2, plot(Tsoil, SoilCO2Efflux))
with(collars2, plot(VWC, SoilCO2Efflux))

collars2 <- merge(collars2, chambersumm)

palette(c("blue","red"))
with(collars2, plot(Date, SoilCO2Efflux, pch=19, col=CO2_treatment))

# Simple first solution: average across all data
collarMeans <- aggregate(SoilCO2Efflux ~ chamber, data=collars2, FUN=mean)
collarMeans$SoilCO2Efflux_mumolm2s1 <-  10^6 * (1/12) * 10^-3 * collarMeans$SoilCO2Efflux / 3600
collarMeans$SoilCO2Efflux_gCm2d1 <- 10^-3 * collarMeans$SoilCO2Efflux * 24

write.csv(collarMeans, "HFE RSoil collar by chamber.csv", row.names=FALSE)


collars$SoilCO2Efflux<-collars$SoilCO2Efflux * 24 / 1000 *(12/44)
collars$Date <-as.character(collars$Date)
collars$Date <-as.Date(collars$Date)
daynumber<-diff(c(collars$Date))
daynumber2<-daynumber[daynumber > 0]
library(doBy)
mean_Rs<-summaryBy (SoilCO2Efflux ~ chamber + Date, data=collars, FUN=c(mean))
Dateorder<-order(mean_Rs$Date)
mean_Rs<-mean_Rs[Dateorder,]
nov10Rs<-(mean_Rs$SoilCO2Efflux.mean[1:18])*22
dec02Rs<-(mean_Rs$SoilCO2Efflux.mean[19:36])*20
dec22Rs<-(mean_Rs$SoilCO2Efflux.mean[37:54])*21
jan12Rs<-(mean_Rs$SoilCO2Efflux.mean[55:72])*28
feb9Rs<-(mean_Rs$SoilCO2Efflux.mean[73:90])
newresp<-cbind(c(nov10Rs, dec02Rs, dec22Rs, jan12Rs, feb9Rs))
actual_Rs<-cbind(mean_Rs, newresp)
chamberorder<-order(actual_Rs$Date)
actual_Rs<-actual_Rs[chamberorder,]
soilresp_sp<-(split(actual_Rs, actual_Rs$chamber))
cumRs<-lapply(soilresp_sp, function(x)cumsum(x$newresp))
              
totalRs<-do.call(rbind, cumRs)
totalRs<-as.data.frame(totalRs)    
str(totalRs)
names(totalRs)<-c("10Nov2008", "02Dec2008", "22Dec2008", "12Jan2009", "09Feb2009")

#need to export table