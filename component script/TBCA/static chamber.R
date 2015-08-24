setwd("C:/Users/90919620/Google Drive/HFE Database")

#calcualte cumulative sum of soil respiraton from chambers
staticRs<-read.csv("HFE-I Soil Respiration Static Chambers.csv")
staticRs$SoilCO2Efflux<-as.numeric(staticRs$SoilCO2Efflux)
staticRs$SoilCO2Efflux<-staticRs$SoilCO2Efflux * 24 / 1000 *(12/44)
staticRs$Date <-as.character(staticRs$Date)
staticRs$Date <-as.Date(staticRs$Date)
daynumber<-diff(c(staticRs$Date))
daynumber2<-daynumber[daynumber > 0]

library(doBy)
mean_Rs<-summaryBy(SoilCO2Efflux ~ chamber + Date, data=staticRs, FUN=c(mean))
Dateorder<-order(mean_Rs$Date)
mean_Rs<-mean_Rs[Dateorder,]

apr3Rs<-(mean_Rs$SoilCO2Efflux.mean[1:18])*42
may15Rs<-(mean_Rs$SoilCO2Efflux.mean[19:36])*116
sep8Rs<-(mean_Rs$SoilCO2Efflux.mean[37:54])*30
oct8Rs<-(mean_Rs$SoilCO2Efflux.mean[55:72])
newresp<-cbind(c(apr3Rs, may15Rs, sep8Rs, oct8Rs))
actual_Rs<-cbind(mean_Rs, newresp)
chamberorder<-order(actual_Rs$Date)
actual_Rs<-actual_Rs[chamberorder,]
staticRs_sp<-(split(actual_Rs, actual_Rs$chamber))
cumRs<-lapply(staticRs_sp, function(x)cumsum(x$newresp))

totalRs<-do.call(rbind, cumRs)
totalRs<-as.data.frame(totalRs)    
str(totalRs)
names(totalRs)<-c("3april2008", "15may2008", "8sep2008", "8oct2008")


