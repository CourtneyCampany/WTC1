setwd("C:/Users/90919620/Google Drive/HFE Database")

# chamber treatments
chambersumm <- read.csv("HFE chamber treatments.csv")

#Read stem diameters and pathlengths
#remove early dates with diameter measurements below 65cm (estimate these seperately?)
stemD <- read.csv("HFE Tree Diameters all.csv")
stemD$Date <- as.character(as.Date(stemD$Date))
stemD<-subset(stemD, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
stemD<-subset(stemD, stemD$Date > "2008-01-16")

#Read tree top heights, convert to cm, set top height diameter to .001cm, assume top height is on main stem
#remove dates earlier than stemD as well as reasons above
stemH <- read.csv("HFE Tree Height Fixed.csv")
stemH<-subset(stemH, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
names(stemH)[3] <- "Pathlength"
stemH$Date <- as.character(as.Date(stemH$Date))
stemH$Pathlength <- stemH$Pathlength*100
stemH$Diameter <- as.numeric(ifelse(stemH$Pathlength >0, ".001", "NA"))
stemH$Stemsegmnt <- ifelse(stemH$Pathlength >0, "1", "NA")
stemH<-subset(stemH, stemH$Date > "2008-01-16")

# merge all diameters with top height data
stem<-rbind(stemD, stemH)
chamberorder<-order(stem$chamber, by=stem$Date)
stem<-stem[chamberorder,]

#calcualte stem length each diameter represents, remove base at 65cm
stem <- subset(stem, stem$Pathlength > 65)
stem$Lengthvalue <- ifelse(stem$Diameter == .001, 15, 30)

#read base diameters data,  merge and calculate volume
baseD <- read.csv("HFE BaseDiameters.csv")
baseD <- baseD[, 2:7]
stemV <- rbind(stem, baseD)
chamberorder<-order(stemV$chamber, by=stemV$Date)
stemV <- stemV[chamberorder,]
stemV$Volume <- ((((stemV$Diameter/2)^2)*(pi))*stemV$Lengthvalue)

###look back at first couple of dates...are they worth including???

#whole volume calculation by chamber with treatment
stem_volume <- aggregate(Volume ~ chamber + Date, data = stemV, FUN = sum)
stem_volume <- merge(stem_volume, chambersumm, by = "chamber")


#Final Date stem volume
stemfinal <- subset(stem_volume, Date == "2009-03-16")
write.csv(stemfinal, file = "HFE stem volume.csv")
library(sciplot)
with(stemfinal, bargraph.CI(Water_treatment, Volume, CO2_treatment, legend = TRUE))

volumetrtmodel <- lm(Volume ~ Water_treatment * CO2_treatment, data = stemfinal)
summary(volumetrtmodel)
plot(volumetrtmodel, which = c(3, 2))


#linear models, anovas, and plotting                
StemV_chamber <- split(stem_volume, stem_volume$chamber)

pdf("Tree Volume x Date.pdf", onefile=TRUE)
with(StemV_chamber, plot(Date, Volume, 
                         pch=c(1,19)[Water_treatment], col=CO2_treatment))
