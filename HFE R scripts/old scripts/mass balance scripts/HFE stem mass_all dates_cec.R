setwd("C:/Users/90919620/Google Drive/HFE Database")

#This script calculates stem mass from stem volume(above and below 65cm seperately) and density parameters 

# chamber treatments
chambersumm <- read.csv("HFE chamber treatments.csv")

#Read stem diameters and pathlengths
#remove early dates with diameter measurements below 65cm (estimate these seperately?)
stemD <- read.csv("HFE Tree Diameters all.csv")
stemD$Date <- as.character(stemD$Date)
stemD$Date <- as.Date(stemD$Date)
stemD<-subset(stemD, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
stemD1<-subset(stemD, stemD$Date >= "2008-02-21")

#Read tree top heights, convert to cm, set top height diameter to .001cm, assume top height is on main stem
#remove dates earlier than stemD as well as reasons above
stemH <- read.csv("HFE Tree Height Fixed.csv")
stemH1<-subset(stemH, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
names(stemH1)[3] <- "Pathlength"
stemH1$Date <- as.character(stemH1$Date)
stemH1$Date <- as.Date(stemH1$Date)
stemH1$Pathlength <- stemH1$Pathlength*100
stemH1$Diameter <- as.numeric(ifelse(stemH1$Pathlength >0, ".001", "NA"))
stemH1$Stemsegmnt <- ifelse(stemH1$Pathlength >0, "1", "NA")
stemH1<-subset(stemH1, stemH1$Date >= "2008-02-21")

# merge all stem diameters with top height data
stem<-rbind(stemD1, stemH1)
chamberorder<-order(stem$chamber, by=stem$Date)
stem<-stem[chamberorder,]

#calcualte stem length each diameter represents, remove base at 65cm
stem <- subset(stem, stem$Pathlength > 65)
stem$Lengthvalue <- ifelse(stem$Diameter == .001, 15, 30)

#calculate base stem metrics
#2 news diameter and height dataframes
stemD2 <- subset(stemD, stemD$Pathlength == 65)

stemH2 <- subset(stemH, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
stemH2$Date <- as.character(as.Date(stemH2$Date))
stemH2$Height <- stemH2$Height * 100

#estimate Diameter (from cone equations) for 30cm and base
BaseDiameter <- merge(stemD2, stemH2)
BaseDiameter$BaseD <- with(BaseDiameter, (((Diameter*Pathlength)+(Height*Diameter))/Height))
BaseDiameter$MidPath <- 30
BaseDiameter$MidD <- with(BaseDiameter, (((Diameter*MidPath)+(Height*Diameter))/Height))

#seperate, rename and bind diameters (<65cm) and pathlengths, write to csv
midDiameter <- BaseDiameter [, c("Date", "chamber", "MidPath", "MidD")]
names(midDiameter)[3:4] <- c("Pathlength", "Diameter")
trunkDiameter <- BaseDiameter[, c("Date", "chamber", "Pathlength", "Diameter")]
stumpDiameter <- BaseDiameter[, c("Date", "chamber", "Pathlength", "BaseD")]
stumpDiameter$Pathlength <- 5
names(stumpDiameter)[4] <- "Diameter"

mainstem <- rbind(midDiameter, trunkDiameter)
mainstem <- rbind(mainstem, stumpDiameter)
chamberorder <- order(mainstem$chamber, by=mainstem$Date)
mainstem<-mainstem[chamberorder,]
mainstem$Lengthvalue <- ifelse(mainstem$Pathlength == 65, 30, mainstem$Pathlength)
mainstem$Stemsegmnt <- 1

# merge base and stem diameters and calculate volume
baseD <- subset(mainstem, select = c("Date", "chamber", "Pathlength", "Diameter", "Lengthvalue", "Stemsegmnt"))
stemV <- rbind(stem, baseD)
chamberorder<-order(stemV$chamber, by=stemV$Date)
stemV <- stemV[chamberorder,]
stemV$Volume <- ((((stemV$Diameter/2)^2)*(pi))*stemV$Lengthvalue)

##read in stem density, merge
stem_density <- read.csv("HFE bark and wood density_cec.csv")
stem_density <- stem_density[2:8]
stem_mass <- merge(stemV, stem_density, by = "chamber")

##calcualte mass
stem_mass$bark_mass <- (stem_mass$Volume * stem_mass$bark.wood.diameter) * stem_mass$barkdensity_wm
stem_mass$wood_mass <- (stem_mass$Volume * (1-stem_mass$bark.wood.diameter)) * stem_mass$wood_wm
stem_mass$bole_mass <- stem_mass$bark_mass+stem_mass$wood_mass

####still need to calucalte early date mass (assume very little taper???)


#whole stem mass calculation by chamber
Bole_Mass <- aggregate(bole_mass ~ Date + chamber, data = stem_mass, FUN = sum)
write.csv(Bole_Mass, file = "HFE Bole Mass_all dates_cec.csv")

