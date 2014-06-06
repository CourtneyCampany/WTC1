setwd("C:/Users/90919620/Google Drive/HFE Database")

#write in chamber summaries and treatments
chambersumm <- read.csv("HFE chamber treatments.csv")

#Write in tree diameters, only use the 65cm height
stemD <- read.csv("HFE Tree Diameters all.csv")
stemD$Date <- as.character(as.Date(stemD$Date))
stemD <- subset(stemD, stemD$Pathlength == 65)
stemD <- subset(stemD, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])

#write in total tree height
stemH <- read.csv("HFE Tree Height Fixed.csv")
stemH$Date <- as.character(as.Date(treeH$Date))
stemH$Height <- stemH$Height * 100
stemH<-subset(stemH, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
stemH<-subset(stemH, stemH$Date >= "2008-01-16")

#calculate missing DBH (from cone equations) for 30cm and base
BaseDiameter <- merge(stemD, stemH)
BaseDiameter$BaseD <- with(BaseDiameter, (((Diameter*Pathlength)+(Height*Diameter))/Height))
BaseDiameter$MidPath <- 30
BaseDiameter$MidD <- with(BaseDiameter, (((Diameter*MidPath)+(Height*Diameter))/Height))


#seperate, rename and bind diameters and pathlengths, write to csv
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

write.csv(mainstem, file = "HFE BaseDiameters.csv")
