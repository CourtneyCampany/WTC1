setwd("C:/Users/90919620/Google Drive/HFE Database")

#This script calculates stem volume (above and below 65cm seperately) and branch volume for final harvest

# chamber treatments
chambersumm <- read.csv("HFE chamber treatments.csv")

#Read stem diameters and pathlengths
stemD <- read.csv("HFE Tree Diameters all.csv")
stemD$Date <- as.character(as.Date(stemD$Date))
stemD<-subset(stemD, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
stemD<-subset(stemD, stemD$Date == "2009-03-16")

#Read tree top heights, convert to cm, set top height diameter to .001cm, assume top height is on main stem
stemH <- read.csv("HFE Tree Height Fixed.csv")
stemH1 <- subset(stemH, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
names(stemH1)[3] <- "Pathlength"
stemH1$Date <- as.character(as.Date(stemH1$Date))
stemH1 <- subset(stemH1, stemH1$Date == "2009-03-16")
stemH1$Pathlength <- stemH1$Pathlength*100
stemH1$Diameter <- as.numeric(ifelse(stemH1$Pathlength >0, ".001", "NA"))
stemH1$Stemsegmnt <- ifelse(stemH1$Pathlength >0, "1", "NA")

# merge all stem diameters with top height data
stem<-rbind(stemD, stemH1)
chamberorder<-order(stem$chamber, by=stem$Date)
stem<-stem[chamberorder,]

#calcualte stem length each diameter represents, remove base at 65cm
stem <- subset(stem, stem$Pathlength > 65)
stem$Lengthvalue <- ifelse(stem$Diameter == .001, 15, 30)

#calculate base stem metrics
#2 news diameter and height dataframes
stemD1 <- subset(stemD, stemD$Pathlength == 65)

stemH2 <- subset(stemH, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
stemH2$Date <- as.character(as.Date(stemH2$Date))
stemH2$Height <- stemH2$Height * 100
stemH2<-subset(stemH2, stemH2$Date == "2009-03-16")

#estimate Diameter (from cone equations) for 30cm and base
BaseDiameter <- merge(stemD1, stemH2)
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

write.csv(mainstem, file = "HFE BaseDiameters_2009-03-16.csv")

# merge base and stem diameters and calculate volume
baseD <- subset(mainstem, select = c("Date", "chamber", "Pathlength", "Diameter", "Lengthvalue", "Stemsegmnt"))
stemV <- rbind(stem, baseD)
chamberorder<-order(stemV$chamber, by=stemV$Date)
stemV <- stemV[chamberorder,]
stemV$Volume <- ((((stemV$Diameter/2)^2)*(pi))*stemV$Lengthvalue)
write.csv(stemV, file = "HFE stem volume_2009-03-16.csv")

#read branch harvest data
###assuming branches as cyclinders, adj with form factor of .75(cone= 2/3's of cyldiner(1))
branchD <- read.csv("HFE branch diameter length.csv")
branchD <- subset(branchD, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
branchD <- subset(branchD, select = c("Date", "chamber", "stemnumber", "branchnumber", "diameter", "length", "branchBA"))
branchD$Date <- as.character(as.Date(branchD$Date))
branchD<-subset(branchD, branchD$Date == "2009-03-16")
branchD$startlength <- 5
branchD$Volume <- branchD$branchBA*(branchD$length+5) # add 5cm to height, assuming no difference between base and insertion diam

write.csv(branchD, file = "HFE branch volume_2009-03-16.csv")

branch_volume <- aggregate(Volume ~ chamber, data = branchD, FUN = sum)
branch_volume <- merge(branch_volume, chambersumm, by = "chamber")

stem_volume <- aggregate(Volume ~ chamber + Date, data = stemV, FUN = sum)
stem_volume <- merge(stem_volume, chambersumm, by = "chamber")

#total woody volume(stem +branches), convert from cm3 to m3
branch <- branch_volume[, 1:2]
names(branch)[2] <- "Branch_Volume"
stem <- subset(stem_volume, select = c("chamber", "Volume", "CO2_treatment", "Water_treatment"))
names(stem)[2] <- "Stem_Volume"

treeV <- cbind(branch, stem[, -1] )
treeV <- subset(treeV, select = c("chamber", "Branch_Volume", "Stem_Volume", "CO2_treatment", "Water_treatment"))
treeV$woodyvolume <- treeV$Branch_Volume + treeV$Stem_Volume
treeV$woodyvolume <- treeV$woodyvolume * 10^-6
treeV$Branch_Volume <- treeV$Branch_Volume * 10^-6
treeV$Stem_Volume <- treeV$Stem_Volume * 10^-6
##assume a branch form factor of .75 (intermediate betwen cylcinder=1 and cone = 2/3), from Makela
treeV$Branch_Volume_adj <- treeV$Branch_Volume *(.75)
treeV$woodyvolume_adj <- treeV$Branch_Volume_adj + treeV$Stem_Volume

#whole stem volume calculation by chamber with treatment
with(treeV, plot(Stem_Volume, woodyvolume_adj,
                 pch=c(1,19)[Water_treatment], col=CO2_treatment))
                 
lmstemtotree_adj <- lm(woodyvolume_adj ~ Stem_Volume, data=treeV)
summary(lmstemtotree_adj)

abline(coef(lmstemtotree_adj))


