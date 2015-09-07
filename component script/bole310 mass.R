#This script calculates bole mass from only surveys which recorded stem diameters over 310 cm 
#had to manually determine the dates where trees were < 310cm tall
#and/or when the tall tree surverys occured

#read raw data--------------------------------------------------------------------------------------------------------------

# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

#stem diameters through time
stem_diameters <- read.csv("raw csv/HFE Tree Diameters all.csv")
  stem_diameters$Date <- as.character(stem_diameters$Date)
  stem_diameters$Date <- as.Date(stem_diameters$Date)
  stem_diameters <- subset(stem_diameters, chamber %in% unique(chambersumm$chamber))
  stem_diameters <- droplevels(stem_diameters)

#stem height through time
stem_height <- read.csv("raw csv/HFE Tree Height Fixed.csv")
  stem_height$Date <- as.character(stem_height$Date)
  stem_height$Date <- as.Date(stem_height$Date)
  stem_height <- subset(stem_height, chamber %in% unique(chambersumm$chamber))
  stem_height <- droplevels(stem_height)

#stem density cookies from harvest
stem_density <- read.csv("raw csv/HFE wood density cookies.csv")
  stem_density <- subset(stem_density, chamber %in% unique(chambersumm$chamber))
  stem_density <- droplevels(stem_density)

#choose dates with surveys over 310cm in last year (other surveys did not get total tree height)
selectdates <- as.Date(c("2003-03-04", "2008-03-11", "2008-03-18", "2008-03-25", 
                        "2008-09-17", "2008-12-09", "2009-01-20", "2009-03-16"))


#STEM VOLUME----------------------------------------------------------------------------------------------------------------

#stem volume (above and below 65cm seperately) 
stemD310 <- subset(stem_diameters, Date %in% selectdates)

#Read tree heights, convert to cm, set top height diameter to .001cm
stemH1<-subset(stem_height, Date %in% selectdates)
  names(stemH1)[3] <- "Pathlength"
  stemH1$Pathlength <- stemH1$Pathlength*100
  stemH1$Diameter <- as.numeric(ifelse(stemH1$Pathlength >0, ".001", "NA"))
  stemH1$Stemsegmnt <- ifelse(stemH1$Pathlength >0, "1", "NA")

# merge stem diameters with top height data
stem <- rbind(stemD310, stemH1)
  chamberorder<-order(stem$chamber, by=stem$Date)
  stem <- stem[chamberorder,]

#calcualte length each cookie represents above 65cm only
stem_high <- subset(stem, stem$Pathlength > 65)
  stem_high$Lengthvalue <- ifelse(stem_high$Diameter == .001, 15, 30)

#calculate base stem metrics, add taper below 65cm
stem_low <- subset(stem_diameters, stem_diameters$Pathlength == 65 & Date %in% selectdates)

stemH2 <- subset(stem_height, Date %in% selectdates)
  stemH2$Height <- stemH2$Height * 100 #convert m to cm

#estimate Diameter (from cone equations) for 30cm and base
BaseDiameter <- merge(stem_low, stemH2)
  BaseDiameter$BaseD <- with(BaseDiameter, (((Diameter*Pathlength)+(Height*Diameter))/Height))
  BaseDiameter$MidPath <- 30
  BaseDiameter$MidD <- with(BaseDiameter, (((Diameter*MidPath)+(Height*Diameter))/Height))

#seperate, rename and bind diameters (<65cm) and pathlengths
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
stemV <- rbind(stem_high, baseD)
  chamberorder<-order(stemV$chamber, by=stemV$Date)
  stemV <- stemV[chamberorder,]
  ##volume of a cylinder pi*r^2*height
  stemV$Volume <- ((((stemV$Diameter/2)^2)*(pi))*stemV$Lengthvalue)

  
#STEM DENSITY-------------------------------------------------------------------------------------------------------------

#determine mass and densty per cm for each layer/stem segmnt
density <- subset(stem_density,select = c("chamber", "layerno", "Stemsegmnt",  "doverbark",  "dunderbark", "freshvolume",  "wbark",  "wwood"))

#density parameter calculations
density$height_cookie <- density$freshvolume / (pi*((density$doverbark/2)^2))
  density$woodV <- (((density$dunderbark/2)^2)*pi)*density$height_cookie
  density$wood_density <- density$wwood / density$woodV
  density$BarkV <- density$freshvolume - density$woodV
  density$bark_density <- density$wbark / density$BarkV
  density$barkdiam <- with(density, doverbark - dunderbark)

  
#calculate bark:wood as ratio of dry weight instead of diameter
density$Bark_Wood <- density$wbark / density$wwood
density$totaldens <- (density$wood_density + density$bark_density)/2

#chamber/treatment means
# Tree_density_mean <- aggregate(cbind(bark_density , wood_density) ~ chamber, data=density, FUN=mean)
# Tree_density_mean_trt <- merge(Tree_density_mean, chambersumm, by = "chamber")

#weighted mean of density bark, wood, and bark:wood
density_sp <- split(density, density$chamber)

# woodMean <- sapply(density_sp, function(x) weighted.mean(x$wood_density, w = x$wwood))
# woodD_wm <- data.frame(chamber=names(woodMean), wooddensity_wm=as.vector(woodMean) )
# 
# barkMean <- sapply(density_sp, function(x) weighted.mean(x$bark_density, w = x$wbark))
# barkD_wm <- data.frame(chamber=names(barkMean), barkdensity_wm=as.vector(barkMean) )

# BWratio <- sapply(density_sp, function(x) weighted.mean(x$Bark_Wood, w = x$freshvolume))
# BWratio <- data.frame(chamber=names(BWratio), barktowood_ratio=as.vector(BWratio) )

totMean <- sapply(density_sp, function(x) weighted.mean(x$totaldens, w = x$wwood+x$wbark))
totD_wm <- data.frame(chamber=names(totMean), density_wm=as.vector(totMean) )

#dataframe with weighted avergages (from layers) of bark and wood density for each chamber, and diameter ratios
# wooddensity_wm <- merge(barkD_wm, woodD_wm, by = "chamber")
# wooddensity_wm <- merge(wooddensity_wm, BWratio, by = "chamber")

#Merge stem density and volume dataframes, assume density does not change over time
#stem_mass <- merge(stemV, wooddensity_wm, by = "chamber")



#BOle MASS------------------------------------------------------------------------------------------------------------

##Merge stem density and volume dataframes
##calculate mass from density weightedmean and volume

stem_mass <- merge(stemV, totD_wm, by = "chamber")
stem_mass$bole_mass <- with(stem_mass, density_wm * Volume)

#stem mass calculation by chamber, add month and year metric
Bole310_Mass <- summaryBy(bole_mass ~ Date + chamber, data = stem_mass, FUN = sum, keep.names=TRUE)

# stem_mass$bark_mass <- (stem_mass$Volume * stem_mass$barktowood_ratio) * stem_mass$barkdensity_wm
# stem_mass$wood_mass <- (stem_mass$Volume * (1 - stem_mass$barktowood_ratio)) * stem_mass$wooddensity_wm
# stem_mass$bole_mass <- stem_mass$bark_mass+stem_mass$wood_mass


#Mass correction----------------------------------------------------------------------------------------------------------

lastdate <- subset(Bole310_Mass, Date == max(Date))

treemass <- read.csv("calculated_mass/harvest_mass_new.csv")
bole_harvest <- treemass[1:12, c("chamber",  "stem_mass_dry")]

#plot bole mass pred vs harvest
plot(lastdate$bole_mass, bole_harvest$stem_mass_dry)
abline(0,1)

###make correction factor by chamber
bole_corr <- merge(lastdate, bole_harvest)
bole_corr$bole_offset <- with(bole_corr, (bole_mass-stem_mass_dry)/bole_mass)


#LINEAR INTERPOLATION of data inbetween allometry surveys--------------------------------------------------------------------

#First I need dfr with all days for interpolation
#cham <- subset(chambersumm, inside_or_outside_WTC == "inside")

#empty list
datels <- list()

#date seq loop
datemaker <- for (i in unique(chambersumm$chamber)){
  datels[[i]] <- data.frame(Date = seq(from = as.Date("2008-3-14"), to = as.Date("2009-3-16"), by = "day"), 
                            chamber = i)} #bole_mass = "")}

# row-bind everything together:
dateseq <- do.call(rbind,datels)

#merge bole dataset with dateseq dfr
bole_alldays <- merge(dateseq, Bole310_Mass, by = c("Date", "chamber"), all=TRUE)


#use dateseq for interpolation
bole310_sp <- split(bole_alldays, bole_alldays$chamber)
bole310_sp <- lapply(bole310_sp, function(z){
  
  apfun_bo310 <- approxfun(x=z$Date, y=z$bole_mass)
  
  z$bole_pred <- apfun_bo310(z$Date)
  
  return(z)
})
bole310_pred <- do.call(rbind, bole310_sp)


##now multiply by offest----------------------------------------------------------------------------------------------------
bole310_pred <- merge(bole310_pred, bole_corr[, c(1,5)], by="chamber")

bole310_pred$bole_mass_adj <- with(bole310_pred, bole_pred * (1-bole_offset)) 
bole310_pred$bole_carbon <- bole310_pred$bole_mass_adj * .5


#write to calcualted mass subfolder
write.csv(Bole310_Mass, file = "calculated_mass/bole310 mass.csv", row.names=FALSE)
write.csv(bole310_pred, file = "calculated_mass/bole_mass_pred.csv", row.names=FALSE)

