#This script calculates bole mass from only surveys which recorded 
#stem diameters over 310 cm 
#had to manually determine the dates where trees were < 310cm tall
#and/or when the tall tree surverys occured

#read raw data
source("HFE chamber read data.R")

#choose dates with surveys over 310cm in last year
selectdates <- as.Date(c("2003-03-04", "2008-03-11", "2008-03-18", "2008-03-25", 
                        "2008-09-17", "2008-12-09", "2009-01-20", "2009-03-16"))

#STEM MASS
#from stem volume(above and below 65cm seperately) and density parameters
stemD310 <- subset(stem_diameters, Date %in% selectdates)


#Read tree top heights, convert to cm, set top height diameter to .001cm
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
stemV$Volume <- ((((stemV$Diameter/2)^2)*(pi))*stemV$Lengthvalue)

#calculate stem density parameter
#determine mass and densty per cm for each layer/stem segmnt
density <- subset(stem_density,select = c("chamber", "layerno", "Stemsegmnt",  "doverbark",  "dunderbark", "freshvolume",  "wbark",  "wwood"))

#density parameter calculations
density$height_cookie <- density$freshvolume / (pi*((density$doverbark/2)^2))
density$woodV <- (((density$dunderbark/2)^2)*pi)*density$height_cookie
density$wood_density <- density$wwood / density$woodV
density$BarkV <- density$freshvolume - density$woodV
density$bark_density <- density$wbark / density$BarkV
density$barkdiam <- with(density, doverbark - dunderbark)

#calculate bark:wood diamter ratio
density$Bark_Wood <- with(density, barkdiam / dunderbark)

#chamber/treatment means
Tree_density_mean <- aggregate(cbind(bark_density , wood_density) ~ chamber, data=density, FUN=mean)
Tree_density_mean_trt <- merge(Tree_density_mean, chambersumm, by = "chamber")

#weighted mean of density bark, wood, and bark:wood
density_sp <- split(density, density$chamber)

woodMean <- sapply(density_sp, function(x) weighted.mean(x$wood_density, w = x$wwood))
woodD_wm <- data.frame(chamber=names(woodMean), wooddensity_wm=as.vector(woodMean) )

barkMean <- sapply(density_sp, function(x) weighted.mean(x$bark_density, w = x$wbark))
barkD_wm <- data.frame(chamber=names(barkMean), barkdensity_wm=as.vector(barkMean) )

BWratio <- sapply(density_sp, function(x) weighted.mean(x$Bark_Wood, w = x$freshvolume))
BWratio <- data.frame(chamber=names(BWratio), barktowood_ratio=as.vector(BWratio) )

#dataframe with weighted avergages (from layers) of bark and wood density for each chamber, and diameter ratios
wooddensity_wm <- merge(barkD_wm, woodD_wm, by = "chamber")
wooddensity_wm <- merge(wooddensity_wm, BWratio, by = "chamber")

#Merge stem density and volume dataframes, assume density does not change over time
stem_mass <- merge(stemV, wooddensity_wm, by = "chamber")

# Fit power function instead???
# windows()
#with(density, plot(doverbark, barkdiam, xlim=c(0,15), ylim=c(0,3)))
#abline(lm(barkdiam ~ doverbark, data=density))

#diam_pwr <- nls(barkdiam ~ I(doverbark^power), data = density, start = list(power = 1), + trace = T)

#calcualte mass
stem_mass$bark_mass <- (stem_mass$Volume * stem_mass$barktowood_ratio) * stem_mass$barkdensity_wm
stem_mass$wood_mass <- (stem_mass$Volume * (1 - stem_mass$barktowood_ratio)) * stem_mass$wooddensity_wm
stem_mass$bole_mass <- stem_mass$bark_mass+stem_mass$wood_mass

#stem mass calculation by chamber, add month and year metric
Bole310_Mass <- aggregate(bole_mass ~ Date + chamber, data = stem_mass, FUN = sum)


#plot bole mass
plot(Bole310_Mass$Date, Bole310_Mass$bole_mass)


#LINEAR INTERPOLATION of data inbetween allometry surveys

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

#plot interpolated values
plot(bole310_pred$Date, bole310_pred$bole_pred)

#write to calcualted mass subfolder
write.csv(Bole310_Mass, file = "calculated mass/bole310 mass.csv", row.names=FALSE)
write.csv(bole310_pred, file = "calculated mass/bole_mass_pred.csv", row.names=FALSE)

