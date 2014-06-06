
#source raw data here:
source("HFE chamber read data.R")
source("./HFE R scripts/HFE chamber read data.R")

library(doBy)


#STEM MASS
#from stem volume(above and below 65cm seperately) and density parameters
stemD1 <- subset(stem_diameters, stem_diameters$Date >= "2008-04-15")


#Read tree top heights, convert to cm, set top height diameter to .001cm
stemH1<-subset(stem_height, stem_height$Date >= "2008-04-15")
names(stemH1)[3] <- "Pathlength"
stemH1$Pathlength <- stemH1$Pathlength*100
stemH1$Diameter <- as.numeric(ifelse(stemH1$Pathlength >0, ".001", "NA"))
stemH1$Stemsegmnt <- ifelse(stemH1$Pathlength >0, "1", "NA")

# merge stem diameters with top height data
stem <- rbind(stemD1, stemH1)
stem <- subset(stem, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
chamberorder<-order(stem$chamber, by=stem$Date)
stem <- stem[chamberorder,]
stem <- droplevels(stem)

#stem_select <- subset(stem, Date %in% c("2008-04-15", "2008-12-09", "2009-1-20", "2009-3-16"))
date1<-subset(stem, Date == "2008-04-15")
date2<-subset(stem, Date == "2008-12-09")
date3<-subset(stem, Date == "2009-1-20")
date4<-subset(stem, Date == "2009-3-16")
stem_select <- rbind(date1, date2)
stem_select <- rbind(stem_select, date3)
stem_select <- rbind(stem_select, date4)

#subset base, bole, crown for each tree
crown <- subset(stem_select, stem_select$Pathlength >= 310)
bole <- subset(stem_select, stem_select$Pathlength >65 & stem_select$Pathlength <310)
base <- subset(stem_select, stem_select$Pathlength == 65)


#CROWN (generate length value  and CSA)
crown$chamberdate <- factor(paste(crown$chamber, crown$Date, sep=";"))
crown$CSA <- with(crown, (pi/4)*Diameter^2)

#split by chamberdate
crownsp <- split(crown, crown$chamberdate)

#function to generate cross sectional area at each cookie height (summed across split stem)
crownsp <- lapply(crownsp, function(x){
  dfr <- summaryBy(CSA ~ Pathlength, data=x, FUN=sum, keep.names=TRUE, id=~Date+chamber+chamberdate)
  
  return(dfr)
})
crown2 <- do.call(rbind, crownsp)

#split again
crown2sp <- split(crown2, crown2$chamberdate)

#function to generate length between each tree cookie (NA for first value at each chamberdate)
crown2sp <- lapply(crown2sp, function(x){
  x$length <- c(NA, diff(x$Pathlength)) 
  
  return(x)
})
crown3 <- do.call(rbind, crown2sp)

#replace NA with 30 (difference of min value - pathlength set at bole split off 
crown3$length <- with(crown3, ifelse(is.na(length), Pathlength - 280, length))



#BOLE (generate length value  and CSA)
bole$chamberdate <- factor(paste(bole$chamber, bole$Date, sep=";"))
bole$CSA <- with(bole, (pi/4)*Diameter^2)

#split by chamberdate
bolesp <- split(bole, bole$chamberdate)

#function to generate cross sectional area at each cookie height (summed across split stem)
bolesp <- lapply(bolesp, function(x){
  dfr <- summaryBy(CSA ~ Pathlength, data=x, FUN=sum, keep.names=TRUE, id=~Date+chamber)
  
  return(dfr)
})
bole2 <- do.call(rbind, bolesp)

#split again
bole2$chamberdate <- factor(paste(bole2$chamber, bole2$Date, sep=";"))
bole2sp <- split(bole2, bole2$chamberdate)

#function to generate length between each tree cookie (NA for first value at each chamberdate)
bole2sp <- lapply(bole2sp, function(x){
  x$length <- c(NA, diff(x$Pathlength)) 
  
  return(x)
})
bole3 <- do.call(rbind, bole2sp)

#replace NA with 30 (difference of 95 - the base diameter (65))
bole3$length <- with(bole3, ifelse(is.na(length), Pathlength - 65, length))


#merge bole and crown
trunk <- rbind(bole3, crown3)
trunkorder<-order(trunk$chamber, by=trunk$Date)
trunk <- trunk[trunkorder,]


#MAIN STEM-BASE (add taper below 65cm)
#dataframe with height of each tree at each date
stemH2 <- subset(stem_height, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
stemH2$Height <- stemH2$Height * 100 #convert m to cm
stemH2 <- subset(stemH2, stemH2$Date >= "2008-04-15")

#estimate Diameter (from cone equations) for 30cm and base
BaseDiameter <- merge(base, stemH2)
BaseDiameter$BaseD <- with(BaseDiameter, (((Diameter*Pathlength)+(Height*Diameter))/Height))
BaseDiameter$MidPath <- 30
BaseDiameter$MidD <- with(BaseDiameter, (((Diameter*MidPath)+(Height*Diameter))/Height))

#seperate, rename and bind diameters (<65cm) and pathlengths
mid_standard <- BaseDiameter [, c("Date", "chamber", "MidPath", "MidD")]
names(mid_standard)[3:4] <- c("Pathlength", "Diameter")
standard <- BaseDiameter[, c("Date", "chamber", "Pathlength", "Diameter")]
stump <- BaseDiameter[, c("Date", "chamber", "Pathlength", "BaseD")]
stump$Pathlength <- 5
names(stump)[4] <- "Diameter"

#new dataframe to use to bind stem base lengths 
basetrunk <- rbind(mid_standard, standard)
basetrunk <- rbind(basetrunk, stump)
#add cross sectional area
basetrunk$CSA <- with(basetrunk, (pi/4)*Diameter^2)

#order()
basetrunk$chamberdate <- factor(paste(basetrunk$chamber, basetrunk$Date, sep=";"))
baseorder <- order(basetrunk$chamberdate, by=basetrunk$Pathlength)
basetrunk<-basetrunk[baseorder,]

#split
basetrunksp <- split(basetrunk, basetrunk$chamberdate)

#function to generate length between each tree cookie (NA for first value at each chamberdate)
basetrunksp <- lapply(basetrunksp, function(x){
  x$length <- c(NA, diff(x$Pathlength)) 
  
  return(x)
})
basetrunk2 <- do.call(rbind, basetrunksp)

#replace NA with 5 (value from 5cm to ground level)
basetrunk2$length[is.na(basetrunk2$length)]<-5


# merge base and maeinstem diameters and calculate volume
#no longer a need for stem segment
basetrunk3 <- subset(basetrunk2, select = c("Date","chamber", "CSA", "chamberdate", "length", "Pathlength"))

trunk_all <- rbind(trunk, basetrunk3)

#order
trunkfinalorder <- order(trunk_all$chamberdate, by=trunk_all$Pathlength)
trunk_all<-trunk_all[trunkfinalorder,]

trunksp <- split(trunk_all, trunk_all$chamberdate)

trunksp <- lapply(trunksp, function(x) {
  
  for (i in 1:length(x$CSA)){
    x$volume[i] <- ((x$CSA[i]+x$CSA[i+1])*x$length[i])/2
  }
  
  return(x)
})

trunkvolume <- do.call(rbind,trunksp)

#caluclate total stem volume for each tree and data
# NA represents the top of the tree so we can remove to sum volume
trunkvolume2 <- summaryBy(volume ~ chamberdate, FUN=sum, data= trunkvolume, na.rm=TRUE, keep.names=TRUE, id=~Date+chamber)

#calculate STEM DENSITY parameter
#determine mass and densty per cm for each layer/stem segmnt
density <- subset(stem_density,select = c("chamber", "layerno", "Stemsegmnt",  "doverbark",  "dunderbark", "freshvolume",  "wbark",  "wwood"))
density <- subset(density, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
density <- droplevels(density)

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
stem_mass <- merge(trunkvolume2, wooddensity_wm, by = "chamber")


#calcualte mass
stem_mass$bark_mass <- (stem_mass$volume * stem_mass$barktowood_ratio) * stem_mass$barkdensity_wm
stem_mass$wood_mass <- (stem_mass$volume * (1 - stem_mass$barktowood_ratio)) * stem_mass$wooddensity_wm
stem_mass$bole_mass <- stem_mass$bark_mass+stem_mass$wood_mass

#stem mass calculation by chamber, add month and year metric
bole_mass <- summaryBy(bole_mass ~ chamberdate, data = stem_mass, FUN = sum, keep.names=TRUE, id=~Date+chamber)

plot(bole_mass ~ Date, data=subset(bole_mass, chamber=="ch05"))

