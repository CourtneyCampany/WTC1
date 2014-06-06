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

#subset base, bole, crown for each tree
crown <- subset(stem, stem$Pathlength >= 310)
bole <- subset(stem, stem$Pathlength >65 & stem$Pathlength <310)
base <- subset(stem, stem$Pathlength == 65)

trunk <- subset(stem,stem$Pathlength > 65)
#function to add cross sectional areas for across pathlengths for boles
trunk$chamberdate <- factor(paste(trunk$chamber, trunk$Date, sep=";"))
trunk$CSA <- with(trunk, (pi/4)*Diameter^2)

trunksp <- split(trunk, trunk$chamberdate)

trunksp <- lapply(trunksp, function(x){
  dfr <- summaryBy(CSA ~ Pathlength, data=x, FUN=sum, keep.names=TRUE, id=~Date+chamber)
  return(dfr)
})

trunk2 <- do.call(rbind, trunksp)


trunk2$chamberdate <- factor(paste(trunk2$chamber, trunk2$Date, sep=";"))
trunk2sp <- split(trunk2, trunk2$chamberdate)

#trunk2sp <- lapply(trunk2sp, function(d){
 # length <- diff(d$Pathlength, lag=1, differences=1)
# return(length)
#})
#length
#str(length)
#length <- do.call(rbind,length)


#function to add cross sectional areas for across pathlengths for boles
bole$chamberdate <- factor(paste(bole$chamber, bole$Date, sep=";"))
bole$CSA <- with(bole, (pi/4)*Diameter^2)

bolesp <- split(bole, bole$chamberdate)

bolesp <- lapply(bolesp, function(x){
  bole2 <- summaryBy(CSA ~ Pathlength, data=x, FUN=sum, keep.names=TRUE, id=~Date+chamber)
  return(bole2)
})

bole2 <-do.call(rbind,bolesp)

