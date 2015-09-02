source("HFE chamber read data.R")
source("./HFE R scripts/HFE chamber read data.R")

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



# Split by Date (chamber by date specific)
bole$chamberdate <- factor(paste(bole, bole$Date, sep=";"))
bole_sp <- split(bole, bole$chamberdate)

#apply length function
bole_sp2 <- lapply(bole_sp, function(x){
  
  
  sumdiam <- aggregate(x$Diameter ~ x$Pathlength, FUN=sum)
  return(sumdiam)
})


sumdiam <- do.call(rbind, sumdiam)










#split by tree, split by date, sum diameters for each pathlength
bole_sp <- split(stem_bole, stem_bole$chamber)
bole_sp <- lapply(bole_sp, function(cham){
  
  bole_sumdiams <- aggregate(cham$Diameter ~ cham$Pathlength + cham$Date, FUN=sum)
  
  return(cham)
})

bole_sumdiams <- do.call(rbind, bole_sp)









