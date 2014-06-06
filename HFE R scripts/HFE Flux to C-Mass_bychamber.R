setwd("C:/Users/90919620/Google Drive/HFE Database")

setwd("g:/shared/HFE Database")

#all raw data here:
source("./HFE R scripts/HFE chamber read data.R")
#six1 <- subset(six,Date==as.Date("2009-1-20"))
#six2 <- subset(six,Date==as.Date("2009-2-03"))

#STEM MASS
#from stem volume(above and below 65cm seperately) and density parameters
stemD1 <- subset(stem_diameters, stem_diameters$Date >= "2008-02-21")

#Read tree top heights, convert to cm, set top height diameter to .001cm
stemH1<-subset(stem_height, stem_height$Date >= "2008-02-21")
names(stemH1)[3] <- "Pathlength"
stemH1$Pathlength <- stemH1$Pathlength*100
stemH1$Diameter <- as.numeric(ifelse(stemH1$Pathlength >0, ".001", "NA"))
stemH1$Stemsegmnt <- ifelse(stemH1$Pathlength >0, "1", "NA")

# merge stem diameters with top height data
stem <- rbind(stemD1, stemH1)
stem <- subset(stem, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
chamberorder<-order(stem$chamber, by=stem$Date)
stem <- stem[chamberorder,]

#calcualte length each cookie represents above 65cm only
stem_high <- subset(stem, stem$Pathlength > 65)
stem_high$Lengthvalue <- ifelse(stem_high$Diameter == .001, 15, 30)

#calculate base stem metrics, add taper below 65cm
stem_low <- subset(stem_diameters, stem_diameters$Pathlength == 65)
stemH2 <- subset(stem_height, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
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
density <- subset(stem_density,select = c("chamber", "layerno", "Stemsegmnt",  "doverbark",  "dunderbark", "freshvolume",  "wbark",	"wwood"))
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
Bole_Mass <- aggregate(bole_mass ~ Date + chamber, data = stem_mass, FUN = sum)


tmp <- aggregate(cbind(bark_mass,wood_mass,Volume) ~ Date + chamber, data = stem_mass, FUN = sum)
tmp <- subset(tmp, chamber=="ch06")


#plot bole mass
#plot(Bole_Mass$Date, Bole_Mass$bole_mass)

#BRANCH MASS
##write in and merge harvest branch mass with volume
branchM <- subset(harvest_mass, select =c("chamber",  "layerno", "Wbrgt1" ,"Wbrlt1"))
#branches are sum of mass less than and greater than 1cm diameter
branchM$br_mass <- with(branchM, Wbrgt1 + Wbrlt1)

#calcualte Branch density (Pb) form mass and volume equations
#assume branches as cyclinders, adj with form factor of .75(cone= 2/3's of cyldiner(1))
branch_diam <- subset(branch_allometry, branch_allometry$Date == "2009-03-16")
branch_diam <- subset(branch_diam, select = c("Date", "chamber", "stemnumber", "branchnumber", "diameter", "length", "branchBA"))
branch_diam <- branch_diam[complete.cases(branch_diam),]
branch_diam$startlength <- 5
branch_diam$Volume <- (branch_diam$branchBA*(branch_diam$length+5))*.75 # add 5cm to height, assuming no difference between base and insertion diam
row.names(branch_diam)<-NULL
#sum data for harvest branch mass(by layer) an volume (branch#)
BRmass_tot<- aggregate(br_mass ~ chamber, data = branchM, FUN = sum)
branch_volume_tot <- aggregate(Volume ~ chamber, data = branch_diam, FUN = sum)

#calculate chamber branch denisty from  total Mass/Volume (shape factor within volume equation)
#assumer no difference between bark and wood density of branches
branchD <- merge(branch_volume_tot, BRmass_tot, by = "chamber")
branchD$branch_density <- branchD$br_mass / branchD$Volume
branchD <- subset(branchD, select = c("chamber" , "branch_density"))

#calucalte branch mass through time
#as before assumer shape factor of .75 and add 5cm to length(assumer no taper)
branch_dates <- subset(branch_allometry, select = c("Date",  "chamber",  "stemnumber",	"branchnumber",	"diameter",	"length",	"branchBA"))
branch_dates <- merge(branch_dates, branchD, by = "chamber")
branch_dates$branch_mass <- ((branch_dates$branchBA*(branch_dates$length+5))*.75) * branch_dates$branch_density

#total branch mass by chamber for each date
branch_mass_total <- aggregate(branch_mass ~ chamber + Date, data = branch_dates, FUN = sum)
#plot(branch_mass_total$Date, branch_mass_total$branch_mass)

#LEAF MASS
# from harvest calculate SLA, assume SLA is constant through time to calculate mass from area
leafM <- subset(harvest_mass, select =c("chamber",  "layerno", "LA", "Wleaf"))
names(leafM)[3:4] <- c("leafarea", "leafmass")

##tree totals(sum layers)
leafM_agg <- aggregate(cbind(leafarea, leafmass) ~ chamber, FUN = sum, data = leafM)
leafM_agg  <- subset(leafM_agg , chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])  
leafM_agg <-  merge(leafM_agg, chambersumm)

# SLA in m2kg-1
leafM_agg$SLA <- with(leafM_agg, leafarea / (10^-3 * leafmass))
sladfr <- leafM_agg[,c("chamber","SLA")]

#calculate leaf mass through time
leafA_est <- merge(leafA_est, sladfr, by="chamber")
leafA_est$leafmass <- with(leafA_est, (LAestlin / SLA)*1000)
 

#TREE CHAMBER FLUX
WTCflux<- flux[, c("DateTime", "Date", "chamber", "FluxH2Otot", "FluxCO2tot", "sunrise", 
                   "sunset", "daylength")]

DATS <- as.POSIXlt(WTCflux$DateTime)
WTCflux$hour<-DATS$hour
# daytime fluxes
Dayflux<-subset(WTCflux, hour <= sunset & hour >= sunrise, 
                select= c("DateTime", "chamber", "FluxH2Otot", "FluxCO2tot"))

# Total CO2 and H2O fluxes (diurnal or whole day)
#Dayfluxagg <- aggregate(FluxCO2tot + FluxH2Otot ~ chamber, data=Dayflux, FUN=sum)
WTCfluxagg <- aggregate(cbind(FluxCO2tot, FluxH2Otot) ~ chamber + Date, data=WTCflux, FUN=sum)
names(WTCfluxagg)[3:4]<-c("CO2flux","H2Oflux")
# Change units to gC
WTCfluxagg$CO2flux <- 12 * WTCfluxagg$CO2flux
# liters (kg) water
WTCfluxagg$H2Oflux <- 18 * 10^-3 * WTCfluxagg$H2Oflux
WTCfluxagg <- merge(WTCfluxagg, chambersumm)
#add WUE
WTCfluxagg$WUEflux <- WTCfluxagg$CO2flux / WTCfluxagg$H2Oflux

#cumulative chamber flux sum
chamberflux <- subset(WTCfluxagg, select= c("chamber", "Date","CO2flux"))
chamberflux <- merge(chamberflux, chambersumm)
chamberflux <- chamberflux[order(as.Date(WTCfluxagg$Date, format="%d/%m/%Y")),]
chamberorder<-order(chamberflux$chamber, by=chamberflux$Date)
chamberflux <- chamberflux[chamberorder,]
row.names(chamberflux)<-NULL

chamberflux_sp<-split(chamberflux, chamberflux$chamber)
chamflux_cum<-lapply(chamberflux_sp, function(x){
  x$CO2cum <- cumsum(x$CO2flux)
  return(x)
  })

#cumulative chamber flux data frame
chamberflux_time <-unsplit(chamflux_cum,chamberflux$chamber)
chamberflux_time <- subset(chamberflux_time, select = c("Date", "chamber", "CO2cum", "CO2flux"))


#C MASS vs C FLUX
#new dataframe 'treemass' with all components and chamber flux over all dates, used for linear interpolation
tree_mass <- merge(branch_mass_total, leafA_est, by = c("Date", "chamber"), all=TRUE)
tree_mass <- merge(tree_mass, Bole_Mass, by = c("Date", "chamber"), all=TRUE)
tree_mass <- merge(tree_mass, chamberflux_time, by = c("Date", "chamber"), all=TRUE)

tree_mass_yr <- subset(tree_mass, tree_mass$Date >= "2008-04-15")
tree_mass_yr <- droplevels(tree_mass_yr)

#branch allom, leafarea = branch mass and leaf mass
tree_sp <- split(tree_mass_yr, tree_mass_yr$chamber)
tree_sp <- lapply(tree_sp, function(cham){
  
  apfun_br <- approxfun(x=cham$Date, y=cham$branch_mass)
  apfun_bo <- approxfun(x=cham$Date, y=cham$bole_mass)
  
  cham$BR_pred <- apfun_br(cham$Date)
  cham$BO_pred <- apfun_bo(cham$Date)
  
return(cham)
})
tree_mass_yr <- do.call(rbind, tree_sp)


#calculate Tree C, assume 50% for all components
tree_mass_pred <- subset(tree_mass_yr, select = c("Date",  "chamber", "BR_pred",  
                              "BO_pred",	"leafmass",	"CO2cum", "CO2flux", "LAlittercumlin"))
tree_mass_pred$branchC <- (tree_mass_pred$BR_pred * .5)
tree_mass_pred$boleC <- (tree_mass_pred$BO_pred * .5)
tree_mass_pred$leafC <- (tree_mass_pred$leafmass * .5)
tree_mass_pred$litterC <- (tree_mass_pred$LAlittercumlin * .5)
tree_mass_pred <- merge(tree_mass_pred, chambersumm)

tree_mass_pred <- tree_mass_pred[order(as.Date(tree_mass_pred$Date, format="%d/%m/%Y")),]
row.names(tree_mass_pred)<-NULL

#roots from harvest data
#Root Mass estimate for <2cm and >2cm diameter seperately
rootM$Frootmass <- with(rootM, veryfine + wrlt2)
rootM$Crootmass <- with(rootM, wr25  + wr510  + wrgt10)

#convert core diameter to chamber(which is roots/tree)
#mean of 5 cores and sum across depths (scale up ~10cm2 diameter core to 325cm chamber, *1055.25)
rootM_total <- aggregate(cbind(Frootmass, Crootmass) ~ chamber + upper, FUN = mean, data = rootM)
rootM_total <- aggregate(cbind(Frootmass, Crootmass) ~ chamber, FUN = sum, data = rootM_total)
#area scale factor
core <- (((10/2)^2)*pi)
hfe <- (((325/2)^2)*pi)
scaleup <- ((hfe-core)/core)

rootM_total$Frootmass <- rootM_total$Frootmass *scaleup
rootM_total$Crootmass <- rootM_total$Crootmass *scaleup
rootM_total$rootmass_all <- rootM_total$Crootmass + rootM_total$Frootmass
rootM_total  <- subset(rootM_total , chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])  
rootM_total <- droplevels(rootM_total)

write.csv(tree_mass_pred, file = "Tree Flux and Mass.csv")
write.csv(rootM_total, file = "HFE root harvest scaled.csv")

#component graph of means
#windows (6,6)

#tree_stats <- aggregate(cbind(CO2cum, CO2flux, branchC, boleC, leafC, litterC) ~ Date, data= tree_mass_pred, FUN = mean)

#plot(CO2cum ~ Date, data = tree_stats, 
 #                     pch = 19,
 #                     ylim = c(0,20000))

#points(boleC ~ Date, data = tree_stats, pch=18, col="blue" )
#points(branchC ~ Date, data = tree_stats, pch=17, col="orange" )
#points(leafC ~ Date, data = tree_stats, pch=15, col="green" )
#points(litterC ~ Date, data = tree_stats, pch=15, col="brown" )







