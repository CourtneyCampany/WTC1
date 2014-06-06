setwd("C:/Users/90919620/Google Drive/HFE Database")

#all raw data here:
source("HFE chamber read data.R")

#calculate stem mass 
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

#plot all bole mass
plot(Bole_Mass$Date, Bole_Mass$bole_mass)

# 
# #Twig Mass model
# twig_est <- subset(branches, select = c("chamber",  "layerno","branchnumber", "diameter", "length", "Wbrlt1", "Wbrgt1", "br2order"))
# #twig_est <- aggregate(cbind(diameter, length, Wbrlt1, Wbrgt1)~ chamber + layerno, data = branches, FUN=mean)
# names(twig_est)[6] <- "twig"
# names(twig_est)[7] <- "branch"
# 
# #dry mass 
# DWbranches <- subset(DWbranches, select = c("chamber",  "layerno", "Wbrlt1",  "Wbrgt1", "DWbrlt1", "DWbrgt1"))
# DWbranches$percDWbranch <- with(DWbranches, DWbrgt1 / Wbrgt1)
# DWbranches$percDWtwig <-  with(DWbranches, DWbrlt1 /  Wbrlt1)
# 
# #merge sample branch allometry with dry mass
# twig_est <- merge(twig_est, subset(DWbranches, select = c("chamber",  "layerno", "percDWbranch", "percDWtwig"),
#                                    by = c("chamber" , "layerno")))
# twig_est  <- subset(twig_est , chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"]) 
# twig_est <- droplevels(twig_est)
# twig_est$layerno <- as.factor(twig_est$layerno)
# twig_est$twig <- as.numeric(twig_est$twig)
# #assume water content same within a layer
# 
# twig_est$twig_dry <- with(twig_est, twig * percDWtwig)
# twig_est$branch_dry <- with(twig_est, branch * percDWbranch)
# twig_est$branch_dry <- ifelse(twig_est$branch == 0, 0, twig_est$branch_dry)
# twig_est$branch_volume <- with(twig_est, (length *(pi*((diameter/2)^2)))*.75)
# twig_est <- merge(twig_est, chambersumm)
# 
# #twig branch number model, so many zeros for branches....model number of second order branches, then twigs from those
# #will not be able to estimate twig mass when intial branch/twig is less than 1cm
# 
# #second order branches
# 
# #br2order_model <- lm(br2order ~ length, data = twig_est)
# #summary(br2order_model)
# 
# #The 0 + suppresses the fitting of the intercept by lm.
# intercept <- 0.0
# #br2order_zeromodel <- lm(I(br2order - intercept) ~ 0 + length, data = twig_est)
# #summary(br2order_zeromodel)
# 
# br2order_trt<- lm(br2order ~ length * CO2_treatment * Water_treatment, data = twig_est)
# summary(br2order_trt)
# anova(br2order_trt)
# ###need 4 seperate equations , length at dry, wet, ambient, elev
# br2order_eq <- subset(twig_est, select = c("chamber" , "length",	"br2order"))
# br2order_eq <- merge(br2order_eq, chambersumm)
# 
# br2order_water <- split(br2order_eq, br2order_eq$Water_treatment)
# br2_length_water <-  lapply(br2order_water, function(wet) lm(I(br2order - intercept) ~0 +length, data = wet))
# 
# br2order_co2 <- split(br2order_eq, br2order_eq$CO2_treatment)
# br2_length_CO2 <-  lapply(br2order_co2, function(CO2) lm(I(br2order - intercept) ~0 +length, data = CO2))
# 
# br2order_water_pred <- as.data.frame(sapply(br2_length_water, coef))
# names(br2order_water_pred)[1] <- "br2order_water_coef"
# 
# br2order_CO2_pred <- as.data.frame(sapply(br2_length_CO2, coef))                                  
# names(br2order_CO2_pred)[1] <- "br2order_CO2_coef"
# ###START HERE..insert these equations !!!!
# 
# 
# #plot
# plot(twig_est$length, twig_est$br2order, ylim = c(0, 100), xlim = c(0, 500))
# abline(br2order_zeromodel)
# 
# #twig dry mass from # of second order branches
# twig_zeromodel <- lm(I(twig_dry - intercept) ~ 0 + br2order, data = twig_est)
# summary(twig_zeromodel)
# 
# #plot
# #obvious from plot that there are two data groups
# plot(twig_est$br2order, twig_est$twig_dry, ylim = c(0, 200), xlim = c(0, 100))
# abline(twig_zeromodel)
# 
# #plots of canopy layers do not solve issue
# canopy1 <- subset(twig_est , layerno==1) 
# canopy2 <- subset(twig_est , layerno==2) 
# canopy3 <- subset(twig_est , layerno==3) 
# canopy4 <- subset(twig_est , layerno==4) 
# canopy5 <- subset(twig_est , layerno==5) 
# 
# plot(canopy1$br2order, canopy1$twig_dry, ylim = c(0, 200), xlim = c(0, 100))
# points(canopy2$br2order, canopy2$twig_dry, col="blue" )
# points(canopy3$br2order, canopy3$twig_dry, col="red" )
# points(canopy4$br2order, canopy4$twig_dry, col="green" )
# points(canopy5$br2order, canopy5$twig_dry, col="yellow" )
# 
# #generate two equations, one for br2order above and below 20
# br2order_high <- subset(twig_est, br2order >= 20)
# br2order_high <- merge(br2order_high, chambersumm)
# plot(br2order_high$br2order, br2order_high$twig_dry, ylim = c(0, 200), xlim = c(0, 100))
# 
# br2order_low <- subset(twig_est, br2order < 20)
# br2order_low <- merge(br2order_low, chambersumm)
# plot(br2order_low$br2order, br2order_low$twig_dry, ylim = c(0, 200), xlim = c(0, 100))
# 
# # twig dry mass models
# br2order_low_model <- lm(I(twig_dry - intercept) ~ 0 + br2order, data = br2order_low)
# summary(br2order_low_model)
# 
# br2order_high_model <- lm(I(twig_dry - intercept) ~ 0 + br2order, data = br2order_high)
# summary(br2order_high_model)
# 
# #test for treatment effects
# 
# br2order_low_trt <- lm(twig_dry ~ br2order * CO2_treatment * Water_treatment, data = br2order_low)
# summary(br2order_low_trt)
# anova(br2order_low_trt)
# ###need two equations here, one for wet and dry
# 
# br2order_high_trt <- lm(twig_dry ~ br2order * CO2_treatment * Water_treatment, data = br2order_high)
# summary(br2order_high_trt)
# anova(br2order_high_trt)
# ###need two equations here, one for wet and dry
# 
# 
# #extract coefficients
# br2orderpred <- as.data.frame(coef(br2order_zeromodel))
# names(br2orderpred)[1] <- "br2order_coef"
# 
# #twigpredzero <- as.data.frame(coef(twig_zeromodel))
# #names(twigpredzero)[1] <- "twig_zero_coef"
# 
# twig_br2low <- as.data.frame(coef(br2order_low_model))
# names(twig_br2low)[1] <- "twig_br2low_coef"
# 
# twig_br2high <- as.data.frame(coef(br2order_high_model))
# names(twig_br2high)[1] <- "twig_br2high_coef"

#Calculate Branch Mass (>1cm) and Twig Mass (<1cm)
#caluclate a density metric to measure branch volmue through time with final harvest data

##write in and merge harvest branch mass with volume
branchM <- subset(harvest_mass, select =c("chamber",  "layerno", "Wbrgt1"))
names(branchM)[3] <- "br_mass"

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

#calculate chamber branch denisty from Mass/Volume (shape factor within volume equation)
#assumer no difference between bark and wood density of branches
branchD <- merge(branch_volume_tot, BRmass_tot, by = "chamber")
branchD$branch_density <- branchD$br_mass / branchD$Volume
branchD <- subset(branchD, select = c("chamber" , "branch_density"))

#calucalte branch mass through time
#as before assumer shape factor of .75 and add 5cm to length(assumer no taper)
branch_dates <- subset(branch_allometry, select = c("Date",  "chamber",  "stemnumber",	"branchnumber",	"diameter",	"length",	"branchBA"))
branch_dates <- merge(branch_dates, branchD, by = "chamber")
branch_dates$branch_mass <- ((branch_dates$branchBA*(branch_dates$length+5))*.75) * branch_dates$branch_density

#calculate twig mass through time(need branch mass from above)
br2order_slope <- br2orderpred$br2order_coef
branch_dates$br2order <- (br2order_slope * branch_dates$length) +0

branch_br2low <- subset(branch_dates, br2order < 20)
twigbr2low_slope <-  twig_br2low$twig_br2low_coef      
branch_br2low$twigbr2low_mass <- (twigbr2low_slope * branch_br2low$br2order) + 0

branch_br2high <- subset(branch_dates, br2order >= 20)
twigbr2high_slope <-  twig_br2high$twig_br2high_coef      
branch_br2high$twigbr2high_mass <- (twigbr2high_slope * branch_br2high$br2order) + 0

#add twig mass through time
twig_br2low <- subset(branch_br2low, select = c("Date", "chamber", "stemnumber", "branchnumber", "twigbr2low_mass"))
names(twig_br2low)[5] <- "twig_mass"
twig_br2high <- subset(branch_br2high, select = c("Date", "chamber", "stemnumber", "branchnumber", "twigbr2high_mass"))
names(twig_br2high)[5] <- "twig_mass"

twig_mass <- rbind(twig_br2low, twig_br2high)
chamberorder<-order(twig_mass$chamber, by=twig_mass$Date)
twig_mass <- twig_mass[chamberorder,]

branch_dates <- merge(branch_dates, twig_mass, by = c("Date", "chamber", "stemnumber",  "branchnumber"), all=TRUE)

#total branch and twig mass by chamber for each date
branch_mass_total <- aggregate(branch_mass ~ chamber + Date, data= branch_dates, FUN = sum)


#Leaf Mass
# a leaf mass model must first be constructed (leaf parameters from harvest)
leafM <- subset(harvest_mass, select =c("chamber",  "layerno", "LA", "Wleaf"))
names(leafM)[3:4] <- c("leafarea", "leafmass")


##tree totals(sum layers)
leafM <- aggregate(cbind(leafarea, leafmass) ~ chamber, FUN = sum, data = leafM)
leafM  <- subset(leafM , chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])  
leafM <-  merge(leafM, chambersumm)

leafMagg <- aggregate(cbind(leafarea,leafmass) ~ chamber, FUN=sum, data=leafM)
# SLA in m2kg-1
leafMagg$SLA <- with(leafMagg, leafarea / (10^-3 * leafmass))
sladfr <- leafMagg[,c("chamber","SLA")]

leafA_est <- merge(leafA_est, sladfr, by="chamber")
leafA_est$leafmass <- with(leafA_est, LAestlin / SLA)
 
# #leaf mass models (leafarea was best model, others omitted from this script)
# #no treatment interaction (stats/graphs script)
# 
# leafmass_model <- lm(leafmass ~ leafarea, data = leafM)
# summary(leafmass_model)
# 
# leafzero_model <- lm(I(leafmass - intercept) ~ 0 + leafarea, data = leafM)
# summary(leafzero_model)
# 
# plot(leafM$leafarea, leafM$leafmass, ylim = c(0, 10000), xlim = c(0, 100))
# abline(leafmass_model)
# abline(leafzero_model)

#extract coefficients
#leafmasspred <- as.data.frame(coef(leafmass_model))
#names(leafmasspred)[1] <- "leaf_coef"
# 
# leafzeropred <- as.data.frame(coef(leafzero_model))
# names(leafzeropred)[1] <- "leaf_coef"       
# 
# #read in leaf and height data
# leafA_est <- subset(leafA_est, select = c("chamber","Date",  "LAestlin", "LAlittercumlin"))
# names(leafA_est)[4] <- "leaflitter_mass"
# 
# #calculate leaf mass
# leaf_slope <- leafzeropred$leaf_coef
# leafA_est$leafmass <- (leaf_slope * leafA_est$LAestlin) + 0


##Calculate flux vs mass
#tree chamber flux
WTCflux<- flux[, c("DateTime", "Date", "chamber", "FluxH2Otot", "FluxCO2tot", "sunrise", 
                   "sunset", "daylength", "DOY")]

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

#soil respiration (subtract Rs from chamber flux??????)
#Read WTC soil respiration data
# Soil respiration (from collars+static average over one year, mean will cover winter and summer now)

#calcualte mean of soil respiraton from collars a/b
resp_collars <- collars[,1:6]
collars_mean <- aggregate(cbind(SoilCO2Efflux, Tsoil, VWC) ~ Date + chamber, data= collars, FUN = mean)

#calcualte mean of soil respiraton from staic chambers
resp_static <- static[,1:5]
static_mean <- aggregate(cbind(SoilCO2Efflux, Tsoil, VWC) ~ Date + chamber, data= static, FUN = mean)

#merge dataframes, now have more dates of soil respiration (big assumption that values are similar)
soilR <- rbind(collars_mean, static_mean)
chamberorder<-order(soilR$chamber, by=soilR$Date)
soilR <- soilR[chamberorder,]
names(soilR)[3] <- "SoilCO2"
soilR$soilC <- 10^-3 * soilR$SoilCO2 *24 *10
soilR <- subset(soilR, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
row.names(soilR)<-NULL
soilC_chamber <- subset(soilR, select = c("Date", "chamber", "soilC"))

#new dataframe 'treemass' with all components and chamber flux over all dates, used for linear interpolation
tree_mass <- merge(branch_mass_total, leafA_est, by = c("Date", "chamber"), all=TRUE)
tree_mass <- merge(tree_mass, Bole_Mass, by = c("Date", "chamber"), all=TRUE)
tree_mass <- merge(tree_mass, chamberflux_time, by = c("Date", "chamber"), all=TRUE)
tree_mass <- merge(tree_mass, soilC_chamber, by = c("Date", "chamber"), all=TRUE)

tree_mass_yr <- subset(tree_mass, tree_mass$Date >= "2008-04-15")
tree_mass_yr <- droplevels(tree_mass_yr)

#branch allom, leafarea = branch mass and leaf mass
br_nonna <- subset(tree_mass_yr, !is.na(branch_mass))
apfun_br <- approxfun(x=br_nonna$LAestlin, y=br_nonna$branch_mass)
tree_mass_yr$BR_pred <- apfun_br(tree_mass_yr$LAestlin)

#Bole mass
bo_nonna <- subset(tree_mass_yr, !is.na(bole_mass))
apfun_bo <- approxfun(x=bo_nonna$LAestlin, y=bo_nonna$bole_mass)
tree_mass_yr$BO_pred <- apfun_bo(tree_mass_yr$LAestlin)

#Twig mass
tw_nonna <- tree_mass_yr #subset(tree_mass_yr, !is.na(twig_mass))
apfun_tw <- approxfun(x=tw_nonna$LAestlin, y=tw_nonna$twig_mass)
tree_mass_yr$TW_pred <- apfun_tw(tree_mass_yr$LAestlin)

#Soil respiration in g of C
rs_nonna <- subset(tree_mass_yr, !is.na(soilC_chamber))
apfun_rs <- approxfun(x=rs_nonna$LAestlin, y=rs_nonna$soilC_chamber)
tree_mass_yr$RS_pred <- apfun_rs(tree_mass_yr$LAestlin)

#cumsum of soil respiration C
soilcarbonflux <- subset(tree_mass_yr, select = c("Date", "chamber", "RS_pred"))
soilcarbonflux_sp <- (split(soilcarbonflux, soilcarbonflux$chamber))
soilcarbonflux_cum <- lapply(soilcarbonflux_sp, function(x){x$RScum <- cumsum(x$RS_pred)
                                                 return(x)})
#cumulative Rs flux data frame
soilcarbonflux_yr <- unsplit(soilcarbonflux_cum, soilcarbonflux$chamber)
soilcarbonflux_yr <- subset(soilcarbonflux_yr, select = c("Date", "chamber", "RScum"))
#merge with treemass and subtract from chamber flux
tree_mass_yr <- merge(tree_mass_yr, soilcarbonflux_yr, by = c("Date", "chamber"), all=TRUE)
tree_mass_yr$flux_actual <- tree_mass_yr$CO2cum - tree_mass_yr$RScum

#calculate Tree C, assume 50% for all components
tree_mass_pred <- subset(tree_mass_yr, select = c("Date",  "chamber", "flux_actual", "BR_pred",  "BO_pred",	"leafmass",	"TW_pred",	"CO2cum", "CO2flux", "leaflitter_mass"))
tree_mass_pred$branchC <- (tree_mass_pred$BR_pred * .5)
tree_mass_pred$boleC <- (tree_mass_pred$BO_pred * .5)
tree_mass_pred$leafC <- (tree_mass_pred$leafmass * .5)
tree_mass_pred$twigC <- (tree_mass_pred$TW_pred * .5)
tree_mass_pred$litterC <- (tree_mass_pred$leaflitter_mass * .5)
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


#component graph
tree_stats <- aggregate(cbind(flux_actual, CO2cum, CO2flux, branchC, boleC, leafC, twigC, litterC) ~ Date, data= tree_mass_pred, FUN = mean)

plot(flux_actual ~ Date, data = tree_stats, 
                      pch = 19,
                      ylim = c(0,35000))

points(boleC ~ Date, data = tree_stats, pch=18, col="blue" )
points(branchC ~ Date, data = tree_stats, pch=17, col="orange" )
points(twigC ~ Date, data = tree_stats, pch=16, col="red" )
points(leafC ~ Date, data = tree_stats, pch=15, col="green" )
points(litterC ~ Date, data = tree_stats, pch=15, col="brown" )

palette(c( "blue", "red"))
pch=c(1,19)[Water_treatment], col=CO2_treatment,











