setwd("C:/Users/90919620/Google Drive/HFE Database")
library(doBy)

#read chamber treatments
chambersumm <- read.csv("HFE chamber treatments.csv")

#Read in Branches, Twigs/Leaf, Stem, and Root mass
##dates are limited by Branch measurements
##generate monthly averages
Branch_mass <- read.csv("HFE branch mass allometry_cec.csv")
Branch_mass <- Branch_mass[,2:4]
DATS_BR <- as.POSIXlt(Branch_mass$Date)
Branch_mass$month <- as.numeric(format(DATS_BR ,"%m"))
Branch_mass$year <- as.numeric(format(DATS_BR ,"%Y"))
Branch_mass_mean <- aggregate(branch_mass ~ chamber + month + year, FUN = mean, data = Branch_mass)

Twig_Leaf_mass <- read.csv("HFE Leaf and Twig mass model_cec.csv")
Twig_Leaf_mass <- subset(Twig_Leaf_mass, select = c("Date", "chamber", "leafmass", "twig_mass"))
DATS_TW <- as.POSIXlt(Twig_Leaf_mass$Date)
Twig_Leaf_mass$month <- as.numeric(format(DATS_TW  ,"%m"))
Twig_Leaf_mass$year <- as.numeric(format(DATS_TW  ,"%Y"))
Twig_Leaf_mass_mean <- aggregate(cbind(twig_mass, leafmass) ~ chamber + month + year, FUN = mean, data = Twig_Leaf_mass)

Stem_mass <- read.csv("HFE bole mass_all dates_cec.csv")
Stem_mass <- Stem_mass[,2:4]
DATS_ST <- as.POSIXlt(Stem_mass$Date)
Stem_mass$month <- as.numeric(format(DATS_ST  ,"%m"))
Stem_mass$year <- as.numeric(format(DATS_ST  ,"%Y"))
Stem_mass_mean <- aggregate(bole_mass ~ chamber + month + year, FUN = mean, data = Stem_mass)

Root_mass <- read.csv("HFE root mass model_cec.csv")
Root_mass <- Root_mass[,2:5]
DATS_R <- as.POSIXlt(Root_mass$Date)
Root_mass$month <- as.numeric(format(DATS_R  ,"%m"))
Root_mass$year <- as.numeric(format(DATS_R  ,"%Y"))
Root_mass_mean <- aggregate(cbind(finerootbiomass, coarserootmass) ~ chamber + month + year, FUN = mean, data = Root_mass)

#Merge by month/year, reconfig dates
tree_mass <- merge(Branch_mass_mean, Twig_Leaf_mass_mean, by = c("year","month", "chamber"), all=TRUE)
tree_mass <- merge(tree_mass, Stem_mass_mean, by = c("year","month", "chamber"), all=TRUE)
tree_mass <- merge(tree_mass, Root_mass_mean, by = c("year","month", "chamber"), all=TRUE)
tree_mass <- tree_mass[complete.cases(tree_mass), ]
row.names(tree_mass)<-NULL
tree_mass$tree_biomass <- with(tree_mass, branch_mass + leafmass  + twig_mass  + bole_mass + finerootbiomass  +coarserootmass)
tree_mass <- merge(tree_mass, chambersumm, by = "chamber")

tree_mass$date <- ifelse(tree_mass$year == "2008" & tree_mass$month == "4", "2008-04-01", NA)
tree_mass$date <- ifelse(tree_mass$year == "2008" & tree_mass$month == "12", "2008-12-01", tree_mass$date)
tree_mass$date <- ifelse(tree_mass$year == "2008" & tree_mass$month == "11", "2008-11-01", tree_mass$date)
tree_mass$date <- ifelse(tree_mass$year == "2008" & tree_mass$month == "9", "2008-09-01", tree_mass$date)
tree_mass$date <- ifelse(tree_mass$year == "2008" & tree_mass$month == "10", "2008-10-01", tree_mass$date)
tree_mass$date <- ifelse(tree_mass$year == "2009" & tree_mass$month == "1", "2009-01-01", tree_mass$date)
tree_mass$date <- ifelse(tree_mass$year == "2009" & tree_mass$month == "3", "2009-03-01", tree_mass$date)
tree_mass$date <- as.Date(tree_mass$date)

###replace estimated data for last date with harvest data
treemass_noharv <- subset(tree_mass[, c("chamber", "tree_biomass", "date")], tree_mass$date != "2009-03-01")

treemass_harv<-read.csv("HFE final DM totals.csv")
treemass_harv<-subset(treemass_harv, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
treemass_harv$tree_biomass<- with(treemass_harv, wr + wf + wbr + ws)
treemass_harv <- treemass_harv[, c("chamber", "tree_biomass")]
treemass_harv$date <- as.Date(as.character("2009-03-16"))
row.names(treemass_harv)<-NULL

tree_allmass <- rbind(treemass_noharv, treemass_harv)
tree_allmass <- merge(tree_allmass, chambersumm)

#calculate Tree C, assume 50% for all components
tree_allmass$treeC <- tree_allmass$tree_biomass *.5

#treatment means plots
tree_stats <- summaryBy(tree_biomass ~ date + CO2_treatment + Water_treatment, data= tree_allmass, Fun = c(mean, sd))
tree_stats$treeC <- tree_stats$tree_biomass *.5
#treatment series for plotting
elevdry <- subset(tree_stats, CO2_treatment == "elevated" & Water_treatment == "dry")
elevwet <- subset(tree_stats, CO2_treatment == "elevated" & Water_treatment == "wet")
ambdry <- subset(tree_stats, CO2_treatment == "ambient" & Water_treatment == "dry")
ambwet <- subset(tree_stats, CO2_treatment == "ambient" & Water_treatment == "wet")
#graph
plot(treeC ~ date, data = elevdry, pch = 19, col = "darkolivegreen4", ylim = c(0,25000))
points(treeC ~ date, data = elevwet, pch = 21, bg = "darkgoldenrod3")
points(treeC ~ date, data = ambdry , pch = 21, bg = "red")
points(treeC ~ date, data = ambwet, pch = 21, bg = "blue")

##Calculate flux vs mass

#Read WTC flux data
chams <-  paste0("ch", sprintf("%02.0f",1:12))
fns <- paste0("HFE WTC hourly flux GapFilled ",chams,".csv")
allflux <- lapply(fns, read.csv)

# Soil respiration
Rsoil <- read.csv("HFE RSoil collar by chamber.csv")

flux<-do.call(rbind, allflux)
flux<-data.frame(flux)
WTCflux<- flux[, c("DateTime", "Date", "chamber", "FluxH2Otot", "FluxCO2tot", "sunrise", 
                   "sunset", "daylength", "DOY")]
WTCflux$DateTime<-as.character(WTCflux$DateTime)
WTCflux$DateTime<-as.POSIXct(WTCflux$DateTime, tz = "GMT")
WTCflux$Date <- as.Date(as.character(WTCflux$Date))
DATS <- as.POSIXlt(WTCflux$DateTime)
WTCflux$hour<-DATS$hour
# daytime fluxes
Dayflux<-subset(WTCflux, hour <= sunset & hour >= sunrise, 
                select= c("DateTime", "chamber", "FluxH2Otot", "FluxCO2tot"))

# Total CO2 and H2O fluxes (diurnal or whole day)
#Dayfluxagg <- summaryBy(FluxCO2tot + FluxH2Otot ~ chamber, data=Dayflux, FUN=sum)
WTCfluxagg <- summaryBy(FluxCO2tot + FluxH2Otot ~ chamber, data=WTCflux, FUN=sum)
names(WTCfluxagg)[2:3]<-c("CO2flux","H2Oflux")
# Change units to gC
WTCfluxagg$CO2flux <- 12 * WTCfluxagg$CO2flux
WTCfluxagg$H2Oflux <- 18 * 10^-3 * WTCfluxagg$H2Oflux
WTCfluxagg <- merge(WTCfluxagg, chambersumm)
#add WUE
WTCfluxagg$WUEflux <- WTCfluxagg$CO2flux / WTCfluxagg$H2Oflux

#add #of days for each date for mass
#total flux C over those days
ndays_apr <- as.Date("2008-04-15") - as.Date("2008-04-15")
ndays_sept <- as.Date("2008-09-17") - as.Date("2008-04-15")
ndays_oct <- as.Date("2008-10-28") - as.Date("2008-04-15")
ndays_nov <- as.Date("2008-11-15") - as.Date("2008-04-15")
ndays_dec <- as.Date("2008-12-11") - as.Date("2008-04-15")
ndays_jan <- as.Date("2009-01-15") - as.Date("2008-04-15")
ndays_mar <- as.Date("2009-03-16") - as.Date("2008-04-15")

Rsoil$SoilCO2_gCtot_apr <- 10*ndays_apr*Rsoil$SoilCO2Efflux_gCm2d1
Rsoil$SoilCO2_gCtot_sept <- 10*ndays_sept*Rsoil$SoilCO2Efflux_gCm2d1
Rsoil$SoilCO2_gCtot_oct <- 10*ndays_oct*Rsoil$SoilCO2Efflux_gCm2d1
Rsoil$SoilCO2_gCtot_nov <- 10*ndays_nov*Rsoil$SoilCO2Efflux_gCm2d1
Rsoil$SoilCO2_gCtot_dec <- 10*ndays_dec*Rsoil$SoilCO2Efflux_gCm2d1
Rsoil$SoilCO2_gCtot_jan <- 10*ndays_jan*Rsoil$SoilCO2Efflux_gCm2d1
Rsoil$SoilCO2_gCtot_mar <- 10*ndays_mar*Rsoil$SoilCO2Efflux_gCm2d1

#merge with mass
dayfluxtomass <- tree_allmass
dayfluxtomass$SoilCO2Efflux_gCtot <- ifelse(dayfluxtomass$date == "2008-04-01", Rsoil$SoilCO2_gCtot_apr, NA)
dayfluxtomass$SoilCO2Efflux_gCtot <- ifelse(dayfluxtomass$date == "2008-09-01", Rsoil$SoilCO2_gCtot_sept, dayfluxtomass$SoilCO2Efflux_gCtot)
dayfluxtomass$SoilCO2Efflux_gCtot <- ifelse(dayfluxtomass$date == "2008-10-01", Rsoil$SoilCO2_gCtot_oct, dayfluxtomass$SoilCO2Efflux_gCtot)
dayfluxtomass$SoilCO2Efflux_gCtot <- ifelse(dayfluxtomass$date == "2008-11-01", Rsoil$SoilCO2_gCtot_nov, dayfluxtomass$SoilCO2Efflux_gCtot)
dayfluxtomass$SoilCO2Efflux_gCtot <- ifelse(dayfluxtomass$date == "2008-12-01", Rsoil$SoilCO2_gCtot_dec, dayfluxtomass$SoilCO2Efflux_gCtot)
dayfluxtomass$SoilCO2Efflux_gCtot <- ifelse(dayfluxtomass$date == "2009-01-01", Rsoil$SoilCO2_gCtot_jan, dayfluxtomass$SoilCO2Efflux_gCtot)
dayfluxtomass$SoilCO2Efflux_gCtot <- ifelse(dayfluxtomass$date == "2009-03-16", Rsoil$SoilCO2_gCtot_mar, dayfluxtomass$SoilCO2Efflux_gCtot)
names(dayfluxtomass)[8] <- "CO2flux"

apr <- subset(dayfluxtomass, date == "2008-04-01")
sept <- subset(dayfluxtomass, date == "2008-09-01")
oct <- subset(dayfluxtomass, date == "2008-10-01")
nov <- subset(dayfluxtomass, date == "2008-11-01")
dec <- subset(dayfluxtomass, date == "2008-12-01")
jan <- subset(dayfluxtomass, date == "2009-01-01")
mar <- subset(dayfluxtomass, date == "2009-03-16")

palette(c( "black", "red"))
plot(CO2flux ~ treeC, data = sept, 
     pch=c(1,19)[Water_treatment], col=CO2_treatment, ylim = c(0,30000), xlim = c(0, 30000))
points(CO2flux ~ treeC, data = oct,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ treeC, data = nov,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ treeC, data = dec,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ treeC, data = jan,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ treeC, data = mar,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
abline(0,1)
##subtract april mass and replot
sept$C_yr <- sept$treeC - apr$treeC
oct$C_yr <- oct$treeC - apr$treeC
nov$C_yr <- nov$treeC - apr$treeC
dec$C_yr <- dec$treeC - apr$treeC
jan$C_yr <- jan$treeC - apr$treeC
mar$C_yr <- mar$treeC - apr$treeC

palette(c( "blue", "red"))
plot(CO2flux ~ C_yr, data = sept, 
     pch=c(1,19)[Water_treatment], col=CO2_treatment, ylim = c(0,30000), xlim = c(0, 10000))
points(CO2flux ~ C_yr, data = oct,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ C_yr, data = nov,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ C_yr, data = dec,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ C_yr, data = jan,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ C_yr, data = mar,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
abline(0,1)


###ratio of flux to mass
###plot by treatment effect across dates


###should we model litter
###need roots

###can substitute leaf count model here for 4 dates in 2008 (estimates are a little lower of leaf mass)
##april152008, nov122008, jan122002, feb172009
leafcount <- read.csv("HFE leaf area by count model_cec.csv")
###have not done, more dates avlaible with est leaf area

