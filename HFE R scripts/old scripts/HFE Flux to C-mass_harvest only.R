
setwd("C:/Users/90919620/Google Drive/HFE Database")

library(doBy)

# Read WTC flux data
chams <-  paste0("ch", sprintf("%02.0f",1:12))
fns <- paste0("HFE WTC hourly flux GapFilled ",chams,".csv")
allflux <- lapply(fns, read.csv)

# chamber treatments
chambersumm <- read.csv("HFE chamber treatments.csv")

# Read final harvest biomass 
treemass<-read.csv("HFE final DM totals.csv")
treemass<-subset(treemass, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])

# Soil respiration
Rsoil <- read.csv("HFE RSoil collar by chamber.csv")

flux<-do.call(rbind, allflux)
flux<-data.frame(flux)
WTCflux<- flux[, c("DateTime", "Date", "chamber", "FluxH2Otot", "FluxCO2tot", "sunrise", 
                   "sunset", "daylength", "DOY")]
WTCflux$DateTime<-as.character(WTCflux$DateTime)
WTCflux$DateTime<-as.POSIXct(WTCflux$DateTime, tz = "GMT")
any(is.na(WTCflux$DateTime))

WTCflux$Date <- as.Date(as.character(WTCflux$Date))

DATS <- as.POSIXlt(WTCflux$DateTime)
WTCflux$hour<-DATS$hour

# daytime fluxes
Dayflux<-subset(WTCflux, hour <= sunset & hour >= sunrise, 
                select= c("DateTime", "chamber", "FluxH2Otot", "FluxCO2tot"))

# Total CO2 and H2O fluxes
Dayfluxagg <- summaryBy(FluxCO2tot + FluxH2Otot ~ chamber, data=Dayflux, FUN=sum)
WTCfluxagg <- summaryBy(FluxCO2tot + FluxH2Otot ~ chamber, data=WTCflux, FUN=sum)


# Harvest data
treemass$allmass<- with(treemass, wr + wf + wbr + ws)
treemass <- treemass[, c("chamber", "allmass")]

# Two options : use daytime or 24hr day
#dayfluxtomass <- merge(Dayfluxagg, treemass, by="chamber")
dayfluxtomass <- merge(WTCfluxagg, treemass, by="chamber")

names(dayfluxtomass)[2:4]<-c("CO2flux","H2Oflux", "totalmass")

# Change units to gC
dayfluxtomass$CO2flux <- 12 * dayfluxtomass$CO2flux
dayfluxtomass$H2Oflux <- 18 * 10^-3 * dayfluxtomass$H2Oflux
dayfluxtomass$totalmass <- 0.5 * dayfluxtomass$totalmass # Assume 50% C.

dayfluxtomass <- merge(dayfluxtomass, chambersumm)

dayfluxtomass$WUEflux <- dayfluxtomass$CO2flux / dayfluxtomass$H2Oflux
dayfluxtomass$WUEmass <- dayfluxtomass$totalmass / dayfluxtomass$H2Oflux

# Add Soil respiration
ndays <- as.numeric(max(WTCflux$Date) - min(WTCflux$Date))
Rsoil$SoilCO2Efflux_gCtot <- 10*ndays*Rsoil$SoilCO2Efflux_gCm2d1

# Soil respiration very large term compared to aboveground flux.
dayfluxtomass <- merge(dayfluxtomass, Rsoil[,c("chamber","SoilCO2Efflux_gCtot")])


with(dayfluxtomass, plot(CO2flux, totalmass, 
                         xlim=c(0,30000), ylim=c(0,30000),
                         pch=c(1,19)[Water_treatment], col=CO2_treatment))

abline(0,1)
lmMassFlux <- lm(totalmass ~ CO2flux, data=dayfluxtomass)
abline(lmMassFlux, lty=5)


WUEmass_means <- with(dayfluxtomass, tapply(WUEmass, CO2_treatment, mean))
WUEmass_means[2] / WUEmass_means[1]


with(dayfluxtomass, plot(WUEmass, WUEflux, pch=c(1,19)[Water_treatment], col=CO2_treatment))

# CO2 effect
WUEmass_means <- with(dayfluxtomass, tapply(WUEmass, CO2_treatment, mean))
WUEmass_means[2] / WUEmass_means[1]

WUEflux_means <- with(dayfluxtomass, tapply(WUEflux, CO2_treatment, mean))
WUEflux_means[2] / WUEflux_means[1]



write.csv(dayfluxtomass, file = "HFE Ps_WUE vs Mass.csv")
str(dayfluxtomass)

model <- lm(CO2flux ~ H2Oflux, data = dayfluxtomass)
summary(model)
plot(CO2flux ~ H2Oflux, data = dayfluxtomass)
abline(model)
plot(model, which = c(3, 2))

modelco2<- lm(CO2flux ~ H2Oflux * CO2, data = dayfluxtomass)
anova(modelco2)

model <- lm(totalmass ~ WUE, data = dayfluxtomass)
summary(model)

WUEelev<-subset(dayfluxtomass$WUE, dayfluxtomass$CO2 == "elevated")
WUEamb<-subset(dayfluxtomass$WUE, dayfluxtomass$CO2 == "ambient")
mean(WUEelev) / mean(WUEamb)


