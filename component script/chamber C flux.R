
source("HFE chamber read data.R")

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

write.csv(chamberflux_time, file = "calculated mass/chamber C flux.csv", row.names=FALSE)

