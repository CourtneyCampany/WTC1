
source("HFE chamber read data.R")

# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
chambersumm <- droplevels(chambersumm[,1:3])

#chamberflux
chams <-  paste0("ch", sprintf("%02.0f",1:12))
fns <- paste0("raw csv/HFE WTC hourly flux GapFilled ",chams,".csv")
allflux <- lapply(fns, read.csv)

#tree chamber flux
flux<-do.call(rbind, allflux)
flux<-data.frame(flux)
flux$DateTime<-as.character(flux$DateTime)
flux$DateTime<-as.POSIXct(flux$DateTime, tz = "GMT")
flux$Date <- as.Date(flux$DateTime)


#TREE CHAMBER FLUX
WTCflux<- flux[, c("DateTime", "Date", "chamber", "FluxH2Otot", "FluxCO2tot", "sunrise", 
                   "sunset", "daylength")]

#Calcualte Diurnal C flux if needed
#DATS <- as.POSIXlt(WTCflux$DateTime)
#WTCflux$hour<-DATS$hour
# daytime fluxes
#Dayflux<-subset(WTCflux, hour <= sunset & hour >= sunrise, 
                #select= c("DateTime", "chamber", "FluxH2Otot", "FluxCO2tot"))
#Dayfluxagg <- aggregate(FluxCO2tot + FluxH2Otot ~ chamber, data=Dayflux, FUN=sum)


#calculate mean daily flux for CO2 and H20

flux_mean_calc <- function(dfr){

  WTCfluxagg <- aggregate(cbind(FluxCO2tot, FluxH2Otot) ~ chamber + Date, data=dfr, FUN=sum)
  names(WTCfluxagg)[3:4]<-c("CO2flux","H2Oflux")
  # Change units to gC
  WTCfluxagg$CO2flux <- 12 * WTCfluxagg$CO2flux
  # liters (kg) water
  WTCfluxagg$H2Oflux <- 18 * 10^-3 * WTCfluxagg$H2Oflux
  WTCfluxagg <- merge(WTCfluxagg, chambersumm)
  #add WUE
  WTCfluxagg$WUEflux <- WTCfluxagg$CO2flux / WTCfluxagg$H2Oflux
  return(WTCfluxagg)
}

WTCfluxagg <- flux_mean_calc(WTCflux)  
  
#calculate cumulative chamber flux

sumflux_calc <- function(dfr, plotsumm){
  
  chamberflux <- subset(dfr, select= c("chamber", "Date","CO2flux"))
  chamberflux <- merge(chamberflux, plotsumm)
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
}

Cflux_time <- sumflux_calc(WTCfluxagg, chambersumm)

write.csv(Cflux_time, file = "calculated mass/chamber C flux.csv", row.names=FALSE)

