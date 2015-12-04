library(doBy)
####tbca vs PAR, AIRT, SOILT, soil M?


###load met data-----------------------------------------------------------------------------------------------------------

met2008 <- read.csv("raw csv/HFE 30min Metdata 2008.csv")
  met2008$Date <- as.Date(met2008$Date)

met2009 <- read.csv("raw csv/HFE 30min Metdata 2009.csv")
  met2009$Date <- as.Date(met2009$Date)

### load daily tbca data---------------------------------------------------------------------------------------------------
flux <- read.csv("master_scripts/Cflux_day.csv")
flux$tbca <- with(flux, fluxC-aboveC)
flux$Date <- as.Date(flux$Date)

fluxdates <- range(flux$Date)

tbcadates <- seq.Date(from = as.Date(fluxdates[1]), to = as.Date(fluxdates[2]), by="day")


#tree chamber flux----------------------------------------------------------------------------------------------------------
#chamberflux
chams <-  paste0("ch", sprintf("%02.0f",1:12))
fns <- paste0("raw csv/HFE WTC hourly flux GapFilled ",chams,".csv")
allflux <- lapply(fns, read.csv)


flux<-do.call(rbind, allflux)
flux<-data.frame(flux)
flux$DateTime<-as.character(flux$DateTime)
flux$DateTime<-as.POSIXct(flux$DateTime, tz = "GMT")
flux$Date <- as.Date(flux$DateTime)

#TREE CHAMBER FLUX (par=mumol m-2 s-1, tsoil @10cm, tair=C)
WTCflux<- flux[, c("DateTime", "Date", "chamber", "sunrise", 
                   "sunset", "daylength", "TairRef", "PAR", "Tsoil")]

###creates daily total PAR-------------------------------------------------------------------------------------

ppfd_fun <- function(x) {
  
  dat<- x[ , c("DateTime", "Date", "PAR","chamber")]
  
  dat$PAR <- ifelse(dat$PAR < 0, 0, dat$PAR)
  dat$ppfd_mol <- dat$PAR/1000000 #average Par every hour 
  dat$PPFDhour_mol_s <- dat$ppfd_mol*60*60 #scale to hour

  dat2 <- dat[,c("DateTime", "Date", "PPFDhour_mol_s", "chamber")]
  return(dat2)
}


WTC1_par <- ppfd_fun(WTCflux)
WTC1_daypar <- summaryBy(PPFDhour_mol_s ~ Date+chamber, data=WTC1_par, FUN=sum, keep.names = TRUE)

plot(PPFDhour_mol_s~Date, data=WTC1_daypar, col=as.factor(chamber), pch=16)


###creates min, max and mean air and soil T

WTCflux_clean <- WTCflux[WTCflux$Tsoil <= 38 & WTCflux$Tsoil >= 9,]


wtc_temps <- summaryBy(TairRef + Tsoil ~ Date+chamber, data=WTCflux, FUN=c(min, max, mean))
##split into air and soil for cleaning (some made numbers for soil)

wtc_air <- wtc_temps[, c(1:3,5,7)]

wtc_soil <- wtc_temps[, c(1:2,4,6,8)]
##clean wtcsoil
wtc_soil2 <- wtc_soil[wtc_soil$Tsoil.max <= 38 & wtc_soil$Tsoil.min >= 9,]

plot(TairRef.min~Date, data=wtc_air, col=as.factor(chamber), pch=16)
plot(Tsoil.max~Date, data=wtc_soil, col=as.factor(chamber), pch=16)


####plot these variables against tbca, see what happens




# ##lets clip the met data to fit within tbca measurement period
# met2008_flux <- met2008[met2008$Date %in% tbcadates,]
# met2008_flux2 <- met2008_flux[, c("DateTime", "Date", "TairRef", "PAR")]
# 
# met2009_flux <- met2009[met2009$Date %in% tbcadates,]
# met2009_flux2 <- met2009_flux[, c("DateTime", "Date", "TairRef", "PAR")]
# 
# ##what variables can I use PAR (umol m-2 s-1), TairMet (C)
# metWTC <- rbind(met2008_flux2, met2009_flux2)

