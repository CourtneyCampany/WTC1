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