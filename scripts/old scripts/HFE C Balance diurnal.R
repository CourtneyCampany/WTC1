library(doBy)

#read data
treemass<-read.csv("HFE final DM totals.csv")

# chamber treatments
chambersumm <- read.csv("HFE chamber treatments.csv")

#This produces a list (allflux) with flux data, one for each chmaber. 
chams <-  paste0("ch", sprintf("%02.0f",1:12))
fns <- paste0("HFE WTC hourly flux GapFilled ",chams,".csv")
allflux <- lapply(fns, read.csv)

#merge list into a dataframe,
flux<-do.call(rbind, allflux)
flux<-data.frame(flux)
WTCflux<- flux[, c("DateTime", "chamber", "FluxH2Otot", "FluxCO2tot", "PAR", "sunrise", "sunset", "DOY")]
WTCflux$FluxC<-WTCflux$FluxCO2tot*12

#convert Date,order by Date, determine day/night
WTCflux$DateTime<-as.character(WTCflux$DateTime)
WTCflux$DateTime<-as.POSIXct(WTCflux$DateTime, format = "%Y-%m-%d %H",tz = "GMT")
any(is.na(WTCflux$DateTime))


#add CO2 flux for day flux (24 hours)
alllux_agg<-summaryBy (FluxC ~ chamber + DOY, data=WTCflux, FUN=c(sum))
allflux1_agg<-summaryBy (FluxC ~ chamber, data=WTCflux, FUN=c(sum))


#harvest mass data, add all components, subset totalmass and divide by .5 for carbon
treemass1<-subset(treemass, chamber %in% c("ch01", "ch02","ch03","ch04","ch05","ch06","ch07","ch08","ch09","ch10","ch011","ch12"))
treemass1$allmass<-treemass1$wr + treemass1$wf + treemass1$wbr +treemass1$ws
treemass2<-treemass1[, c("chamber", "allmass")]
treemass2$treecarbon <- with(treemass2, allmass * .5)

#merge harvest mass with both day and diurnal total fluxes
dayfluxtomass <- merge(Dayflux1_agg, treemass2, by="chamber")
names(dayfluxtomass)[2:3]<-c("diurnalCO2flux","totalmass") 

dayfluxtomass <- merge(dayfluxtomass, allflux1_agg, by = "chamber")
names(dayfluxtomass)[5]<-c("dayCO2flux")

#add treatments
dayfluxtomass$CO2<-factor(ifelse(dayfluxtomass$chamber == "ch01" | dayfluxtomass$chamber == "ch03" | dayfluxtomass$chamber == "ch05" | dayfluxtomass$chamber == "ch07" | dayfluxtomass$chamber == "ch09" |dayfluxtomass$chamber == "ch11", "ambient", "elevated"))
dayfluxtomass$water<-factor(ifelse(dayfluxtomass$chamber == "ch01" | dayfluxtomass$chamber == "ch03" | dayfluxtomass$chamber == "ch04" | dayfluxtomass$chamber == "ch06" | dayfluxtomass$chamber == "ch08" | dayfluxtomass$chamber == "ch11", "wet", "dry"))
write.csv(dayfluxtomass, file = "HFE Day Flux vs Mass.csv")          


#linear model of day an diurnal flux versus harvest mass
daymodel <- lm(dayCO2flux ~ treecarbon, data = dayfluxtomass)
summary(daymodel)

diurnalmodel <- lm(diurnalCO2flux ~ treecarbon, data = dayfluxtomass)
summary(diurnalmodel)


plot(CO2flux ~ totalmass, data = fluxtomass)
abline(model)
plot(model, which = c(3, 2))