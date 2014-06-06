
library(doBy)


chams <-  paste0("ch", sprintf("%02.0f",1:12))
fns <- paste0("HFE WTC hourly flux GapFilled ",chams,".csv")
allflux <- lapply(fns, read.csv)

#This produces a list (allflux) with 12 dataframes. 

flux<-do.call(rbind, allflux)
flux<-data.frame(flux)
WTCflux<- flux[, c("DateTime", "chamber", "FluxCO2tot", "DOY")]
WTCflux$DateTime<-as.POSIXct(as.character(WTCflux$DateTime))
WTCflux$DateTime<-as.POSIXct(WTCflux$DateTime)
any(is.na(WTCflux$DateTime))
WTCflux$FluxCO2tot<-WTCflux$FluxCO2tot*(12/44)

treeflux<-summaryBy (FluxCO2tot ~ chamber + DOY, data=WTCflux, FUN=c(sum))
treeflux1<-summaryBy (FluxCO2tot ~ chamber, data=WTCflux, FUN=c(sum))
  

treemass<-read.csv("HFE final DM totals.csv")
treemass1<-subset(treemass, chamber %in% c("ch01", "ch02","ch03","ch04","ch05","ch06","ch07","ch08","ch09","ch10","ch011","ch12"))
treemass1$allmass<-treemass1$wr + treemass1$wf + treemass1$wbr +treemass1$ws
treemass2<-treemass1[, c("chamber", "allmass")]
treemass2$treecarbon <- with(treemass2, allmass * .5)


fluxtomass <- merge(treeflux1, treemass2, by="chamber")
names(fluxtomass)[2:3]<-c("CO2flux","totalmass") 
fluxtomass$CO2<-factor(ifelse(fluxtomass$chamber == "ch01" | fluxtomass$chamber == "ch03" | fluxtomass$chamber == "ch05" | fluxtomass$chamber == "ch07" | fluxtomass$chamber == "ch09" |fluxtomass$chamber == "ch11", "ambient", "elevated"))
fluxtomass$water<-factor(ifelse(fluxtomass$chamber == "ch01" | fluxtomass$chamber == "ch03" | fluxtomass$chamber == "ch04" | fluxtomass$chamber == "ch06" | fluxtomass$chamber == "ch08" | fluxtomass$chamber == "ch11", "wet", "dry"))
write.csv(fluxtomass, file = "HFE Flux vs Mass.csv")          



model <- lm(CO2flux ~ treecarbon, data = fluxtomass)
summary(model)

plot(CO2flux ~ totalmass, data = fluxtomass)
  abline(model)


plot(model, which = c(3, 2))
plot(CO2flux ~ totalmass, data = fluxtomass, log = "xy")
logmodel <- lm(log10(CO2flux) ~ log10(totalmass), data = fluxtomass)
summary(model)
plot(logmodel, which = c(3, 2))


modeltrt<- lm(CO2flux ~ totalmass*CO2*water, data = fluxtomass)
anova(modeltrt)
