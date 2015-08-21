
#read data
source("HFE chamber read data.R")
leafpercC <- read.csv("calculated mass/leaf percent carbon.csv")
treemass <- read.csv("raw csv/HFE final DM totals.csv")
rootmass <- read.csv("calculated mass/root_mass.csv")


#HARVEST mass data, add all components, subset totalmass and divide by .5 for carbon
treemass1<-subset(treemass, chamber %in% c("ch01", "ch02","ch03","ch04","ch05","ch06","ch07","ch08","ch09","ch10","ch11","ch12"))
treemass1 <- droplevels(treemass1)

#add extra mass from leaves, branches, and cwd
moremass <- extra_mass[,c("chamber", "xleaf", "cwd")]
treemass1 <- merge(treemass1, moremass, by = "chamber")

#for roots use data from root_mass script which is fine roots scales from cores
#and coarse roots summed from harvest and cores
treemass1 <- merge(treemass1, rootmass[,1:3], by="chamber")


#sum mass and change to carbon, leaves are seperate
treemass1$woodmass <- with(treemass1, fineroot_mass + coarseroot_mass + wbr + ws + cwd)

treemass1$leafmass <- with(treemass1, wf+xleaf)
treemass1$leafC <- treemass1$leafmass * (leafpercC$leafpercC/100)

#total Tree carbon
treecarbon<-treemass1[, c("chamber", "woodmass", "leafC")]
treecarbon$woodC <- with(treecarbon, woodmass * .5)
treecarbon$treeC <- with(treecarbon, leafC+woodC)
#simplify for merging with flux
treecarbon <- treecarbon[, c("chamber", "treeC")]

###Chamber FLux
WTCflux<- flux[, c("Date","chamber","FluxCO2tot")]


# Total CO2 fluxes (diurnal)
WTCfluxagg <- aggregate(FluxCO2tot ~ chamber + Date, data=WTCflux, FUN=sum)
names(WTCfluxagg)[3]<-c("CO2flux")

# Change units to gC
WTCfluxagg$Cflux <- 12 * WTCfluxagg$CO2flux

#cumulative chamber flux sum
Cflux <- subset(WTCfluxagg, select= c("chamber", "Date","Cflux"))

#order by Date
Cflux <- Cflux[order(as.Date(Cflux$Date, format="%d/%m/%Y")),]
#order by chamber too
chamberorder<-order(Cflux$chamber, by=Cflux$Date)
Cflux <- Cflux[chamberorder,]
row.names(Cflux)<-NULL

#cumulative chamber C flux function
chamber_sp <- split(Cflux, Cflux$chamber)

Cflux_cum <- lapply(chamber_sp, function(x){
  x$Ccum <- cumsum(x$Cflux)
  return(x)
})

#cumulative chamber flux data frame
Cflux_total <-unsplit(Cflux_cum,Cflux$chamber)
Cflux_total <- subset(Cflux_total, select = c("Date", "chamber", "Ccum", "Cflux"))

#calculate total Cfluxes 
Cflux_agg<-aggregate (cbind(Cflux, Ccum) ~ chamber, data=Cflux_total, FUN=sum)

#merge mass with flux
CFlux_mass <- merge(Cflux_agg, treecarbon, by = "chamber")

#merge flux with treatments
CFlux_mass <- merge(CFlux_mass, chambersumm, by = "chamber")

write.csv(CFlux_mass, file = "harvest mass and carbon flux_newroots.csv", row.names=FALSE)    




