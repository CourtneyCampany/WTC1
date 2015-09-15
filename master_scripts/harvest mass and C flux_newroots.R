##Calculate carbon mass of each tissue compenent from final harvest

#read data------------------------------------------------------------------------------------------------------------------------

# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

treemass <- read.csv("calculated_mass/harvest_mass_new.csv")
#treemass <- read.csv("raw csv/HFE final DM totals.csv")

rootmass <- read.csv("calculated_mass/root_mass_simple.csv")

#extra biomas from CWD, damage, or removal
extra_mass <- read.csv("raw csv/HFE extra plant mass.csv")
  extra_mass$cwd <- with(extra_mass, damage_branch + branch_litter + bark_litter + harvest_branch_litter +
                           harvest_bark_litter + removed_branch)

# #leaf and litter carbon calucalted from LA estimates
# leaf_carbon<- read.csv("calculated_mass/leaf_carbon.csv")
#   leaf_carbon$Date <- as.Date(leaf_carbon$Date)
#   leaf_last <- leaf_carbon[leaf_carbon$Date == max(leaf_carbon$Date),]
# leaf_litter <- leaf_last[,c(1:2, 9,11,13,15)]

##leaf from harvest and litter has collection total 
leaves <- read.csv("calculated_mass/leafcarbon_data.csv")
litter <- read.csv("calculated_mass/litterfallcarbon.csv")

  
##tree chamber flux
source("component script/raw_flux_chamber.R")
  

#HARVEST mass data, add all components, subset totalmass and divide by .5 for carbon----------------------------------------------

#add extra mass from leaves, branches, and cwd
moremass <- extra_mass[,c("chamber", "cwd")]
treemass2 <- merge(treemass, moremass, by = "chamber")

  #for roots use data from root_mass script which is fine roots scaled from cores
  #and coarse roots summed from harvest and cores
  treemass3 <- merge(treemass2, rootmass[,1:2], by="chamber")

  ##leaf and litter Carbon
  treemass4 <- merge(treemass3, leaves)
  treemass4 <- merge(treemass4, litter)

    
  #sum mass and change to carbon, leaves are seperate
  treemass4$woodmass <- with(treemass4, root_mass + wbr + stem_mass_dry  + cwd)
  ##this includes above + belowground

#Calcualte Tree carbon-------------------------------------------------------------------------------------------------------------
  treemass4$woodC <- with(treemass4, woodmass * .5)
  treemass4$treeC <- with(treemass4, leafcarbon+woodC+littercarbon)
  
  treemass4$rootC <- with(treemass4, root_mass * .5)
  
  treemass4$branchC <- with(treemass4, (wbr+cwd) * .5)
  treemass4$boleC <- with(treemass4, stem_mass_dry * .5)
  
  treemass4$Cab <- with(treemass4, branchC+boleC+leafcarbon+littercarbon)

#simplify for merging with flux
treecarbon <- treemass4[, c("chamber", "rootC", "branchC", "boleC", "leafcarbon","littercarbon","Cab","treeC")]

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
Cflux_agg <- aggregate (cbind(Cflux, Ccum) ~ chamber, data=Cflux_total, FUN=sum)

#merge mass with flux
CFlux_mass <- merge(Cflux_agg, treecarbon, by = "chamber")

#merge flux with treatments
CFlux_mass <- merge(CFlux_mass, chambersumm, by = "chamber")


#write.csv(CFlux_mass, file = "harvest mass and carbon flux_newroots.csv", row.names=FALSE)  

###made some additions so save it as something else
write.csv(CFlux_mass[,c(1:2, 4:12)], file = "calculated_mass/chamber_carbon.csv", row.names=FALSE)   

