# Put all raw data in here

# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
chambersumm <- droplevels(chambersumm[,1:3])

#stem diameters through time
stem_diameters <- read.csv("raw csv/HFE Tree Diameters all.csv")
stem_diameters$Date <- as.character(stem_diameters$Date)
stem_diameters$Date <- as.Date(stem_diameters$Date)
stem_diameters <- subset(stem_diameters, chamber %in% unique(chambersumm$chamber))
stem_diameters <- droplevels(stem_diameters)

#stem height through time
stem_height <- read.csv("raw csv/HFE Tree Height Fixed.csv")
stem_height$Date <- as.character(stem_height$Date)
stem_height$Date <- as.Date(stem_height$Date)
stem_height <- subset(stem_height, chamber %in% unique(chambersumm$chamber))
stem_height <- droplevels(stem_height)

#stem density cookies from harvest
stem_density <- read.csv("raw csv/HFE wood density cookies.csv")
stem_density <- subset(stem_density, chamber %in% unique(chambersumm$chamber))
stem_density <- droplevels(stem_density)

#final harvest component biomasses
harvest_mass <- read.csv("raw csv/HFE final harvest biomass by layer.csv")
harvest_mass <- subset(harvest_mass, chamber %in% unique(chambersumm$chamber))
harvest_mass <- droplevels(harvest_mass)

#branch allometry through time
branch_allometry <- read.csv("raw csv/HFE branch diameter length.csv")
branch_allometry$Date <- as.character(branch_allometry$Date)
branch_allometry$Date <- as.Date(branch_allometry$Date)

#Sample branches from harvest, with dry mass as seperate dataframe for twig mass
branches <- read.csv("raw csv/HFE final harvest Sample Branches.csv", na.strings="???")
branches <- subset(branches, chamber %in% unique(chambersumm$chamber))
branches <- droplevels(branches)

DWbranches <- read.csv("raw csv/HFE final harvest Sample Branches dry weight.csv")
DWbranches <- subset(DWbranches, chamber %in% unique(chambersumm$chamber))
DWbranches <- droplevels(DWbranches)

#leaf area estimates
leafA_est <- read.csv("raw csv/HFE LA estimates alldates.csv")
leafA_est$Date <- as.character(leafA_est$Date)
leafA_est$Date <- as.Date(leafA_est$Date)

#extra biomas from CWD, damage, or removal
extra_mass <- read.csv("raw csv/HFE extra plant mass.csv")
extra_mass$xleaf <- with(extra_mass,damage_leaf+ removed_leaf)
extra_mass$cwd <- with(extra_mass, damage_branch + branch_litter + bark_litter + harvest_branch_litter +
                          harvest_bark_litter + removed_branch)


#root mass from harvest
rootM <- read.csv("raw csv/HFE Fine root cores final harvest.csv")
rootM <- subset(rootM, chamber %in% unique(chambersumm$chamber))
rootM <- droplevels(rootM)

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

#soil respiration (two different measurements of Rs added to add number of dates (do not overlap))
#collars<-read.csv("HFE-I Soil Respiration Collars.csv")
#collars$Date <- as.character(collars$Date)
#collars$Date <- as.Date(collars$Date)

#static<-read.csv("HFE-I Soil Respiration Static Chambers.csv")
#static$Date <- as.character(static$Date)
#static$Date <- as.Date(static$Date)

#litter
#not used...used leaf litter from Leaf Area estimates
#litter_HFE<- read.csv("HFE leaf litter 2008-2009.csv")
#litter_HFE$date <- as.character(litter_HFE$Date)
#litter_HFE$date <- as.Date(litter_HFE$Date)