library(plyr)
#read data
source("HFE chamber read data.R")
treemass <- read.csv("raw csv/HFE final DM totals.csv")


#COARSE ROOTS
#function to calculate coarse root mass from cores + harvest
croot_mass_calc <- function(rootdfr, harvestdfr){
  croot_cores <- subset(rootdfr, select = c("chamber", "coreno", "wr25",  "wr510",  "wrgt10"))
  croot_cores$cr_core_mass <- with(croot_cores, wr25+wr510+wrgt10)
  cr_core_sum <- ddply(croot_cores, c("chamber"), function(x) data.frame(cr_core_mass=sum(x$cr_core_mass)))
  
  croot_harvest <- subset(harvestdfr, select = c("chamber", "wr25",  "wr5",  "wstump"))
  
  crootmass <- merge(croot_harvest, cr_core_sum, by = "chamber")
  crootmass$coarseroot_mass <- with(crootmass, wr25+wr5+wstump+cr_core_mass)
  return(crootmass)
}
crootmass<- croot_mass_calc(rootM, treemass)


#FINE ROOT MASS, scales fine root cores, roots are <2mm, sampled to hard layer
fine_root_calc <- function(rootdfr){
  
  frootmass <- rootdfr[, 1:6]
  names(frootmass)[6] <- "fr_mass"
  #seperate variable for veryfine+wrlt2...veryfine seem way to large
  frootmass$froot_sum <- with(frootmass, veryfine+fr_mass)
  #mean of 5 cores and at each sampled layer
  fr_cores_mean <- ddply(frootmass, .(chamber, upper), summarise,
                         fr_mass=mean(fr_mass),              
                         fr_sum_mass=mean(froot_sum))
                                              
  fr_cores_sum <- ddply(fr_cores_mean, .(chamber), summarise,
                        fr_mass=sum(fr_mass),
                        fr_sum_mass=sum(fr_sum_mass))
}

frootmass_total <-fine_root_calc(rootM)


#ALL ROOTS, scaled
root_mass_scale <- function(fine, coarse){
  #scaling specs
  core <- (((10/2)^2)*pi) #area of 10cm2 diameter core
  wtc <- (((325/2)^2)*pi) #area of chamber
  scaleup <- ((wtc-core)/core)
  
  fine$fineroot_mass <- fine$fr_mass *scaleup
  fine$fineroot_mass_all <- fine$fr_sum_mass *scaleup
  
  #dfr coarse + fine root, add harvest date
  all_roots <- merge(coarse, fine, by = "chamber")
  all_roots <- subset(all_roots, select = c("chamber", "coarseroot_mass", "fineroot_mass", "fineroot_mass_all"))
  all_roots$Date <- as.Date("2009-03-16")
  all_roots$frootC <- with(all_roots, fineroot_mass*.5)
  all_roots$frootC_all <- with(all_roots, fineroot_mass_all*.5)
  all_roots$CrootC <- with(all_roots, coarseroot_mass*.5)
  
  return(all_roots)
}

root_mass <- root_mass_scale(frootmass_total, crootmass)


write.csv(root_mass, file = "calculated mass/root_mass.csv", row.names=FALSE)

