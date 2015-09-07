##take harvest frewsh mass and wet/dry conversion from harvest cookies to calculated stem dry mass
library(doBy)

# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])


stem_fresh <- read.csv("raw csv/HFE final harvest layer stem fresh mass.csv")
  stem_fresh <- subset(stem_fresh, chamber %in% unique(chambersumm$chamber))
  stem_fresh <- droplevels(stem_fresh)
  
##Stem fresh by layer and by chamber
stem_fresh_layerno <- summaryBy(freshmass ~ chamber + layerno, data=stem_fresh, FUN=sum, keep.names=TRUE)
stem_fresh_cham <- summaryBy(freshmass ~ chamber, data=stem_fresh, FUN=sum, keep.names=TRUE)


#stem density cookies from harvest to get wet conversion
stem_density <- read.csv("raw csv/HFE wood density cookies.csv")
  stem_density <- subset(stem_density, chamber %in% unique(chambersumm$chamber))
  stem_density <- droplevels(stem_density)
  ##perc
  stem_density$percdry <- with(stem_density, (wbark+wwood)/(barkfreshweight+woodfreshweight)*100)

watercontent <- stem_density[,c(1:2, 16)]    
watercontent_mean <- summaryBy(percdry~chamber, data=watercontent, FUN=mean, keep.names=TRUE)

  
##need to merge converion parameter with fresh mass and make a new dataframe
stem_mass_fin <- merge(stem_fresh_cham, watercontent_mean)
stem_mass_fin$stem_mass_dry <- with(stem_mass_fin, freshmass*(percdry/100))


####need to amke a new harvest dataframe that replace the bad dry mass with this new one
treemass <- read.csv("raw csv/HFE final DM totals.csv")

harvest_mass <- merge(treemass[,-6], stem_mass_fin[, c(1,4)])
##simplify to sum of each component and save
harvest_mass2 <- harvest_mass[, c("chamber", "wf", "wbr", "wr","stem_mass_dry")]


write.csv(harvest_mass2, "calculated_mass/harvest_mass_new.csv", row.names=FALSE)








  
  
  
  