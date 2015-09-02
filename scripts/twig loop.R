
setwd("C:/Users/90919620/Google Drive/HFE Database")

#all raw data here:
source("HFE chamber read data.R")


twig_est <- subset(branches, select = c("chamber",  "layerno","branchnumber", "diameter", "length", "Wbrlt1", "Wbrgt1", "br2order"))
#twig_est <- aggregate(cbind(diameter, length, Wbrlt1, Wbrgt1)~ chamber + layerno, data = branches, FUN=mean)
names(twig_est)[6] <- "twig"
names(twig_est)[7] <- "branch"

#dry mass 
DWbranches <- subset(DWbranches, select = c("chamber",  "layerno", "Wbrlt1",  "Wbrgt1", "DWbrlt1", "DWbrgt1"))
DWbranches$percDWbranch <- with(DWbranches, DWbrgt1 / Wbrgt1)
DWbranches$percDWtwig <-  with(DWbranches, DWbrlt1 /  Wbrlt1)

#merge sample branch allometry with dry mass
twig_est <- merge(twig_est, subset(DWbranches, select = c("chamber",  "layerno", "percDWbranch", "percDWtwig"),
                                   by = c("chamber" , "layerno")))
twig_est  <- subset(twig_est , chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"]) 
twig_est <- droplevels(twig_est)
twig_est$layerno <- as.factor(twig_est$layerno)
twig_est$twig <- as.numeric(twig_est$twig)
#assume water content same within a layer

twig_est$twig_dry <- with(twig_est, twig * percDWtwig)
twig_est$branch_dry <- with(twig_est, branch * percDWbranch)
twig_est$branch_dry <- ifelse(twig_est$branch == 0, 0, twig_est$branch_dry)
twig_est$branch_volume <- with(twig_est, (length *(pi*((diameter/2)^2)))*.75)
twig_est <- merge(twig_est, chambersumm)

#twig from sample branchesmodel, so many zeros for branches....model number of second order branches, then dry twigs from those
#will not be able to estimate twig mass when intial branch/twig is less than 1cm


# 
# 
# ch <- "ch01"
# 
# twig_cham <- subset(twig_est, chamber == ch)
# 
# 
# 
# 
# branches$Wbr <- with(branches, Wbrlt1 + Wbrgt1)
# 



# Make empty list, dataframes will be stored in it
alltrees <- list()

#loop with 2nd order branch and twig model for each chamber tree
#twig_est_tree <-split(twig_est, twig_est$chamber)

for(i in unique(twig_est$chamber)) {
  
  twig_cham <- subset(twig_est, chamber == i)
  
  #The 0 + suppresses the fitting of the intercept by lm.
  intercept <- 0.0
  
  #estimate 2nd order branches first
  br2order_tree <- lm(I(br2order - intercept) ~ 0 + length, data = twig_cham)
  anova(br2order_tree)
  
  #extract coefficients 
  br2orderpred_tree <- as.data.frame(coef(br2order_tree))
  names(br2orderpred_tree)[1] <- "br2order_coef"

  #estimate dry mass of twigs from second order branch estimates
  drytwig_tree <- lm(I(twig_dry - intercept) ~ 0 + br2order, data = twig_cham)
  anova(drytwig_tree)
  
  #extract coefficients 
  twigpred_tree <- as.data.frame(coef(drytwig_tree))
  names(twigpred_tree)[1] <- "twig_coef"
  
  allcoefs <- merge(br2orderpred_tree, twigpred_tree)
  
  # store result in list
  alltrees[[i]] <- allcoefs
  #write.csv(br2orderpred_tree, "output.csv", append=TRUE)
}

alltrees <- do.call(rbind, alltrees)


##write in and merge harvest branch mass with volume
branchM <- subset(harvest_mass, select =c("chamber",  "layerno", "Wbrgt1"))
names(branchM)[3] <- "br_mass"

#calcualte Branch density (Pb) from mass and volume equations
#assume branches as cyclinders, adj with form factor of .75(cone= 2/3's of cyldiner(1))
branch_diam <- subset(branch_allometry, branch_allometry$Date == "2009-03-16")
branch_diam <- subset(branch_diam, select = c("Date", "chamber", "stemnumber", "branchnumber", "diameter", "length", "branchBA"))
branch_diam <- branch_diam[complete.cases(branch_diam),]
branch_diam$startlength <- 5
branch_diam$Volume <- (branch_diam$branchBA*(branch_diam$length+5))*.75 # add 5cm to height, assuming no difference between base and insertion diam
row.names(branch_diam)<-NULL

#sum data for harvest branch mass(by layer) an volume (branch#)
BRmass_tot<- aggregate(br_mass ~ chamber, data = branchM, FUN = sum)
branch_volume_tot <- aggregate(Volume ~ chamber, data = branch_diam, FUN = sum)

#calculate chamber branch denisty from Mass/Volume (shape factor within volume equation)
#assumer no difference between bark and wood density of branches
branchD <- merge(branch_volume_tot, BRmass_tot, by = "chamber")
branchD$branch_density <- branchD$br_mass / branchD$Volume
branchD <- subset(branchD, select = c("chamber" , "branch_density"))


#calucalte branch mass through time
#as before assumer shape factor of .75 and add 5cm to length(assumer no taper)
branch_dates <- subset(branch_allometry, select = c("Date",  "chamber",  "stemnumber",  "branchnumber",  "diameter",	"length",	"branchBA"))
branch_dates <- merge(branch_dates, branchD, by = "chamber")

# Make empty list, dataframes will be stored in it
twigbranch<- list()

for(i in unique(branch_dates$chamber)) {
  
  branch_cham <- subset(branch_dates, chamber == i)

  #calucalte branch mass through time
  #as before assumer shape factor of .75 and add 5cm to length(assumer no taper)
  branch_cham$branch_mass <- ((branch_cham$branchBA*(branch_cham$length+5))*.75) * branch_cham$branch_density
  
  #calculate twig mass through time(need branch mass from above and both equations)
  br2order_slope <- alltrees$br2order_coef
  branch_cham$br2order <- (br2order_slope * branch_cham$length) +0

  twig_slope <-  alltrees$twigpred_tree      
  branch_cham$twig_mass <- (twig_slope * branch_cham$br2order) + 0
  
  # store result in list
  twigbranch[[i]] <- branch_cham
}

twigbranch <- do.call(rbind, twigbranch)
  
#total branch and twig mass by chamber for each date
branch_mass_total <- aggregate(cbind(branch_mass, twig_mass) ~ chamber + Date, data= twigbranch, FUN = sum)


