setwd("C:/Users/90919620/Google Drive/HFE Database")

setwd("g:/shared/hfe database/")

require(effects)
library(outliers)
library(doBy)

# Put all raw data in here:
# source("HFE chamber read data.R")

# chamber treatments
chambersumm <- read.csv("HFE chamber treatments.csv")

# dataframes that are reused
stem_diameters <- read.csv("HFE Tree Diameters all.csv")
stem_diameters$Date <- as.character(stem_diameters$Date)
stem_diameters$Date <- as.Date(stem_diameters$Date)

stem_height <- read.csv("HFE Tree Height Fixed.csv")
stem_height$Date <- as.character(stem_height$Date)
stem_height$Date <- as.Date(stem_height$Date)

harvest_mass <- read.csv("HFE final harvest biomass by layer.csv")

branch_allometry <- read.csv("HFE branch diameter length.csv")
branch_allometry$Date <- as.character(branch_allometry$Date)
branch_allometry$Date <- as.Date(branch_allometry$Date)

#calculate stem mass 

#from stem volume(above and below 65cm seperately) and density parameters
stemD1 <- subset(stem_diameters, stem_diameters$Date >= "2008-02-21")
#Read tree top heights, convert to cm, set top height diameter to .001cm
stemH1<-subset(stem_height, stem_height$Date >= "2008-02-21")
names(stemH1)[3] <- "Pathlength"
stemH1$Pathlength <- stemH1$Pathlength*100
stemH1$Diameter <- as.numeric(ifelse(stemH1$Pathlength >0, ".001", "NA"))
stemH1$Stemsegmnt <- ifelse(stemH1$Pathlength >0, "1", "NA")

# merge stem diameters with top height data
stem <- rbind(stemD1, stemH1)
stem <- subset(stem, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
chamberorder<-order(stem$chamber, by=stem$Date)
stem <- stem[chamberorder,]

#calcualte length each cookie represents above 65cm only
stem <- subset(stem_height, stem_height$Pathlength > 65)
stem$Lengthvalue <- ifelse(stem$Diameter == .001, 15, 30)

#calculate base stem metrics, add taper below 65cm
stemD_65 <- subset(stem_diameters, stem_diameters$Pathlength == 65)
stemH2 <- subset(stem_height, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
stemH2$Height <- stemH2$Height * 100 #convert m to cm
#estimate Diameter (from cone equations) for 30cm and base
BaseDiameter <- merge(stemD_65, stemH2)
BaseDiameter$BaseD <- with(BaseDiameter, (((Diameter*Pathlength)+(Height*Diameter))/Height))
BaseDiameter$MidPath <- 30
BaseDiameter$MidD <- with(BaseDiameter, (((Diameter*MidPath)+(Height*Diameter))/Height))
#seperate, rename and bind diameters (<65cm) and pathlengths
midDiameter <- BaseDiameter [, c("Date", "chamber", "MidPath", "MidD")]
names(midDiameter)[3:4] <- c("Pathlength", "Diameter")
trunkDiameter <- BaseDiameter[, c("Date", "chamber", "Pathlength", "Diameter")]
stumpDiameter <- BaseDiameter[, c("Date", "chamber", "Pathlength", "BaseD")]
stumpDiameter$Pathlength <- 5
names(stumpDiameter)[4] <- "Diameter"

mainstem <- rbind(midDiameter, trunkDiameter)
mainstem <- rbind(mainstem, stumpDiameter)
chamberorder <- order(mainstem$chamber, by=mainstem$Date)
mainstem<-mainstem[chamberorder,]
mainstem$Lengthvalue <- ifelse(mainstem$Pathlength == 65, 30, mainstem$Pathlength)
mainstem$Stemsegmnt <- 1

# merge base and stem diameters and calculate volume
baseD <- subset(mainstem, select = c("Date", "chamber", "Pathlength", "Diameter", "Lengthvalue", "Stemsegmnt"))
stemV <- rbind(stem, baseD)
chamberorder<-order(stemV$chamber, by=stemV$Date)
stemV <- stemV[chamberorder,]
stemV$Volume <- ((((stemV$Diameter/2)^2)*(pi))*stemV$Lengthvalue)

#calculate stem density parameter
#determine mass and densty per cm for each layer/stem segmnt
stem_density <- read.csv("HFE wood density cookies.csv")
density <- subset(stem_density,select = c("chamber", "layerno", "Stemsegmnt",  "doverbark",  "dunderbark", "freshvolume",  "wbark",	"wwood"))
density <- subset(density, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
density <- droplevels(density)

#density parameter calculations
density$height_cookie <- density$freshvolume / (pi*((density$doverbark/2)^2))
density$woodV <- (((density$dunderbark/2)^2)*pi)*density$height_cookie
density$wood_density <- density$wwood / density$woodV
density$BarkV <- density$freshvolume - density$woodV
density$bark_density <- density$wbark / density$BarkV
density$barkdiam <- with(density, doverbark - dunderbark)

#calculate bark:wood diamter ratio
density$Bark_Wood <- with(density, barkdiam / dunderbark)



#chamber/treatment means
Tree_density_mean <- aggregate(cbind(bark_density , wood_density) ~ chamber, data=density, FUN=mean)
Tree_density_mean_trt <- merge(Tree_density_mean, chambersumm, by = "chamber")

#TEST = are densities different (bark and wood)? YES
t.test(Tree_density_mean$bark_density,  Tree_density_mean$wood_density, var.equal = TRUE)

#TEST = are densities different across trts? NO
t.test(bark_density ~ CO2_treatment, data=Tree_density_mean_trt)
t.test(wood_density ~ CO2_treatment, data=Tree_density_mean_trt)

anova(lm(bark_density ~ CO2_treatment*Water_treatment, data=Tree_density_mean_trt))

# 
# #TEST = are individual trees different from chamber mean? YES
# meanbarkD <- mean(Tree_density_mean$bark_density_cm)
# meanwoodD <- mean(Tree_density_mean$wood_density_cm)
# wilcox.test(Tree_density_mean$bark_density_cm, mu = meanbarkD)
# wilcox.test(Tree_density_mean$wood_density_cm, mu = meanwoodD)

#general weighted mean of density bark, wood, and bark:wood
##can only use if density not different across trees
density_sp <- split(density, density$chamber)

woodMean <- sapply(density_sp, function(x) weighted.mean(x$wood_density, w = x$wwood))
woodD_wm <- data.frame(chamber=names(woodMean), wooddensity_wm=as.vector(woodMean) )

barkMean <- sapply(density_sp, function(x) weighted.mean(x$bark_density, w = x$wbark))
barkD_wm <- data.frame(chamber=names(barkMean), barkdensity_wm=as.vector(barkMean) )

BWratio <- sapply(density_sp, function(x) weighted.mean(x$Bark_Wood, w = x$freshvolume))
BWratio <- data.frame(chamber=names(BWratio), barktowood_ratio=as.vector(BWratio) )


#dataframe with weighted avergages (from layers) of bark and wood density for each chamber, and diameter ratios
wooddensity_wm <- merge(barkD_wm, woodD_wm, by = "chamber")
wooddensity_wm <- merge(wooddensity_wm, BWratio, by = "chamber")

#Merge stem density and volume dataframes, assume density does not change over time
stem_mass <- merge(stemV, wooddensity_wm, by = "chamber")

# Fit power function instead???
# windows()
# with(density, plot(doverbark, barkdiam, xlim=c(0,15), ylim=c(0,3)))
# abline(lm(barkdiam ~ doverbark, data=density))


##calcualte mass
stem_mass$bark_mass <- (stem_mass$Volume * stem_mass$barktowood_ratio) * stem_mass$barkdensity_wm
stem_mass$wood_mass <- (stem_mass$Volume * (1 - stem_mass$barktowood_ratio)) * stem_mass$wooddensity_wm
stem_mass$bole_mass <- stem_mass$bark_mass+stem_mass$wood_mass



# #stem mass calculation by chamber, add month and year metric
Bole_Mass <- aggregate(bole_mass ~ Date + chamber, data = stem_mass, FUN = sum)
# DATS_Bole <- as.POSIXlt(Bole_Mass$Date)
# Bole_Mass$month <- as.numeric(format(DATS_Bole  ,"%m"))
# Bole_Mass$year <- as.numeric(format(DATS_Bole  ,"%Y"))
# Bole_Mass_mean <- aggregate(bole_mass ~ chamber + month + year, FUN = mean, data = Bole_Mass)


#Calculate Branch Mass (>1cm)
#caluclate a density metric to measure branch volmue through time with final harvest data

##write in and merge harvest branch mass with volume
branchM <- subset(harvest_mass, select =c("chamber",  "layerno", "Wbrgt1"))
names(branchM)[3] <- "br_mass"

#calcualte Branch density (Pb) form mass and volume equations
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
branch_dates <- subset(branch_allometry, select = c("Date",  "chamber",  "stemnumber",	"branchnumber",	"diameter",	"length",	"branchBA"))
branch_dates <- merge(branch_dates, branchD, by = "chamber")
branch_dates$branch_mass <- ((branch_dates$branchBA*(branch_dates$length+5))*.75) * branch_dates$branch_density

#branch mass calculation by chamber, add month and year metric
branch_mass_total <- aggregate(branch_mass ~ chamber + Date, data= branch_dates, FUN = sum)
# DATS_BR <- as.POSIXlt(branch_mass_total$Date)
# branch_mass_total$month <- as.numeric(format(DATS_BR ,"%m"))
# branch_mass_total$year <- as.numeric(format(DATS_BR ,"%Y"))
# branch_mass_total <- aggregate(branch_mass ~ chamber + month + year, FUN = mean, data = branch_mass_total)


#Twig and Leaf Mass
# a leaf mass model must first be constructed (leaf parameters from harvest)
# twigs are not measured in allometry through time so need to be fully modelled

##write in harvest mass and get twig mass, leaf area, and leaf mass
twigleafM <- subset(harvest_mass, select =c("chamber",  "layerno", "Wbrlt1", "LA", "Wleaf"))
names(twigleafM)[3:5] <- c("twigmass", "leafarea", "leafmass")
##tree totals(sum layers)
twigleafM <- aggregate(cbind(twigmass, leafarea, leafmass) ~ chamber, FUN = sum, data = twigleafM)

#merge stem  height and leaf harvest
stemH_harv <- subset(stem_height, stem_height$Date == "2009-03-16")
row.names(stemH_harv)<-NULL

twigleaf_harv <- merge(stemH_harv, twigleafM, by = "chamber")
twigleaf_harv  <- subset(twigleaf_harv , chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])  
twigleaf_harv <-  merge(twigleaf_harv, chambersumm)
twigleaf_harv$LAtoheight <- with(twigleaf_harv, leafarea/Height)
twigleaf_harv$leafMtoheight <- with(twigleaf_harv, leafmass/Height)

#leaf mass models (leafarea was best model, others omitted from this script)
leafmass_model <- lm(leafmass ~ leafarea, data = twigleaf_harv)
summary(leafmass_model)

# TEST =  treatment effects within model. NO
leafarea_model_trt <- lm(leafmass ~ leafarea * CO2_treatment * Water_treatment, data = twigleaf_harv)
summary(leafarea_model_trt)
anova(leafarea_model_trt)

#extract coefficients
leafmasspred <- as.data.frame(coef(leafmass_model))
names(leafmasspred)[1] <- "leaf_coef"

#visualize model/test for normality/plot
plot(allEffects(leafarea_model_trt))
shapiro.test(residuals(leafarea_model_trt))
with(twigleaf_harv, plot(leafarea, leafmass, 
                            pch=c(1,19)[Water_treatment], col=CO2_treatment,
                            ylim = c(0, 10000), xlim = c(0, 60)))
abline(leafmass_model)


#twig mass model
twigmass_model <- lm(twigmass ~ leafMtoheight, data= twigleaf_harv)
summary(twigmass_model)
##use to predict twig mass after leaf mass has been predicted

# TEST =  treatment effects within model. NO
twigmass_model_trt <- lm(twigmass ~ leafMtoheight * CO2_treatment * Water_treatment, data= twigleaf_harv)
summary(twigmass_model_trt)
anova(twigmass_model_trt)

#extract coefficients
twigmasspred <- as.data.frame(coef(twigmass_model))
names(twigmasspred)[1] <- "twig_coef"

#visualize model/test for normality
plot(allEffects(twigmass_model_trt))
shapiro.test(residuals(twigmass_model_trt))


##read in leaf and height data
leafA_est <- read.csv("HFE LA estimates alldates.csv")
leafA_est$Date <- as.character(leafA_est$Date)
leafA_est$Date <- as.Date(leafA_est$Date)
leafA_est <- subset(leafA_est, select = c("chamber","Date",  "LAestlin"))

twig_est <- merge(stem_height, leafA_est, by = c("Date", "chamber"), all=TRUE)
# twig_est <- twig_est[complete.cases(twig_est), ]
row.names(twig_est)<-NULL
twig_est <- merge(twig_est, chambersumm, by = "chamber")



#calculate leaf mass
twig_est$leaf_intercept <-  leafmasspred$leaf_coef[1]
twig_est$leaf_slope <- leafmasspred$leaf_coef[2]
twig_est$leafmass <- with(twig_est, (leaf_slope*LAestlin)+leaf_intercept)
#estimate twig mass 
twig_est$leafmasstoheight <- twig_est$leafmass / twig_est$Height 
twig_est$twig_intercept <- twigmasspred$twig_coef[1]
twig_est$twig_slope <- twigmasspred$twig_coef[2]
twig_est$twig_mass <- with(twig_est, ((twig_slope*leafmasstoheight) + twig_intercept))
#possible use leaf count model instead, necessary if we use more dates

Twig_Leaf_mass <- subset(twig_est, select = c("Date", "chamber", "leafmass", "twig_mass"))
# DATS_TW <- as.POSIXlt(Twig_Leaf_mass$Date)
# Twig_Leaf_mass$month <- as.numeric(format(DATS_TW  ,"%m"))
# Twig_Leaf_mass$year <- as.numeric(format(DATS_TW  ,"%Y"))
# Twig_Leaf_mass_mean <- aggregate(cbind(twig_mass, leafmass) ~ chamber + month + year, FUN = mean, data = Twig_Leaf_mass)


#Root Mass
#model estimate of roots <2cm and >2cm diameter seperately

## twig mass, leaf area, and leaf mass from harvest
aboveM <- subset(harvest_mass, select =c("chamber",  "layerno", "Wbrlt1", "LA", "Wleaf", "Wbrgt1", "Ws", "SLA", "BA"))
names(aboveM)[3:9] <- c("twigmass", "leafarea", "leafmass", "branchmass", "stemmass", "leafarea_spec", "basalarea")
aboveM$abovemass <- with(aboveM, twigmass+leafmass+branchmass+stemmass)
##tree totals(sum layers)
aboveM <- aggregate(cbind(twigmass, leafarea, leafmass, branchmass, stemmass, leafarea_spec, basalarea, abovemass) ~ chamber, FUN = sum, data = aboveM)

##harvest  stem height
stemH5 <- subset(stem_height, stem_height$Date == "2009-03-16")
row.names(stemH5)<-NULL

##read in harvest roots
rootM<- read.csv("HFE Fine root cores final harvest.csv")
rootM$wr510 <- as.numeric(rootM$wr510)
#create fine and coarse root vectors
rootM$Frootmass <- with(rootM, veryfine + wrlt2)
rootM$Crootmass <- with(rootM, wr25  + wr510  + wrgt10)

#convert core diameter to chamber(which is roots/tree)
#mean of 5 cores and sum across depths (scale up ~10cm2 diameter core to 325cm chamber, *1055.25)
rootM_total <- aggregate(cbind(Frootmass, Crootmass) ~ chamber + upper, FUN = mean, data = rootM)
rootM_total <- aggregate(cbind(Frootmass, Crootmass) ~ chamber, FUN = sum, data = rootM_total)
#area scale factor
core <- (((10/2)^2)*pi)
hfe <- (((325/2)^2)*pi)
scaleup <- ((hfe-core)/core)

rootM_total$Frootmass <- rootM_total$Frootmass *scaleup
rootM_total$Crootmass <- rootM_total$Crootmass *scaleup
rootM_total$rootmass_all <- rootM_total$Crootmass + rootM_total$Frootmass

root_est <- merge(rootM_total, aboveM, by = "chamber")
root_est <- merge(root_est, stemH5, by = "chamber")
root_est  <- subset(root_est , chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"]) 
root_est$Lmassbyheight <- root_est$leafmass / root_est$Height
root_est$Lareabyheight <- root_est$leafarea / root_est$Height
root_est$rootratio <- root_est$Crootmass / root_est$Frootmass
root_est$RSratio <- root_est$Crootmass / root_est$abovemass #coarseroot/shoot ratio
root_est <- merge(root_est, chambersumm)

###tree #6 looks really bad, 
###but tree #3 is statistically an outlier
#linear root models are total shit without removing, remove 6 for now
chisq.out.test(root_est$abovemass, variance=var(root_est$CO2_treatment), opposite = FALSE)
chisq.out.test(root_est$abovemass, variance=var(root_est$Water_treatment), opposite = FALSE)
boxplot(abovemass ~ Water_treatment, data = root_est)

root_est_no6 <- subset(root_est, root_est$chamber != "ch06")
root_est_no6 <- merge(root_est_no6, chambersumm)

#Fine Root model
Froot_model <- lm(Frootmass ~ Lareabyheight , data = root_est_no6 )
summary(Froot_model)
#TEST = treatment effects. NO
Froot_model_trt <- lm(leafmass ~ Lareabyheight * CO2_treatment * Water_treatment, data = root_est_no6)
summary(Froot_model_trt)
anova(Froot_model_trt)

# extract Coefficients
frootmasspred <- as.data.frame(coef(Froot_model))
names(frootmasspred)[1] <- "Froot_coef"

##visualize model/normality/plot
plot(allEffects(Froot_model_trt))
shapiro.test(residuals(Froot_model_trt))

with(root_est_no6, plot(Lareabyheight, Frootmass, 
                        pch=c(1,19)[Water_treatment], col=CO2_treatment,
                        ylim = c(0, 15000), xlim = c(0, 10)))
abline(Froot_model)

#Coarse Root model
Croot_model <- lm(Crootmass ~  RSratio, data = root_est )
summary(Croot_model)

#TEST = treatment effects. YES
Croot_model_trt <- lm(Crootmass ~  RSratio * CO2_treatment * Water_treatment, data = root_est)
summary(Croot_model_trt)
anova(Croot_model_trt)

# extract Coefficients
Crootmasspred <- as.data.frame(coef(Croot_model))
names(Crootmasspred)[1] <- "Croot_coef"

##visualize model/normality/plot
plot(allEffects(Croot_model_trt))
shapiro.test(residuals(Croot_model_trt))
with(root_est, plot(RSratio, Crootmass, 
                        pch=c(1,19)[Water_treatment], col=CO2_treatment,
                        ylim = c(0,30000), xlim = c(0, 2)))
abline(Croot_model)

#need to extract root-shoot ratio (cannot derive)
Crootratio <- subset(root_est, select = c("chamber", "RSratio"))

#estimate fineroot mass
rootmass_model <- merge(leafA_est, Crootratio, by = "chamber")
rootmass_model$froot_intercept <- frootmasspred$Froot_coef[1]
rootmass_model$froot_slope <- frootmasspred$Froot_coef[2]
rootmass_model$finerootbiomass <- with(rootmass_model, ((froot_slope * LAestlin) + froot_intercept))


# Interpolate missing values
dfr <- data.frame(x=1:100, y=rnorm(100, mean=10, sd=1))
miss <- sample(1:100, 80)
dfr$y[miss] <- NA

dfr_nonna <- subset(dfr, !is.na(y))
apfun <- approxfun(x=dfr_nonna$x, y=dfr_nonna$y)

dfr$ypred <- apfun(dfr$x)


plot(dfr$x, dfr$y)
points(dfr$x, dfr$ypred, type='l', col="red")




#Merge by month/year, reconfig dates
tree_mass <- merge(branch_mass_total, Twig_Leaf_mass_mean, by = c("year","month", "chamber"), all=TRUE)
tree_mass <- merge(tree_mass, Bole_Mass_mean, by = c("year","month", "chamber"), all=TRUE)
tree_mass <- tree_mass[complete.cases(tree_mass), ]
row.names(tree_mass)<-NULL

#Coarse Root mass
#must apply RS ratio after above ground mass is estimated
tree_mass$above_biomass <- with(tree_mass, branch_mass + leafmass  + twig_mass  + bole_mass)
tree_mass <- merge(tree_mass, Crootratio, by = "chamber")
tree_mass$coarserootmass <- tree_mass$above_biomass * tree_mass$RSratio

#finish all components
finerootmass_est <- subset(rootmass_model, select = c("Date",  "chamber",  "finerootbiomass"))
DATS_R <- as.POSIXlt(finerootmass_est$Date)
finerootmass_est$month <- as.numeric(format(DATS_R  ,"%m"))
finerootmass_est$year <- as.numeric(format(DATS_R  ,"%Y"))
finerootmass_est <- aggregate(finerootbiomass ~ chamber + month + year, FUN = mean, data = finerootmass_est)

tree_mass <- merge(tree_mass, finerootmass_est, by = c("year","month", "chamber"), all=TRUE)
tree_mass <- tree_mass[complete.cases(tree_mass), ]
row.names(tree_mass)<-NULL
tree_mass$tree_biomass <- with(tree_mass, branch_mass + leafmass  + twig_mass  + bole_mass + finerootbiomass  +coarserootmass)

tree_mass$date <- ifelse(tree_mass$year == "2008" & tree_mass$month == "4", "2008-04-01", NA)
tree_mass$date <- ifelse(tree_mass$year == "2008" & tree_mass$month == "12", "2008-12-01", tree_mass$date)
tree_mass$date <- ifelse(tree_mass$year == "2008" & tree_mass$month == "11", "2008-11-01", tree_mass$date)
tree_mass$date <- ifelse(tree_mass$year == "2008" & tree_mass$month == "9", "2008-09-01", tree_mass$date)
tree_mass$date <- ifelse(tree_mass$year == "2008" & tree_mass$month == "10", "2008-10-01", tree_mass$date)
tree_mass$date <- ifelse(tree_mass$year == "2009" & tree_mass$month == "1", "2009-01-01", tree_mass$date)
tree_mass$date <- ifelse(tree_mass$year == "2009" & tree_mass$month == "3", "2009-03-01", tree_mass$date)
tree_mass$date <- as.Date(tree_mass$date)

#add litter flux (sum littermass between dates)
# Litter dataframes
litter_HFE<- read.csv("HFE leaf litter 2008-2009.csv")
litter_HFE$date <- as.character(litter_HFE$Date)
litter_HFE$date <- as.Date(litter_HFE$Date)

littertosept <- subset(litter_HFE, litter_HFE$date >= "2008-04-15" & litter_HFE$date <= "2008-09-17")
littertooct <- subset(litter_HFE, litter_HFE$date > "2008-09-17" & litter_HFE$date <= "2008-10-28") 
littertonov <- subset(litter_HFE, litter_HFE$date > "2008-10-28" & litter_HFE$date <= "2008-11-15")    
littertodec <- subset(litter_HFE, litter_HFE$date > "2008-11-15" & litter_HFE$date <= "2008-12-11")
littertojan <- subset(litter_HFE, litter_HFE$date > "2008-12-11" & litter_HFE$date <= "2009-01-15")
littertomar <- subset(litter_HFE, litter_HFE$date > "2009-01-15" & litter_HFE$date <= "2009-03-16")

april_litter <- as.data.frame(chambersumm$chamber)
names(april_litter)[1] <- "chamber"
april_litter  <- subset(april_litter , chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"]) 
april_litter$leaflitter <- "0"
sept_litter <- aggregate(leaflitter ~ chamber, data = littertosept, FUN = sum)                                            
oct_litter <- aggregate(leaflitter ~ chamber, data = littertooct, FUN = sum)
nov_litter <- aggregate(leaflitter ~ chamber, data = littertonov, FUN = sum)
dec_litter <- aggregate(leaflitter ~ chamber, data = littertodec, FUN = sum)                     
jan_litter <- aggregate(leaflitter ~ chamber, data = littertojan, FUN = sum) 
mar_litter <- aggregate(leaflitter ~ chamber, data = littertomar, FUN = sum) 

#add date and rbind
april_litter$date <- "2008-04-01"
sept_litter$date <- "2008-09-01"
oct_litter$date <- "2008-10-01"
nov_litter$date <- "2008-11-01"
dec_litter$date <- "2008-12-01"
jan_litter$date <- "2009-01-01"
mar_litter$date <- "2009-03-01"

#accumulated litter between dates, sept is from april to sept
litterflux <- rbind(april_litter, sept_litter, oct_litter, nov_litter, dec_litter, jan_litter, mar_litter)
litterflux$date <- as.Date(litterflux$date)
chamberorder<-order(litterflux$chamber, by=litterflux$date)
litterflux<-litterflux[chamberorder,]

#merge litter to mass
tree_mass <- merge(tree_mass, litterflux, by = c("chamber", "date"), all = TRUE)
tree_mass$leaflitter <- as.numeric(tree_mass$leaflitter)
tree_mass$tree_biomass <- with(tree_mass, tree_biomass + leaflitter)

#calculate Tree C, assume 50% for all components
tree_mass$treeC <- tree_mass$tree_biomass *.5
tree_mass <- merge(tree_mass, chambersumm)

#treatment means plots
tree_stats <- aggregate(cbind(tree_biomass, treeC) ~ date + CO2_treatment + Water_treatment, data= tree_mass, FUN = mean)

#treatment series for plotting
elevdry <- subset(tree_stats, CO2_treatment == "elevated" & Water_treatment == "dry")
elevwet <- subset(tree_stats, CO2_treatment == "elevated" & Water_treatment == "wet")
ambdry <- subset(tree_stats, CO2_treatment == "ambient" & Water_treatment == "dry")
ambwet <- subset(tree_stats, CO2_treatment == "ambient" & Water_treatment == "wet")
#graph
plot(treeC ~ date, data = elevdry, pch = 19, col = "darkolivegreen4", ylim = c(0,40000))
points(treeC ~ date, data = elevwet, pch = 21, bg = "darkgoldenrod3")
points(treeC ~ date, data = ambdry , pch = 21, bg = "red")
points(treeC ~ date, data = ambwet, pch = 21, bg = "blue")

# TIP: sequence of dates
# seq.Date(from=as.Date("2008-4-15"), length=12, by="month")

##Calculate flux vs mass
#Read WTC flux data
chams <-  paste0("ch", sprintf("%02.0f",1:12))
fns <- paste0("HFE WTC hourly flux GapFilled ",chams,".csv")
allflux <- lapply(fns, read.csv)

#tree chamber flux
flux<-do.call(rbind, allflux)
flux<-data.frame(flux)
WTCflux<- flux[, c("DateTime", "Date", "chamber", "FluxH2Otot", "FluxCO2tot", "sunrise", 
                   "sunset", "daylength", "DOY")]
WTCflux$DateTime<-as.character(WTCflux$DateTime)
WTCflux$DateTime<-as.POSIXct(WTCflux$DateTime, tz = "GMT")
WTCflux$Date <- as.Date(as.character(WTCflux$Date))
DATS <- as.POSIXlt(WTCflux$DateTime)
WTCflux$hour<-DATS$hour
# daytime fluxes
Dayflux<-subset(WTCflux, hour <= sunset & hour >= sunrise, 
                select= c("DateTime", "chamber", "FluxH2Otot", "FluxCO2tot"))

# Total CO2 and H2O fluxes (diurnal or whole day)
#Dayfluxagg <- summaryBy(FluxCO2tot + FluxH2Otot ~ chamber, data=Dayflux, FUN=sum)
WTCfluxagg <- summaryBy(FluxCO2tot + FluxH2Otot ~ chamber + Date, data=WTCflux, FUN=sum)
names(WTCfluxagg)[3:4]<-c("CO2flux","H2Oflux")
# Change units to gC, * 10 for chamber area
WTCfluxagg$CO2flux <- (12 * WTCfluxagg$CO2flux)*10
WTCfluxagg$H2Oflux <- (18 * 10^-3 * WTCfluxagg$H2Oflux)*10
WTCfluxagg <- merge(WTCfluxagg, chambersumm)
#add WUE
WTCfluxagg$WUEflux <- WTCfluxagg$CO2flux / WTCfluxagg$H2Oflux

#total flux C between mass dates
fluxtosept <- subset(WTCfluxagg, WTCfluxagg$Date >= "2008-04-15" & WTCfluxagg$Date <= "2008-09-17")
fluxtooct <- subset(WTCfluxagg, WTCfluxagg$Date > "2008-09-17" & WTCfluxagg$Date <= "2008-10-28") 
fluxtonov <- subset(WTCfluxagg, WTCfluxagg$Date > "2008-10-28" & WTCfluxagg$Date <= "2008-11-15")    
fluxtodec <- subset(WTCfluxagg, WTCfluxagg$Date > "2008-11-15" & WTCfluxagg$Date <= "2008-12-11")
fluxtojan <- subset(WTCfluxagg, WTCfluxagg$Date > "2008-12-11" & WTCfluxagg$Date <= "2009-01-15")
fluxtomar <- subset(WTCfluxagg, WTCfluxagg$Date > "2009-01-15" & WTCfluxagg$Date <= "2009-03-16")

april_flux <- subset(WTCfluxagg, WTCfluxagg$Date == "2008-04-15")
sept_flux <- aggregate(cbind(CO2flux, H2Oflux, WUEflux)  ~ chamber, data = fluxtosept, FUN = sum)                                            
oct_flux <- aggregate(cbind(CO2flux, H2Oflux, WUEflux) ~ chamber, data = fluxtooct, FUN = sum)
nov_flux <- aggregate(cbind(CO2flux, H2Oflux, WUEflux) ~ chamber, data = fluxtonov, FUN = sum)
dec_flux <- aggregate(cbind(CO2flux, H2Oflux, WUEflux) ~ chamber, data = fluxtodec, FUN = sum)                     
jan_flux <- aggregate(cbind(CO2flux, H2Oflux, WUEflux) ~ chamber, data = fluxtojan, FUN = sum) 
mar_flux <- aggregate(cbind(CO2flux, H2Oflux, WUEflux) ~ chamber, data = fluxtomar, FUN = sum) 

#merge with mass
dayfluxtomass <- tree_mass
dayfluxtomass$CO2flux <- ifelse(dayfluxtomass$date == "2008-04-01", april_flux$CO2flux, NA)
dayfluxtomass$CO2flux <- ifelse(dayfluxtomass$date == "2008-09-01", sept_flux$CO2flux, dayfluxtomass$CO2flux)
dayfluxtomass$CO2flux <- ifelse(dayfluxtomass$date == "2008-10-01", oct_flux$CO2flux, dayfluxtomass$CO2flux)
dayfluxtomass$CO2flux <- ifelse(dayfluxtomass$date == "2008-11-01", nov_flux$CO2flux, dayfluxtomass$CO2flux)
dayfluxtomass$CO2flux <- ifelse(dayfluxtomass$date == "2008-12-01", dec_flux$CO2flux, dayfluxtomass$CO2flux)
dayfluxtomass$CO2flux <- ifelse(dayfluxtomass$date == "2009-01-01", jan_flux$CO2flux, dayfluxtomass$CO2flux)
dayfluxtomass$CO2flux <- ifelse(dayfluxtomass$date == "2009-03-01", mar_flux$CO2flux, dayfluxtomass$CO2flux)

#plot all points last 6months(april flux is obviously off (not a cumultaive flux, so omitted))
dayfluxtomass_noapr <- subset(dayfluxtomass, dayfluxtomass$date != "2008-04-01")
with(dayfluxtomass_noapr, plot(CO2flux, treeC, 
                         xlim=c(0,50000), ylim=c(0,50000),
                         pch=c(1,19)[Water_treatment], col=CO2_treatment))

abline(0,1)
lmMassFlux <- lm(treeC ~ CO2flux, data=dayfluxtomass_noapr)
abline(lmMassFlux, lty=5)

#plot across time
apr <- subset(dayfluxtomass, date == "2008-04-01")
sept <- subset(dayfluxtomass, date == "2008-09-01")
oct <- subset(dayfluxtomass, date == "2008-10-01")
nov <- subset(dayfluxtomass, date == "2008-11-01")
dec <- subset(dayfluxtomass, date == "2008-12-01")
jan <- subset(dayfluxtomass, date == "2009-01-01")
mar <- subset(dayfluxtomass, date == "2009-03-01")

#PLOT flux vs Cmass, with and without april mass

palette(c( "black", "red"))
plot(CO2flux ~ treeC, data = sept, 
     pch=c(1,19)[Water_treatment], col=CO2_treatment, ylim = c(0,50000), xlim = c(0, 50000))
points(CO2flux ~ treeC, data = oct,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ treeC, data = nov,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ treeC, data = dec,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ treeC, data = jan,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ treeC, data = mar,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
abline(0,1)
##subtract april mass and replot
sept$C_yr <- sept$treeC - apr$treeC
oct$C_yr <- oct$treeC - apr$treeC
nov$C_yr <- nov$treeC - apr$treeC
dec$C_yr <- dec$treeC - apr$treeC
jan$C_yr <- jan$treeC - apr$treeC
mar$C_yr <- mar$treeC - apr$treeC

palette(c( "blue", "red"))
plot(CO2flux ~ C_yr, data = sept, 
     pch=c(1,19)[Water_treatment], col=CO2_treatment, ylim = c(0,50000), xlim = c(0, 25000))
points(CO2flux ~ C_yr, data = oct,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ C_yr, data = nov,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ C_yr, data = dec,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ C_yr, data = jan,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ C_yr, data = mar,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
abline(0,1)



