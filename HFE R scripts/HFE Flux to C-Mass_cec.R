setwd("C:/Users/90919620/Google Drive/HFE Database")
require(effects)
library(outliers)

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

#calculate stem density functions
#determine mass and densty per cm for each layer/stem segmnt
stem_density <- read.csv("HFE wood density cookies.csv")
density <- subset(stem_density,select = c("chamber", "layerno", "Stemsegmnt",  "doverbark",  "dunderbark", "freshvolume",  "wbark",	"wwood"))
density <- subset(density, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])

#density parameters calculations
density$diam_cookie <- density$doverbark + density$dunderbark
density$height_cookie <- density$freshvolume / ((pi)*((density$diam_cookie/2)^2))
density$woodV <- (((density$dunderbark/2)^2)*pi)*density$height_cookie
density$wood_density <- density$wwood / density$woodV
density$BarkV <- density$freshvolume - density$woodV
density$bark_density <- density$wbark / density$BarkV
###mass and density by cm, these may be more standard for above analyses (not by cookie)
density$mass_wood_cm <- density$wwood/density$height_cookie
density$wood_density_cm <- density$mass_wood_cm / density$woodV
density$mass_bark_cm <- density$wbark/density$height_cookie
density$bark_density_cm <- density$mass_bark_cm / density$BarkV
#calculate bark:wood diamter ratio
density$Bark_Wood <- density$dunderbark/density$doverbark

#chamber/treatment means
###switched to using standarized densities (by cm)
Tree_density_mean <- aggregate(cbind(bark_density_cm , wood_density_cm) ~ chamber, data=density, FUN=mean)
Tree_density_mean_trt <- merge(Tree_density_mean, chambersumm, by = "chamber")

#TEST = are densities different (bark and wood)? YES
t.test(Tree_density_mean$bark_density_cm,  Tree_density_mean$wood_density_cm, var.equal = TRUE)

#TEST = are densities different across trts? NO
barkD_dry <-  subset(Tree_density_mean_trt, Water_treatment == "dry")
barkD_wet <-  subset(Tree_density_mean_trt, Water_treatment == "wet")
barkD_ele <-  subset(Tree_density_mean_trt, CO2_treatment == "elevated")
barkD_amb <-  subset(Tree_density_mean_trt, CO2_treatment == "ambient")
t.test(barkD_dry$bark_density_cm, barkD_wet$bark_density_cm, var.equal = TRUE)
t.test(barkD_dry$wood_density_cm,barkD_wet$wood_density_cm, var.equal = TRUE)
t.test(barkD_ele$bark_density_cm, barkD_amb$bark_density_cm, var.equal = TRUE)
t.test(barkD_ele$wood_density_cm,barkD_amb$wood_density_cm, var.equal = TRUE)

#TEST = are individual trees different from each other? YES
meanbarkD <- mean(Tree_density_mean$bark_density_cm)
meanwoodD <- mean(Tree_density_mean$wood_density_cm)
wilcox.test(Tree_density_mean$bark_density_cm, mu = meanbarkD)
wilcox.test(Tree_density_mean$wood_density_cm, mu = meanwoodD)

#general weighted mean of density bark, wood, and bark:wood
##can only use if density not different across trees
density_sp <- split(density, density$chamber)

woodD_wm <- as.data.frame(lapply(density_sp, function(x) weighted.mean(x$wood_density_cm, w = x$mass_wood_cm)))
woodD_wm <- t(woodD_wm)
woodD_wm <- as.data.frame(woodD_wm[1:12,])
woodD_wm <- data.frame(as.character(rownames(woodD_wm)),woodD_wm)
colnames(woodD_wm)[1]="chamber"
names(woodD_wm)[2] <- "wooddensity_wm"

barkD_wm <- as.data.frame(lapply(density_sp, function(x) weighted.mean(x$bark_density_cm, w = x$mass_bark_cm)))
barkD_wm <- t(barkD_wm)
barkD_wm <- as.data.frame(barkD_wm[1:12,])
barkD_wm <- data.frame(as.character(rownames(barkD_wm)),barkD_wm)
colnames(barkD_wm)[1]="chamber"
names(barkD_wm)[2] <- "barkdensity_wm"
row.names(barkD_wm)<-NULL

BWratio <- as.data.frame(lapply(density_sp, function(x) weighted.mean(x$Bark_Wood, w = x$freshvolume)))
BWratio <- t(BWratio)
BWratio <- as.data.frame(BWratio[1:12,])
BWratio <- data.frame(as.character(rownames(BWratio)), BWratio)
colnames(BWratio)[1] = "chamber"
names(BWratio)[2] <- "barktowood_ratio"
row.names(BWratio) <- NULL

#dataframe with weighted avergages (from layers) of bark and wood density for each chamber, and diameter ratios
wooddensity_wm <- merge(barkD_wm, woodD_wm, by = "chamber")
wooddensity_wm <- merge(wooddensity_wm, BWratio, by = "chamber")

#Merge stem density and volume dataframes, assume density does not change over time
stem_mass <- merge(stemV, wooddensity_wm, by = "chamber")
##calcualte mass
stem_mass$bark_mass <- (stem_mass$Volume * stem_mass$barktowood_ratio) * stem_mass$barkdensity_wm
stem_mass$wood_mass <- (stem_mass$Volume * (1 - stem_mass$barktowood_ratio)) * stem_mass$wooddensity_wm
stem_mass$bole_mass <- stem_mass$bark_mass+stem_mass$wood_mass

#stem mass calculation by chamber, add month and year metric
Bole_Mass <- aggregate(bole_mass ~ Date + chamber, data = stem_mass, FUN = sum)
DATS_Bole <- as.POSIXlt(Bole_Mass$Date)
Bole_Mass$month <- as.numeric(format(DATS_Bole  ,"%m"))
Bole_Mass$year <- as.numeric(format(DATS_Bole  ,"%Y"))
Bole_Mass_mean <- aggregate(bole_mass ~ chamber + month + year, FUN = mean, data = Bole_Mass)


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
DATS_BR <- as.POSIXlt(branch_mass_total$Date)
branch_mass_total$month <- as.numeric(format(DATS_BR ,"%m"))
branch_mass_total$year <- as.numeric(format(DATS_BR ,"%Y"))
branch_mass_total <- aggregate(branch_mass ~ chamber + month + year, FUN = mean, data = branch_mass_total)


#Twig and Leaf Mass
# a leaf mass model must first be contructed (leaf parameters from harvest)
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

#leaf mass models (leafarea/height was best model, others omitted from this script)
leafmass_model <- lm(leafmass ~ LAtoheight, data = twigleaf_harv)
summary(leafmass_model)

# TEST =  treatment effects within model. NO
leafarea_model_trt <- lm(leafmass ~ LAtoheight * CO2_treatment * Water_treatment, data = twigleaf_harv)
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
twigmass_model <- lm(twigmass ~ leafMtoheight, data= harvest_mass_trt)
summary(twigmass_model)
##use to predict twig mass after leaf mass has been predicted

# TEST =  treatment effects within model. NO
twigmass_model_trt <- lm(twigmass ~ leafMtoheight * CO2_treatment * Water_treatment, data= harvest_mass_trt)
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
twig_est <- twig_est[complete.cases(twig_est), ]
row.names(twig_est)<-NULL
twig_est <- merge(twig_est, chambersumm, by = "chamber")

#calculate leaf mass
twig_est$leaf_intercept <-  leafmasspred$leaf_coef[1]
twig_est$leaf_slope <- leafmasspred$leaf_coef[2]
twig_est$LAestlin <- twig_est$LAestlin
twig_est$leafmass <- with(twig_est, (leaf_slope*LAestlin)+leaf_intercept)
#estimate twig mass 
twig_est$leafmasstoheight <- twig_est$leafmass / twig_est$Height 
twig_est$twig_intercept <- twigmasspred$twig_coef[1]
twig_est$twig_slope <- twigmasspred$twig_coef[2]
twig_est$twig_mass <- with(twig_est, ((twig_slope*leafmasstoheight) + twig_intercept))
#possible use leaf count model instead, necessary if we use more dates

Twig_Leaf_mass <- subset(twig_est, select = c("Date", "chamber", "leafmass", "twig_mass"))
DATS_TW <- as.POSIXlt(Twig_Leaf_mass$Date)
Twig_Leaf_mass$month <- as.numeric(format(DATS_TW  ,"%m"))
Twig_Leaf_mass$year <- as.numeric(format(DATS_TW  ,"%Y"))
Twig_Leaf_mass_mean <- aggregate(cbind(twig_mass, leafmass) ~ chamber + month + year, FUN = mean, data = Twig_Leaf_mass)

#Root Mass
#model estimate of roots <2cm and >2cm diameter seperately

## twig mass, leaf area, and leaf mass from harvest
aboveM <- subset(harvest_mass, select =c("chamber",  "layerno", "Wbrlt1", "LA", "Wleaf", "Wbrgt1", "Ws", "SLA", "BA"))
names(aboveM)[3:9] <- c("twigmass", "leafarea", "leafmass", "branchmass", "stemmass", "leafarea_spec", "basalarea")
aboveM$abovemass <- with(aboveM, twigmass+leafmass+branchmass+stemmass)
##tree totals(sum layers)
aboveM <- aggregate(cbind(twigmass, leafarea, leafmass, branchmass, stemmass, leafarea_spec, basalarea, abovemass) ~ chamber, FUN = sum, data = aboveM)

##harvest  stem height
stemH5 <- subset(stem_height, stemH$Date == "2009-03-16")
row.names(stemH)<-NULL

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

rootM_total <- aggregate(cbind(Frootmass, Crootmass) ~ chamber + upper, FUN = sum, data = rootM)
rootM_total$Frootmass <- rootM_total$Frootmass *scaleup
rootM_total$Crootmass <- rootM_total$Crootmass *scaleup
rootM_total <- aggregate(cbind(Frootmass, Crootmass) ~ chamber, FUN = sum, data = rootM_total)
rootM_total$rootmass_all <- rootM_total$Crootmass + rootM_total$Frootmass

root_est <- merge(rootM_total, aboveM, by = "chamber")
root_est <- merge(root_est, stemH5, by = "chamber")
root_est  <- subset(root_est , chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"]) 
root_est$Lmassbyheight <- root_est$leafmass / root_est$Height
root_est$Lareabyheight <- root_est$leafarea / root_est$Height
root_est$rootratio <- root_est$Crootmass / root_est$Frootmass
root_est <- merge(root_est, chambersumm)

###tree # 6 is really bad, 
###but tree #3 is statistically an outlier
#linear root models are total shit without removing, remove 3 for now
chisq.out.test(root_est$abovemass, variance=var(root_est$CO2_treatment), opposite = FALSE)
chisq.out.test(root_est$abovemass, variance=var(root_est$Water_treatment), opposite = FALSE)
boxplot(abovemass ~ Water_treatment, data = root_est)

root_est_no3 <- subset(root_est, root_est$chamber != "ch03")
root_est_no3 <- merge(root_est_no6, chambersumm)

#Fine Root model
Froot_model <- lm(Frootmass ~ Lareabyheight , data = root_est_no3 )
summary(Froot_model)
#TEST = treatment effects. NO
Froot_model_trt <- lm(leafmass ~ Lareabyheight * CO2_treatment * Water_treatment, data = root_est_no3)
summary(Froot_model_trt)
anova(Froot_model_trt)

# extract Coefficients
frootmasspred <- as.data.frame(coef(Froot_model))
names(frootmasspred)[1] <- "Froot_coef"

##visualize model/normality/plot
plot(allEffects(Froot_model_trt))
shapiro.test(residuals(Froot_model_trt))

palette(c( "black", "red"))
with(root_est_no6, plot(Lareabyheight, Frootmass, 
                        pch=c(1,19)[Water_treatment], col=CO2_treatment,
                        ylim = c(0, 15000), xlim = c(0, 10)))
abline(Froot_model)


#Coarse Root model
Croot_model <- lm(Crootmass ~  rootratio, data = root_est )
summary(Croot_model)
#TEST = treatment effects. NO
Croot_model_trt <- lm(Crootmass ~  rootratio * CO2_treatment * Water_treatment, data = root_est_no6)
summary(Croot_model_trt)
anova(Croot_model_trt)
# extract Coefficients
Crootmasspred <- as.data.frame(coef(Croot_model))
names(frootmasspred)[1] <- "Croot_coef"

##visualize model/normality/plot
plot(allEffects(Croot_model_trt))
shapiro.test(residuals(Croot_model_trt))
with(root_est_no6, plot(rootratio, Crootmass, 
                        pch=c(1,19)[Water_treatment], col=CO2_treatment,
                        ylim = c(0, 100), xlim = c(0, 5)))
abline(Croot_model)


#need to extract root ratio (cannot derive)
rootratio <- subset(root_est, select = c("chamber", "rootratio"))

#estimate root mass
rootmass_model <- merge(leafA_est, rootratio, by = "chamber")
str(rootmass_model)

stem_height1  <- subset(stem_height , chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"]) 

rootmass_model <- merge(rootmass_model, stem_height1, by = c("Date", "chamber"), all=TRUE)
rootmass_model <- rootmass_model[complete.cases(rootmass_model), ]
rootmass_model$leafareabyheight <- rootmass_model$LAestlin / rootmass_model$Height
rootmass_model$froot_intercept <- frootmasspred$Froot_coef[1]
rootmass_model$froot_slope <- frootmasspred$Froot_coef[2]
#trouble with extracting slope and intercept from frootmass pred data frame = just copied numbers below
rootmass_model$finerootbiomass <- with(rootmass_model, ((909.3295 * leafareabyheight) + 3312.1353))
rootmass_model$coarserootmass <-  rootmass_model$finerootbiomass * rootmass_model$rootratio

rootmass_est <- subset(rootmass_model, select = c("Date",  "chamber",	"finerootbiomass",	"coarserootmass"))
row.names(rootmass_est)<-NULL
DATS_R <- as.POSIXlt(rootmass_est$Date)
rootmass_est$month <- as.numeric(format(DATS_R  ,"%m"))
rootmass_est$year <- as.numeric(format(DATS_R  ,"%Y"))
rootmass_est_mean <- aggregate(cbind(finerootbiomass, coarserootmass) ~ chamber + month + year, FUN = mean, data = rootmass_est)

#Merge by month/year, reconfig dates
tree_mass <- merge(Branch_mass_mean, Twig_Leaf_mass_mean, by = c("year","month", "chamber"), all=TRUE)
tree_mass <- merge(tree_mass, Stem_mass_mean, by = c("year","month", "chamber"), all=TRUE)
tree_mass <- merge(tree_mass, Root_mass_mean, by = c("year","month", "chamber"), all=TRUE)
tree_mass <- tree_mass[complete.cases(tree_mass), ]
row.names(tree_mass)<-NULL
tree_mass$tree_biomass <- with(tree_mass, branch_mass + leafmass  + twig_mass  + bole_mass + finerootbiomass  +coarserootmass)
tree_mass <- merge(tree_mass, chambersumm, by = "chamber")

tree_mass$date <- ifelse(tree_mass$year == "2008" & tree_mass$month == "4", "2008-04-01", NA)
tree_mass$date <- ifelse(tree_mass$year == "2008" & tree_mass$month == "12", "2008-12-01", tree_mass$date)
tree_mass$date <- ifelse(tree_mass$year == "2008" & tree_mass$month == "11", "2008-11-01", tree_mass$date)
tree_mass$date <- ifelse(tree_mass$year == "2008" & tree_mass$month == "9", "2008-09-01", tree_mass$date)
tree_mass$date <- ifelse(tree_mass$year == "2008" & tree_mass$month == "10", "2008-10-01", tree_mass$date)
tree_mass$date <- ifelse(tree_mass$year == "2009" & tree_mass$month == "1", "2009-01-01", tree_mass$date)
tree_mass$date <- ifelse(tree_mass$year == "2009" & tree_mass$month == "3", "2009-03-01", tree_mass$date)
tree_mass$date <- as.Date(tree_mass$date)

###replace estimated data for last date with harvest data
treemass_noharv <- subset(tree_mass[, c("chamber", "tree_biomass", "date")], tree_mass$date != "2009-03-01")

treemass_harv<-read.csv("HFE final DM totals.csv")
treemass_harv<-subset(treemass_harv, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
treemass_harv$tree_biomass<- with(treemass_harv, wr + wf + wbr + ws)
treemass_harv <- treemass_harv[, c("chamber", "tree_biomass")]
treemass_harv$date <- as.Date(as.character("2009-03-16"))
row.names(treemass_harv)<-NULL

tree_allmass <- rbind(treemass_noharv, treemass_harv)
tree_allmass <- merge(tree_allmass, chambersumm)

#calculate Tree C, assume 50% for all components
tree_allmass$treeC <- tree_allmass$tree_biomass *.5

#treatment means plots
tree_stats <- summaryBy(tree_biomass ~ date + CO2_treatment + Water_treatment, data= tree_allmass, Fun = c(mean, sd))
tree_stats$treeC <- tree_stats$tree_biomass *.5
#treatment series for plotting
elevdry <- subset(tree_stats, CO2_treatment == "elevated" & Water_treatment == "dry")
elevwet <- subset(tree_stats, CO2_treatment == "elevated" & Water_treatment == "wet")
ambdry <- subset(tree_stats, CO2_treatment == "ambient" & Water_treatment == "dry")
ambwet <- subset(tree_stats, CO2_treatment == "ambient" & Water_treatment == "wet")
#graph
plot(treeC ~ date, data = elevdry, pch = 19, col = "darkolivegreen4", ylim = c(0,25000))
points(treeC ~ date, data = elevwet, pch = 21, bg = "darkgoldenrod3")
points(treeC ~ date, data = ambdry , pch = 21, bg = "red")
points(treeC ~ date, data = ambwet, pch = 21, bg = "blue")

##Calculate flux vs mass

#Read WTC flux data
chams <-  paste0("ch", sprintf("%02.0f",1:12))
fns <- paste0("HFE WTC hourly flux GapFilled ",chams,".csv")
allflux <- lapply(fns, read.csv)

# Soil respiration (from collars+static average over one year, mean will cover winter and summer now)

#calcualte mean of soil respiraton from collars a/b
collars<-read.csv("HFE-I Soil Respiration Collars.csv")
collars$Date <- as.character(collars$Date)
collars$Date <- as.Date(collars$Date)
collars <- collars[,1:6]
names(collars)
collars_mean <- aggregate(cbind(SoilCO2Efflux, Tsoil, VWC) ~ Date + chamber, data= collars, FUN = mean)
#calcualte mean of soil respiraton from staic chambers
static<-read.csv("HFE-I Soil Respiration Static Chambers.csv")
static$Date <- as.character(static$Date)
static$Date <- as.Date(static$Date)
static <- static[,1:5]
names(static)
static_mean <- aggregate(cbind(SoilCO2Efflux, Tsoil, VWC) ~ Date + chamber, data= static, FUN = mean)

#merge dataframes, now have more dates of soil respiration (big assumption that values are similar)
soilR <- rbind(collars_mean, static_mean)
chamberorder<-order(soilR$chamber, by=soilR$Date)
soilR <- soilR[chamberorder,]
soilR <- merge(soilR, chambersumm)

#plot
palette(c("black","red"))
with(soilR, plot(Date, SoilCO2Efflux, pch=c(1,19)[Water_treatment], col=CO2_treatment))

# Simple first solution: average across all data
soilR_means <-  merge(soilR_means, chambersumm)
soilR_means <- subset(soilR_means, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
#TEST = is soil respiration different by treatment. NO
soilR_model <- lm(SoilCO2Efflux ~ CO2_treatment*Water_treatment, data=soilR_means)
summary(soilR_model)
anova(soilR_model)
#soil respiration by chamber
soilR_means <- aggregate(SoilCO2Efflux ~ chamber, data=soilR, FUN=mean)
soilR_means$SoilCO2Efflux_mumolm2s1 <-  10^6 * (1/12) * 10^-3 * soilR_means$SoilCO2Efflux / 3600
soilR_means$SoilCO2Efflux_gCm2d1 <- 10^-3 * soilR_means$SoilCO2Efflux * 24

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
WTCfluxagg <- summaryBy(FluxCO2tot + FluxH2Otot ~ chamber, data=WTCflux, FUN=sum)
names(WTCfluxagg)[2:3]<-c("CO2flux","H2Oflux")
# Change units to gC
WTCfluxagg$CO2flux <- 12 * WTCfluxagg$CO2flux
WTCfluxagg$H2Oflux <- 18 * 10^-3 * WTCfluxagg$H2Oflux
WTCfluxagg <- merge(WTCfluxagg, chambersumm)
#add WUE
WTCfluxagg$WUEflux <- WTCfluxagg$CO2flux / WTCfluxagg$H2Oflux

#add #of days for each date for mass
#total flux C over those days
ndays_apr <- as.Date("2008-04-15") - as.Date("2008-04-15")
ndays_sept <- as.Date("2008-09-17") - as.Date("2008-04-15")
ndays_oct <- as.Date("2008-10-28") - as.Date("2008-04-15")
ndays_nov <- as.Date("2008-11-15") - as.Date("2008-04-15")
ndays_dec <- as.Date("2008-12-11") - as.Date("2008-04-15")
ndays_jan <- as.Date("2009-01-15") - as.Date("2008-04-15")
ndays_mar <- as.Date("2009-03-16") - as.Date("2008-04-15")

soilR_means$SoilCO2_gCtot_apr <- 10*ndays_apr*soilR_means$SoilCO2Efflux_gCm2d1
soilR_means$SoilCO2_gCtot_sept <- 10*ndays_sept*soilR_means$SoilCO2Efflux_gCm2d1
soilR_means$SoilCO2_gCtot_oct <- 10*ndays_oct*soilR_means$SoilCO2Efflux_gCm2d1
soilR_means$SoilCO2_gCtot_nov <- 10*ndays_nov*soilR_means$SoilCO2Efflux_gCm2d1
soilR_means$SoilCO2_gCtot_dec <- 10*ndays_dec*soilR_means$SoilCO2Efflux_gCm2d1
soilR_means$SoilCO2_gCtot_jan <- 10*ndays_jan*soilR_means$SoilCO2Efflux_gCm2d1
soilR_means$SoilCO2_gCtot_mar <- 10*ndays_mar*soilR_means$SoilCO2Efflux_gCm2d1

#merge with mass
dayfluxtomass <- tree_allmass
dayfluxtomass$SoilCO2Efflux_gCtot <- ifelse(dayfluxtomass$date == "2008-04-01", soilR_means$SoilCO2_gCtot_apr, NA)
dayfluxtomass$SoilCO2Efflux_gCtot <- ifelse(dayfluxtomass$date == "2008-09-01", soilR_means$SoilCO2_gCtot_sept, dayfluxtomass$SoilCO2Efflux_gCtot)
dayfluxtomass$SoilCO2Efflux_gCtot <- ifelse(dayfluxtomass$date == "2008-10-01", soilR_means$SoilCO2_gCtot_oct, dayfluxtomass$SoilCO2Efflux_gCtot)
dayfluxtomass$SoilCO2Efflux_gCtot <- ifelse(dayfluxtomass$date == "2008-11-01", soilR_means$SoilCO2_gCtot_nov, dayfluxtomass$SoilCO2Efflux_gCtot)
dayfluxtomass$SoilCO2Efflux_gCtot <- ifelse(dayfluxtomass$date == "2008-12-01", soilR_means$SoilCO2_gCtot_dec, dayfluxtomass$SoilCO2Efflux_gCtot)
dayfluxtomass$SoilCO2Efflux_gCtot <- ifelse(dayfluxtomass$date == "2009-01-01", soilR_means$SoilCO2_gCtot_jan, dayfluxtomass$SoilCO2Efflux_gCtot)
dayfluxtomass$SoilCO2Efflux_gCtot <- ifelse(dayfluxtomass$date == "2009-03-16", soilR_means$SoilCO2_gCtot_mar, dayfluxtomass$SoilCO2Efflux_gCtot)
names(dayfluxtomass)[8] <- "CO2flux"

apr <- subset(dayfluxtomass, date == "2008-04-01")
sept <- subset(dayfluxtomass, date == "2008-09-01")
oct <- subset(dayfluxtomass, date == "2008-10-01")
nov <- subset(dayfluxtomass, date == "2008-11-01")
dec <- subset(dayfluxtomass, date == "2008-12-01")
jan <- subset(dayfluxtomass, date == "2009-01-01")
mar <- subset(dayfluxtomass, date == "2009-03-16")

#PLOT flux vs Cmass, with and without april mass

palette(c( "black", "red"))
plot(CO2flux ~ treeC, data = sept, 
     pch=c(1,19)[Water_treatment], col=CO2_treatment, ylim = c(0,30000), xlim = c(0, 30000))
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
     pch=c(1,19)[Water_treatment], col=CO2_treatment, ylim = c(0,30000), xlim = c(0, 10000))
points(CO2flux ~ C_yr, data = oct,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ C_yr, data = nov,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ C_yr, data = dec,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ C_yr, data = jan,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
points(CO2flux ~ C_yr, data = mar,  pch=c(1,19)[Water_treatment], col=CO2_treatment)
abline(0,1)



