### simplified root respiration model from M'Bou (plant and soil)
##best model fit from their Eucalpytus cuttings (5 months)

# Resp = a * (dM Root/dt) + c * Nroot + e

# a = growth respiration coefficient (gC g Dm-1)
a <-  0.106

# c = nitrogen related maintenance coefficient
#c parameter (above) vary based on a nitrogen fertilzatioj to cuttings 
# N fert treatments were 8, 24, 48g per plant; 24 is standard, im using low as it is closet to natural
c8 <- 1.33

# e = intercept
e <- .0386

#dm root = new growth rate of roots (g Dm d-1)
# N root = root N content (gN)

chambersumm <- read.csv("HFE chamber treatments.csv")

#minirhizotron data
rootrhizo <- read.csv("Rootlengthmass.csv")
rootrhizo$DATE <- as.character (rootrhizo$DATE)
rootrhizo$DATE <- as.Date (rootrhizo$DATE)
str(rootrhizo)

#start with growth rate/respiration of fine roots (<2mm)
froot <- subset(rootrhizo, select

#HFE root harvest
rootM<- read.csv("HFE Fine root cores final harvest.csv")
rootM$wr510 <- as.numeric(rootM$wr510)

 
                
                
                
                
#use model from mass balance for root growth rates?????
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
                





