#this script estimates a model to calculate root mass from final harvest
#estiamtes roots <2cm and >2cm diameter seperately
require(effects)
library(outliers)

# chamber treatments
chambersumm <- read.csv("HFE chamber treatments.csv")

##write in harvest mass and get twig mass, leaf area, and leaf mass
aboveM <- read.csv("HFE final harvest biomass by layer.csv")
aboveM <- subset(aboveM, select =c("chamber",  "layerno", "Wbrlt1", "LA", "Wleaf", "Wbrgt1", "Ws", "SLA", "BA"))
names(aboveM)[3:9] <- c("twigmass", "leafarea", "leafmass", "branchmass", "stemmass", "leafarea_spec", "basalarea")
aboveM$abovemass <- with(aboveM, twigmass+leafmass+branchmass+stemmass)
##tree totals(sum layers)
aboveM <- aggregate(cbind(twigmass, leafarea, leafmass, branchmass, stemmass, leafarea_spec, basalarea, abovemass) ~ chamber, FUN = sum, data = aboveM)

##read in  stem height
stemH <- read.csv("HFE Tree Height Fixed.csv")
stemH$Date <- as.character(stemH$Date)
stemH$Date <- as.Date(stemH$Date)
stemH <- subset(stemH, stemH$Date == "2009-03-16")
row.names(stemH)<-NULL

##read in harvest roots
rootM<- read.csv("HFE Fine root cores final harvest.csv")
rootM$wr510 <- as.numeric(rootM$wr510)
#create fine and coarse root vectors
rootM$Frootmass <- with(rootM, veryfine + wrlt2)
rootM$Crootmass <- with(rootM, wr25  + wr510	+ wrgt10)

# convert core diameter to chamber(which is roots/tree)
# try with sum of all cores across depths (total area of 5 cores)
#instead of mean per core ####probably wrong (scale up 5+ 10cm2 diameter cores to 325cm chamber 
core <- (((10/2)^2)*pi)*5
hfe <- (((325/2)^2)*pi)
scaleup <- ((hfe-core)/core)

rootM_total <- aggregate(cbind(Frootmass, Crootmass) ~ chamber + upper, FUN = sum, data = rootM)
rootM_total$Frootmass <- rootM_total$Frootmass * scaleup
rootM_total$Crootmass <- rootM_total$Crootmass * scaleup
rootM_total <- aggregate(cbind(Frootmass, Crootmass) ~ chamber, FUN = sum, data = rootM_total)
rootM_total$rootmass_all <- rootM_total$Crootmass + rootM_total$Frootmass
  
root_est <- merge(rootM_total, aboveM, by = "chamber")
root_est <- merge(root_est, stemH, by = "chamber")
root_est  <- subset(root_est , chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"]) 
root_est$Smassbyheight <-  root_est$stemmass / root_est$Height
root_est$Lmassbyheight <- root_est$leafmass / root_est$Height
root_est$Lareabyheight <- root_est$leafarea / root_est$Height
root_est$abovemassbyheight <- root_est$abovemass / root_est$Height
root_est$abovemassbyarea <- root_est$abovemass / root_est$basalarea
root_est$rootratio <- root_est$Crootmass / root_est$Frootmass
root_est <- merge(root_est, chambersumm)

###tree # 6 is really bad, remove from model estimates for now
chisq.out.test(root_est$abovemass, variance=var(root_est$CO2_treatment), opposite = FALSE)
chisq.out.test(root_est$abovemass, variance=var(root_est$Water_treatment), opposite = FALSE)
boxplot(abovemass ~ Water_treatment, data = root_est)

boxplot(abovemass ~ Water_treatment, data = root_est)
root_est_no6 <- subset(root_est, root_est$chamber != "ch06")
root_est_no6 <- merge(root_est_no6, chambersumm)

root_est_no9 <- subset(root_est, root_est$chamber != "ch09")
root_est_no9 <- merge(root_est_no6, chambersumm)

root_est_no3 <- subset(root_est, root_est$chamber != "ch03")
root_est_no3 <- merge(root_est_no6, chambersumm)

#model testing for all roots
allroot_model <- lm(rootmass_all ~ abovemassbyheight, data = root_est)
summary(allroot_model)

#model testing for fine roots
Froot_model <- lm(Frootmass ~ Lareabyheight , data = root_est_no9 )
summary(Froot_model)
plot(Froot_model, which = c(3, 2))


##use leafarea/height to determine fine root mass
##test trts, normality, extract COEFS
palette(c( "black", "red"))
with(root_est_no6, plot(Lareabyheight, Frootmass, 
                            pch=c(1,19)[Water_treatment], col=CO2_treatment,
                            ylim = c(0, 15000), xlim = c(0, 10)))
abline(Froot_model)

Froot_model_trt <- lm(leafmass ~ Lareabyheight * CO2_treatment * Water_treatment, data = root_est_no6)
summary(Froot_model_trt)
anova(Froot_model_trt)

plot(allEffects(Froot_model_trt))
shapiro.test(residuals(Froot_model_trt))

frootmasspred <- as.data.frame(coef(Froot_model))
names(frootmasspred)[1] <- "Froot_coef"

#model testing for coarse roots
Croot_model <- lm(Crootmass ~  rootratio, data = root_est )
summary(Croot_model)

with(root_est_no6, plot(rootratio, Crootmass, 
                        pch=c(1,19)[Water_treatment], col=CO2_treatment,
                        ylim = c(0, 100), xlim = c(0, 5)))
abline(Croot_model)


Croot_model_trt <- lm(Crootmass ~  rootratio * CO2_treatment * Water_treatment, data = root_est_no6)
summary(Croot_model_trt)
anova(Croot_model_trt)

plot(allEffects(Croot_model_trt))
shapiro.test(residuals(Croot_model_trt))

Crootmasspred <- as.data.frame(coef(Croot_model))
names(frootmasspred)[1] <- "Croot_coef"

#need to extract root ratio (cannot derive)
rootratio <- subset(root_est, select = c("chamber", "rootratio"))

#estimate root mass
leaf_area <- read.csv("HFE LA estimates alldates.csv")
leaf_area <- subset(leaf_area, select = c("chamber",  "Date",  	"LAestlin"))

rootmass_model <- merge(leaf_area, rootratio, by = "chamber")
rootmass_model$Date <- as.character(rootmass_model$Date)
rootmass_model$Date <- as.Date(rootmass_model$Date)

treeH <- read.csv("HFE Tree Height Fixed.csv")
treeH$Date <- as.character(treeH$Date)
treeH$Date <- as.Date(treeH$Date)
treeH  <- subset(treeH , chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"]) 

rootmass_model <- merge(rootmass_model, treeH, by = c("Date", "chamber"), all=TRUE)
rootmass_model <- rootmass_model[complete.cases(rootmass_model), ]
rootmass_model$leafareabyheight <- rootmass_model$LAestlin / rootmass_model$Height
#trouble with extracting slope and intercept from frootmass pred data frame = just copied numbers below
rootmass_model$finerootbiomass <- with(rootmass_model, ((909.3295 * leafareabyheight) + 3312.1353))
rootmass_model$coarserootmass <-  rootmass_model$finerootbiomass * rootmass_model$rootratio

rootmass_est <- subset(rootmass_model, select = c("Date",  "chamber",	"finerootbiomass",	"coarserootmass"))
row.names(rootmass_est)<-NULL
write.csv(rootmass_est, file = "HFE root mass model_cec.csv")
