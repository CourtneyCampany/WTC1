#script calculates a parameter to estimate twig mass (branches <1cm) from harvest mass and harvest leaf mass
# a leaf mass model must first be contructed (leaf parameters from harvest)
## twigs are not measured in allometry through time so need to be fully modelled
##parameter =  twig_model (maybe two twig models = one for leaf mass and one for leaf #)

# chamber treatments
chambersumm <- read.csv("HFE chamber treatments.csv")

##write in harvest mass and get twig mass, leaf area, and leaf mass
twigM <- read.csv("HFE final harvest biomass by layer.csv")
twigM <- subset(twigM, select =c("chamber",  "layerno", "Wbrlt1", "LA", "Wleaf"))
names(twigM)[3:5] <- c("twigmass", "leafarea", "leafmass")
##tree totals(sum layers)
twigM <- aggregate(cbind(twigmass, leafarea, leafmass) ~ chamber, FUN = sum, data = twigM)

##read in  stem height
stemH <- read.csv("HFE Tree Height Fixed.csv")
stemH$Date <- as.character(stemH$Date)
stemH$Date <- as.Date(stemH$Date)
stemH <- subset(stemH, stemH$Date == "2009-03-16")
row.names(stemH)<-NULL

#merge stem  height and leaf harvest
harvest_mass <- merge(stemH, twigM, by = "chamber")
harvest_mass$areatoheight <- with(harvest_mass, leafarea/Height)
harvest_mass$masstoheight <- with(harvest_mass, leafmass/Height)
harvest_mass  <- subset(harvest_mass , chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])  

harvest_mass_trt <-  merge(harvest_mass, chambersumm, by = "chamber")

##leaf massmodels
#mass model with derived predictor (area/height) vs model with just area (height is not a good predictor..omitted)
areatoheight_model <- lm(leafmass ~ areatoheight, data = harvest_mass)
summary(areatoheight_model)

area_model <- lm(leafmass ~ leafarea, data = harvest_mass )
summary(area_model)
#leaf area model is better predictor than area/height 
#plot of leaf mass:area linear model 
with(harvest_mass_trt, plot(leafarea, leafmass, 
                        pch=c(1,19)[Water_treatment], col=CO2_treatment,
                        ylim = c(0, 10000), xlim = c(0, 60)))
abline(area_model)

# test for treatment effects #none
area_model_trt <- lm(leafmass ~ leafarea * CO2_treatment * Water_treatment, data = harvest_mass_trt)
summary(area_model_trt)
anova(area_model_trt)

#visualize best model/test for normality
require(effects)
plot(allEffects(area_model_trt))

shapiro.test(residuals(area_model_trt))

#extract coefficients
leafmasspred <- as.data.frame(coef(area_model))
names(leafmasspred)[1] <- "leaf_coef"
str(leafmasspred)
lapply(massbyarea_CO2, summary)
##now use these to predict leaf mass

#need twig to mass predictor/visualize best model/test for normality
twigmass_model <- lm(twigmass ~ masstoheight, data= harvest_mass_trt)
summary(twigmass_model)

twigmass_model_trt <- lm(twigmass ~ masstoheight * CO2_treatment * Water_treatment, data= harvest_mass_trt)
summary(twigmass_model_trt)
anova(twigmass_model_trt)

twigmasspred <- as.data.frame(coef(twigmass_model))
names(twigmasspred)[1] <- "twig_coef"

require(effects)
plot(allEffects(twigmass_model_trt))

shapiro.test(residuals(twigmass_model_trt))
##use to predict twig mass after leaf mass has been predicted

##read in leaf and height data
treeH <- read.csv("HFE Tree Height Fixed.csv")
treeH  <- subset(treeH , chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])  
treeH$Date <- as.character(treeH$Date)
treeH$Date <- as.Date(treeH$Date)

leafA_est <- read.csv("HFE LA estimates alldates.csv")
leafA_est$Date <- as.character(leafA_est$Date)
leafA_est$Date <- as.Date(leafA_est$Date)
leafA_est <- subset(leafA_est, select = c("chamber","Date",	"LAestlin"))

twig_est <- merge(treeH, leafA_est, by = c("Date", "chamber"), all=TRUE)
twig_est <- twig_est[complete.cases(twig_est), ]
row.names(twig_est)<-NULL
twig_est <- merge(twig_est, chambersumm, by = "chamber")

#calculate leaf mass from 
#must determine leaf mass by co2 trt, use leafmasspred

twig_est$leaf_intercept <-  leafmasspred$leaf_coef[1]
twig_est$leaf_slope <- leafmasspred$leaf_coef[2]
twig_est$LAestlin <- twig_est$LAestlin
twig_est$leafmass <- with(twig_est, (leaf_slope*LAestlin)+leaf_intercept)
#estimate twig mass 
twig_est$leafmasstoheight <- twig_est$leafmass / twig_est$Height 
twig_est$twig_intercept <- twigmasspred$twig_coef[1]
twig_est$twig_slope <- twigmasspred$twig_coef[2]
twig_est$twig_mass <- with(twig_est, ((twig_slope*leafmasstoheight) + twig_intercept))

write.csv(twig_est, file = "HFE Leaf and Twig mass model_cec.csv")

###barton had more....
##model with leaf number for early dates ???????
leaf_final <- read.csv("HFE final harvest leaf samples.csv")
leaf_final <- aggregate(leafcount ~ chamber, FUN = sum, data = leaf_final)
leaf_final  <- subset(leaf_final , chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])  
leaf_final <- merge(leaf_final, harvest_mass, by = "chamber")
leaf_final$leaftoheight <- leaf_final$leafcount / leaf_final$Height
leaf_final$areabycount<- leaf_final$leafarea / leaf_final$leafcount
leaf_final <- merge(leaf_final, chambersumm, by = "chamber")

#leaf count models
leafcount_model <- lm(leafmass ~ leafcount, data = leaf_final )
summary(leafcount_model)

leafheight_model <- lm(leafmass ~ leaftoheight, data = leaf_final )
summary(leafheight_model)

areabycount_model <- lm(leafmass ~ areabycount, data = leaf_final )
summary(areabycount_model)


#leaf area/count model is better predictor than area/height 
#plot of leaf mass:area linear model 
with(leaf_final, plot(areabycount, leafmass, 
                            pch=c(1,19)[Water_treatment], col=CO2_treatment,
                            ylim = c(0, 10000), xlim = c(0, .2)))
abline(areabycount_model)

# test for treatment effects #none
areabycount_model_trt <- lm(leafmass ~ areabycount * CO2_treatment * Water_treatment, data = leaf_final)
summary(areabycount_model_trt)
anova(areabycount_model_trt)

#visualize best model/test for normality
require(effects)
plot(allEffects(areabycount_model_trt))

shapiro.test(residuals(areabycount_model_trt))

#extract coefficients
countmasspred <- as.data.frame(coef(areabycount_model))
names(countmasspred)[1] <- "count_coef"
str(leafmasspred)
lapply(massbyarea_CO2, summary)

#need to merge leaf area estimates, leaf counts, and leaf areacount model
##only three dates, can use if want?????
leaf_alldates <- read.csv("HFE LA estimates alldates.csv")
leaf_alldates <- subset(leaf_alldates, select = c("chamber",  "Date",		"LAestlin"))
leaf_counts <- read.csv("HFE leaf count data.csv")
leaf_counts <- aggregate(noexpleaves ~ chamber + Date, FUN = sum, data= leaf_counts)

leaf_alldates <- merge(leaf_alldates, leaf_counts, by = c("Date", "chamber"), all=TRUE)
leaf_alldates <- leaf_alldates[complete.cases(leaf_alldates), ]

leaf_alldates$areacount <- leaf_alldates$LAestlin/leaf_alldates$noexpleaves
leaf_alldates$intercept <- countmasspred$count_coef[1]
leaf_alldates$slope <- countmasspred$count_coef[2]
leaf_alldates$leafmass <- with(leaf_alldates, ((slope * areacount) + intercept))

write.csv(leaf_alldates, file = "HFE Leaf area by count model_cec.csv")
