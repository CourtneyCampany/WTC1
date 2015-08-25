source("functions and packages/load packages.R")
source("functions and packages/functions.R")


#read calculated data for each component----------------------------------------------------------------------------------------
#source("whole_tree_csv/calculated_read_data.R")


# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

#interpolated aboveground components and fluxes
fluxmass <- read.csv("whole_tree_csv/tree_C_flux.csv")

#roots
rootmass <- read.csv("calculated mass/root_mass.csv")
rootmass$Date <- as.Date("2009-03-16")


#CALCULATE treatment means for each tree component and cumulative chamber flux on harvest date----------------------------------

#merge data with treatment summary and add unique treatment factor
fluxmass <- merge(fluxmass, chambersumm, by = "chamber")
  fluxmass$treatment <- as.factor(paste(fluxmass$CO2_treatment, fluxmass$Water_treatment, sep="-"))

rootC <- rootmass[, c(1,7:8)]

#Date order for ease of viewing
chamberorder<-order(fluxmass$chamber, by=fluxmass$Date)
fluxmass <- fluxmass[chamberorder,]

#sum treeC 
fluxmass$treeC <- with(fluxmass, boleC+branchC+leafC+litterC)

#--------------------------------------------------------------------------------------------------------
######chamber flux ends on mar6, probably due to harvest, so use that dates cumsum to go with harvest data
flux_last <- subset(fluxmass, Date=="2009-03-06", select = c("chamber", "CO2cum"))

final_fluxmass <- fluxmass[fluxmass$Date == "2009-03-16", -(3:4)]
  final_fluxmass <- merge(final_fluxmass, flux_last)
  final_fluxmass$leafC_litterC <- with(final_fluxmass, leafC+litterC)
  final_fluxmass <- merge(final_fluxmass, rootC)
  
###write these chamber finals for TBCA
write.csv(final_fluxmass, "master_scripts/harvest_chamber.csv", row.names = FALSE) 


#summary tables
treefluxC_means <- ddply(final_fluxmass, .(treatment), summarize,
       boleC = round(mean(boleC), 2),                    
       branchC = round(mean(branchC), 2),
       leafC = round(mean(leafC_litterC), 2),
       treeC = round(mean(treeC), 2),
       CO2 = round(mean(CO2cum), 2),
       FrootC = round(mean(frootC_all),2),
       CrootC = round(mean(CrootC), 2))
       
treefluxC_se <- ddply(final_fluxmass, .(treatment), summarize,
        boleC_se = round(se(boleC), 3), 
        branchC_se = round(se(branchC), 3),
        leafC_se = round(se(leafC_litterC), 3),
        treeC_se = round(se(treeC), 3),
        CO2_se = round(se(CO2cum), 3),
        FrootC_se = round(se(frootC_all),3),
        CrootC_se = round(se(CrootC), 3))

treeC_stats <- merge(treefluxC_means, treefluxC_se)
 
write.csv(treeC_stats, "whole tree csv/harvest_trt_means.csv", row.names = FALSE)

#--------------------------------------------------------------------------------------------------------
#run lm on raw data from harvest date
lm_anova_func <- function(var1,var2,dfr){
  fit <- lm(dfr[[var1]] ~ dfr[[var2]])
  anova(fit)
}
lm_bole <- lm_anova_func("boleC", "treatment", final_fluxmass)


lm_leaf <- lm(leafC ~ treatment, data=final_fluxmass)
anova(lm_leaf)
summary(lm_leaf)

lm_bole <- lm(boleC ~ treatment, data=final_fluxmass)
anova(lm_bole)
summary(lm_bole)

lm_branch <- lm(branchC ~ treatment, data=final_fluxmass)
anova(lm_branch)
summary(lm_branch)

lm_tree<- lm(treeC ~ treatment, data=final_fluxmass)
anova(lm_tree)
summary(lm_tree)

lm_CO2<- lm(CO2cum ~ treatment, data=final_fluxmass)
anova(lm_CO2)
summary(lm_CO2)

##add roots
###make function work and then loop or plyr

#----------------------------------------------------------------------------------------------------------
#test with treeC and treament+DAte
temp <- subset(fluxmass, select = c("treeC", "chamber", "Date", "treatment"))

lmetreeC1 <- lm(treeC ~ treatment+Date+treatment:Date, data=temp) 
anova(lmetreeC1)
summary(lmetreeC1)
fluxmass$treeC_pred <- predict(lmetreeC1, temp, level=1)
fluxmass$treeC_pred2 <- predict(lmetreeC1, temp, level=0)

with(temp, plot(treatment, treeC, pch=1, col=chamber))


#STATS on treatments (aboveground components and flux)
#use plyr/functions/latex to create summary tables with treatment means for each component

#pull out p for each treatment combo, aic, coefs, maybe overall from anova?

nlme_func <- function(x){
  fit_bole <- lme(x ~ treatment, random = ~1|chamber, data=fluxmass)
  #data.frame(n=length(fluxmass$bole), r2=summary(fit)$r.squared, a=coef(fit)[[1]],
             #b=coef(fit)[[2]], p=anova(fit_bole)[1,"Pr(>F)"])
  a<- anova(fit_bole)
 
}
nlme_func(boleC)


nlme_models <- ddply(data, .(continent, year), model)

lmeboleC <- lme(boleC ~ treatment, random= ~1|chamber, data=fluxmass) 
anova(lmeboleC)


nlme_func1 <- function(component, dfr){ 
  model <- lme(component$dfr ~ treatment$dfr, random = ~1|chamber, data=dfr)
  anova(model)
}

nlme_func1(boleC, fluxmass)

above_stats <- dlply(fluxmass, .(boleC, leafC, litterC, branchC, treatment), nlme_func)



lmeboleC <- lme(boleC ~ treatment, random= ~1|chamber, data=fluxmass) 
anova(lmeboleC)

lmeleafC <- lme(leafC+litterC ~ treatment, random= ~1|chamber, data=fluxmass)
anova(lmeleafC)

lmebranchC <- lme(branchC ~ treatment, random= ~1|chamber, data=fluxmass)
anova(lmebranchC)


#___________________________________________________________________________________________________
#PLotting Flux vs Mass with treatment means

fluxmass_mean <- summaryBy(branchC+boleC+leafC+litterC+CO2cum ~ Date + treatment, data = fluxmass,
FUN = c(mean, se))



#------------------------------------------------------------------------------------------------- 
 #PLOT with 4 panels, one for each treatment
#as function or loop?

##try loopFactor 

factro<- rep(factor(letters[1:4]), each = 10) 
Size <- runif(40) * 100 

par(mfrow = c(2, 2)) 

for (i in unique(Factor)) { 
  hist(Size[Factor == i], main = i, 
       xlab = paste("n =",length(Size[Factor == i])), ylab = "") 
} 


par(mfrow = c(2, 2)) 

for (i in unique(fluxmass_mean$treatment)) {
  
    par(cex.axis = 0.9,mfrow=c(3,1), mar=c(5,5,1,1))
    #First Panel
    plot(CO2cum.mean ~ Date, 
         data = subset(fluxmass_mean, treatment == "elevated-wet"),
         type = 'n',  
         axes = FALSE, ann = FALSE)
    axis.Date(1, at = xAT, labels = T)
    axis(2)
    
    points(CO2cum.mean ~ Date,
           data = subset(fluxmass_mean, treatment == i),
           pch = 19, lwd=LWD,
           type = 'p',
           col = palette()[1])
    points(boleC.mean ~ Date, 
           data = subset(fluxmass_mean, treatment == i),
           pch = 18, lwd=LWD,
           type = 'l',
           col = palette()[2])
    points(branchC.mean ~ Date , 
           data = subset(fluxmass_mean, treatment == i),
           pch=17, lwd=LWD,
           type = 'l',
           col=palette()[3])
    
}


 plotmeans <- function(ch){
   
   #start plotting
   windows (7,10)
   par(cex.axis = 0.9,mfrow=c(3,1), 
       #omi = c(1,1,0.2,0.2),  # outer margin (inches)
       mar=c(5,5,1,1))  # margin around plots (they are tight together)
   
   #First Panel
   plot(CO2cum.mean ~ Date, 
        data = subset(fluxmass_mean, treatment == "elevated-wet"),
        type = 'n',  
        axes = FALSE, ann = FALSE)
   axis.Date(1, at = xAT, labels = T)
   axis(2)
   
   #with(subset(fluxmass_mean, treatment == "elevated-wet"), 
   #arrows(Date, CO2cum.mean, Date, CO2cum.mean+CO2cum.se, angle=90,  col = palette()[1],length=0.03))
   #with(subset(fluxmass_mean, treatment == "elevated-wet"),
   #arrows(Date, CO2cum.mean, Date, CO2cum.mean-CO2cum.se, angle=90,  col = palette()[1],length=0.03))
   
   points(CO2cum.mean ~ Date,
          data = subset(fluxmass_mean, treatment == "elevated-wet"),
          pch = 19, lwd=LWD,
          type = 'p',
          col = palette()[1])
   points(boleC.mean ~ Date, 
          data = subset(fluxmass_mean, treatment == "elevated-wet"),
          pch = 18, lwd=LWD,
          type = 'l',
          col = palette()[2])
   points(branchC.mean ~ Date , 
          data = subset(fluxmass_mean, treatment == "elevated-wet"),
          pch=17, lwd=LWD,
          type = 'l',
          col=palette()[3])
   points(leafC.mean+litterC.mean ~ Date , 
          data = subset(fluxmass_mean, treatment == "elevated-wet"),
          pch = 15, lwd=LWD,
          type = 'l',
          col=palette()[4])
   
   points(coarseroot_mass.mean ~ Date , 
          data = subset(rootmass_mean, treatment == "elevated-wet"),
          pch = 21, cex=CEX,
          type = 'p',
          bg=palette()[5])
   
   points(fineroot_mass.mean ~ Date , 
          data = subset(rootmass_mean, treatment == "elevated-wet"),
          pch = 21, cex=CEX,
          type = 'p',
          bg=palette()[6])
   
   box()
   
   leglab <- c("Chamber flux", "Bole", "Branch", "Leaf+Litter", "Coarse Root", "Fine Root")
   
   
   legend("topleft",leglab, lty = c(1, 1, 1, 1, NA, NA), pch = c(NA, NA, NA, NA, 21, 21),lwd=LWD,  
          col = c("black","blue","orange","forestgreen","black", "black"), 
          pt.bg = c("firebrick2", "mediumorchid1"), bty='n', pt.cex=CEX, title = expression(bold("WTC Carbon Flow")), inset = 0.03,)
   #Axis titles
   # mtext("Date", side = 1, outer = TRUE, line = -2.5, cex = 1.3)
   mtext("Carbon (g)", side = 2, outer=TRUE, line = -2.5, cex = 1.3)
   
   title(main= "Elevated CO2~~Wet", line=-1, outer=FALSE)
   
   
   
   #------------------------
   #It is also a one-liner with ggplot2 
   
   #dataset <- data.frame(Factor = rep(factor(letters[1:4]), each = 10), 
                         #Size = runif(40) * 100) 
   #library(ggplot2) 
   #ggplot(dataset, aes(x = Size)) + geom_histogram() + facet_wrap(~Factor) 
 
