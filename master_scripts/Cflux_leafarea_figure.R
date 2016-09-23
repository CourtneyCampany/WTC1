source("functions_and_packages/plot_objects.R")
source("functions_and_packages/functions.R")
library(plotrix)
library(doBy)

## treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])
  
##Read in total chamber flux and leaf area
treeC <- read.csv("calculated_mass/chamber_carbon.csv")

leafarea <- read.csv("raw csv/HFE LA estimates alldates.csv")
  leafarea$Date <- as.Date(leafarea$Date)
  
###plot needs to have mean leaf area over all dates on yaxis  
la_agg <- summaryBy(LAestlin ~ chamber, data=leafarea, FUN=mean, keep.names = TRUE)
la_agg <- merge(la_agg, chambersumm)

###merge leaf area with chamber flux
la_flux <- merge(la_agg,treeC[, c(1:2)] )  


#   library(nortest) 
#   ad.test(leafflux$LAestlin)
leaffluxlmodel <- lm(Cflux~LAestlin, data = la_flux)
# getP(leaffluxlmodel)
# # write.csv(coef(leaffluxlmodel), "Stats/leaffluxcoefs.csv", row.names = FALSE)
# 
# ###Test for difference in slopes, regression with eCO2, data points dont overlap so using CI is not appropirate
# leaffluxlmodel2 <- aov(Cflux~LAestlin*CO2_treatment, data=la_flux)
# leaffluxlmodel3 <- aov(Cflux~LAestlin+CO2_treatment, data=la_flux) ###no effect of co2 (intercepts not different)
# summary(leaffluxlmodel2)
# summary(leaffluxlmodel3)
# 
# anova(leaffluxlmodel2, leaffluxlmodel3) ###slope not significantly different between co2 treatments
# 
# ##two lines for e/aco2
# leaffluxmod_amb <- lm(Cflux~LAestlin, data = la_flux[la_flux$CO2_treatment == "ambient",])
# leaffluxmod_elev <- lm(Cflux~LAestlin, data = la_flux[la_flux$CO2_treatment == "elevated",])
# summary(leaffluxmod_amb)
# summary(leaffluxmod_elev)
# 
# 
# s1 <- summary(leaffluxmod_amb)$coefficients
# s2 <- summary(leaffluxmod_elev)$coefficients
# 
# db <- (s2[2,1]-s1[2,1])
# sd <- sqrt(s2[2,2]^2+s1[2,2]^2)
# df <- (leaffluxmod_amb$df.residual+leaffluxmod_elev$df.residual)
# td <- db/sd
# 2*pt(-abs(td), df)
# ###slopes not different


##plot LA with total flux-----------------------------------------------------------------------------
palette (c("blue", "red"))

# windows (7,7)

par(mar=c(5,6,1,1),las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0))

plot(1,type='n', ylab = "",xlab=meanlalab,ylim = c(5000, 30000), xlim = c(0, 50))

title(ylab=treefluxlab2, mgp=c(4,1,0))
legend("topleft", leglab2, pch=c(19,1, 19, 1), col=c("blue", "blue", "red", "red"), inset = 0.01, bty='n')
ablineclip(leaffluxlmodel, x1=min(la_flux$LAestlin),x2=max(la_flux$LAestlin),lwd=2)
points(Cflux ~ LAestlin, data = la_flux,pch=c(1,19)[Water_treatment],col=CO2_treatment, cex=1.5)

# dev.copy2pdf(file= "master_scripts/paper_figs/flux_leafarea.pdf")
# dev.off()





  