###stats correlations between the variables plotted in flux, leafarea, tbca
library(RVAideMemoire)
library(visreg)
library(nortest)

## treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])
  
treeC <- read.csv("calculated_mass/chamber_carbon.csv")
  
leafarea <- read.csv("raw csv/HFE LA estimates alldates.csv")
  leafarea$Date <- as.Date(leafarea$Date)
  
###analysis needs to have mean leaf area over all dates on yaxis  
la_agg <- summaryBy(LAestlin ~ chamber, data=leafarea, FUN=mean)
  la_agg <- merge(la_agg, chambersumm)
  
###merge leaf area with chamber flux
la_flux <- merge(la_agg,treeC[, c(1:2)] )    

###STATS on mean LA versus Fc,t
ad.test(la_flux$LAestlin.mean)

leaffluxlmodel <- lm(Cflux~LAestlin.mean, data = la_flux)
  plotresid(leaffluxlmodel)
  summary(leaffluxlmodel)
  anova(leaffluxlmodel)
  visreg(leaffluxlmodel)

# leaffluxlmodel2 <- lm(Cflux~LAestlin.mean*CO2_treatment*Water_treatment, data = la_flux)
#   anova(leaffluxlmodel2)

  

