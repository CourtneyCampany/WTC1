# this script combines the two two sources of soil respiration into one dataframe
# calculates an average respiration rate based on treatment

chambersumm <- read.csv("HFE Chamber treatments.csv")

#calcualte mean of soil respiraton from collars a/b
collars<-read.csv("HFE-I Soil Respiration Collars.csv")
collars$Date <- as.character(collars$Date)
collars$Date <- as.Date(collars$Date)
collars <- collars[,1:6]
names(collars)
collars_mean <- aggregate(cbind(SoilCO2Efflux, Tsoil, VWC) ~ Date + chamber, data= collars, FUN = mean)

#calcualte mean of soil respiraton from collars a/b
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






