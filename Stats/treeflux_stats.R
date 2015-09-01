test <- read.csv("master_scripts/harvest_trt_means.csv")


modeltrt<- lm(treeC ~ CO2_treatment*Water_treatment, data = dayfluxtomass)
anova(modeltrt)
summary()

modeltrt2<- lm(treeC ~ CO2_treatment, data = dayfluxtomass)
anova(modeltrt2)
summary(modeltrt2)

modeltrt3<- lm(treeC ~ Water_treatment, data = dayfluxtomass)
anova(modeltrt3)
summary(modeltrt3)

(mean(dayfluxtomass[dayfluxtomass$CO2_treatment=="ambient", "treeC"]) - mean(dayfluxtomass[dayfluxtomass$CO2_treatment=="elevated", "treeC"]))/mean(dayfluxtomass[dayfluxtomass$CO2_treatment=="ambient", "treeC"])


modeltrt4<- lm(Cflux ~ CO2_treatment*Water_treatment, data = dayfluxtomass)
anova(modeltrt4)

modeltrt5<- lm(Cflux ~ CO2_treatment, data = dayfluxtomass)
anova(modeltrt5)

modeltrt6<- lm(Cflux ~ Water_treatment, data = dayfluxtomass)
anova(modeltrt6)

(mean(dayfluxtomass[dayfluxtomass$CO2_treatment=="ambient", "Cflux"]) - mean(dayfluxtomass[dayfluxtomass$CO2_treatment=="elevated", "Cflux"]))/mean(dayfluxtomass[dayfluxtomass$CO2_treatment=="ambient", "Cflux"])



# library(nlme)
# 
# modeltrt3 <- lme(treeC ~ Cflux*CO2_treatment*Water_treatment ,random=~1|chamber, data=dayfluxtomass)
# summary(modeltrt3)
# 
# modeltrt4 <- lme(treeC ~ CO2_treatment*Water_treatment ,random=~1|chamber, data=dayfluxtomass)
# summary(modeltrt4)
# 
# modeltrt5 <- lme(Cflux ~ CO2_treatment*Water_treatment ,random=~1|chamber, data=dayfluxtomass)
# summary(modeltrt5)
