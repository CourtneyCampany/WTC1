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
root_est_no6 <- subset(root_est, root_est$chamber != "ch06")
root_est_no6 <- merge(root_est_no6, chambersumm)

#Fine Root model
Froot_model <- lm(Frootmass ~ Lareabyheight , data = root_est_no6 )
summary(Froot_model)

# extract Coefficients
frootmasspred <- as.data.frame(coef(Froot_model))
names(frootmasspred)[1] <- "Froot_coef"

#Coarse Root model
Croot_model <- lm(Crootmass ~  RSratio, data = root_est )
summary(Croot_model)

# extract Coefficients
Crootmasspred <- as.data.frame(coef(Croot_model))
names(Crootmasspred)[1] <- "Croot_coef"

#need to extract root-shoot ratio (cannot derive)
Crootratio <- subset(root_est, select = c("chamber", "RSratio"))

#estimate fineroot mass
rootmass_model <- merge(leafA_est, Crootratio, by = "chamber")
rootmass_model$froot_intercept <- frootmasspred$Froot_coef[1]
rootmass_model$froot_slope <- frootmasspred$Froot_coef[2]
rootmass_model$finerootbiomass <- with(rootmass_model, ((froot_slope * LAestlin) + froot_intercept))
