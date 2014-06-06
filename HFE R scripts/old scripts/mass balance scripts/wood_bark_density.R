setwd("C:/Users/90919620/Google Drive/HFE Database")

##test if density of bark is different from density in wood
##determine different volumes of bark and wood for each tree

#read chamber treatments
chambersumm <- read.csv("HFE chamber treatments.csv")

#read density cookie data
#determine mass and densty per cm for each layer/stem segmnt
density <- read.csv("HFE wood density cookies.csv")
density <- subset(density, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
density <- subset(density,select = c("chamber", "layerno", "Stemsegmnt",	"doverbark",	"dunderbark", "freshvolume",  "wbark",	"wwood"))

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

#chamber means
###switched to using standarized densities (by cm)
Tree_density_mean <- aggregate(cbind(bark_density_cm , wood_density_cm) ~ chamber, data=density, FUN=mean)

#densities are different (bark and wood)
t.test(Tree_density_mean$bark_density_cm,  Tree_density_mean$wood_density_cm, var.equal = TRUE)

#are densities different across trts/or mean
Tree_density_mean_trt <- merge(Tree_density_mean, chambersumm, by = "chamber")
###not diff for bark or wood across drought
barkD_dry <-  subset(Tree_density_mean_trt, Water_treatment == "dry")
barkD_wet <-  subset(Tree_density_mean_trt, Water_treatment == "wet")
t.test(barkD_dry$bark_density_cm, barkD_wet$bark_density_cm, var.equal = TRUE)
t.test(barkD_dry$wood_density_cm,barkD_wet$wood_density_cm, var.equal = TRUE)
###not diff for bark or wood across co2
barkD_ele <-  subset(Tree_density_mean_trt, CO2_treatment == "elevated")
barkD_amb <-  subset(Tree_density_mean_trt, CO2_treatment == "ambient")
t.test(barkD_ele$bark_density_cm, barkD_amb$bark_density_cm, var.equal = TRUE)
t.test(barkD_ele$wood_density_cm,barkD_amb$wood_density_cm, var.equal = TRUE)
###chambers are different from mean density
meanbarkD <- mean(Tree_density_mean$bark_density_cm)
wilcox.test(Tree_density_mean$bark_density_cm, mu = meanbarkD)
meanwoodD <- mean(Tree_density_mean$wood_density_cm)
wilcox.test(Tree_density_mean$wood_density_cm, mu = meanwoodD)

#general weighted mean of density (bark and wood) across experiement
##can only use if density not different across trees
density_sp <- split(density, density$chamber)
length (density_sp)
woodD_wm <- as.data.frame(lapply(density_sp, function(x) weighted.mean(x$wood_density_cm, w = x$mass_wood_cm)))
woodD_wm <- t(woodD_wm)
woodD_wm <- as.data.frame(woodD_wm[1:12,])
woodD_wm <- data.frame(as.character(rownames(woodD_wm)),woodD_wm)
colnames(woodD_wm)[1]="chamber"
names(woodD_wm)[2] <- "wooddensity_wm"
row.names(woodD_wm)<-NULL
woodD_wm <- merge(chambersumm, woodD_wm, by = "chamber")
woodD_wm <- subset(woodD_wm, select = c("chamber", "wooddensity_wm"))
names(woodD_wm) <- c("chamber", "wood_wm")

barkD_wm <- as.data.frame(lapply(density_sp, function(x) weighted.mean(x$bark_density_cm, w = x$mass_bark_cm)))
barkD_wm <- t(barkD_wm)
barkD_wm <- as.data.frame(barkD_wm[1:12,])
barkD_wm <- data.frame(as.character(rownames(barkD_wm)),barkD_wm)
colnames(barkD_wm)[1]="chamber"
names(barkD_wm)[2] <- "barkdensity_wm"
row.names(barkD_wm)<-NULL
barkD_wm <- merge(chambersumm, barkD_wm, by = "chamber")

BWratio <- as.data.frame(lapply(density_sp, function(x) weighted.mean(x$Bark_Wood, w = x$freshvolume)))
BWratio <- t(BWratio)
BWratio <- as.data.frame(BWratio[1:12,])
BWratio <- data.frame(as.character(rownames(BWratio)), BWratio)
colnames(BWratio)[1] = "chamber"
names(BWratio)[2] <- "bark:wood diameter"
row.names(BWratio) <- NULL
BWratio <- merge(chambersumm, BWratio, by = "chamber")

#dataframe with weighted avergages (from layers) of bark and wood density for each chamber, and diameter ratios
wooddensity_wm <- merge(barkD_wm, woodD_wm, by = "chamber")
wooddensity_wm <- merge(wooddensity_wm, BWratio,  by = c("chamber", "CO2_treatment",  "Water_treatment",	"inside_or_outside_WTC"))

write.csv(wooddensity_wm, file= "HFE bark and wood density_cec.csv")




