##the relationship between root mass and total mass at the end of the experiment and from harvested pots
##then use the regression to predict root mass at a given tree mass
##for additive plots mus this as tree mass does not equal the final biomass
##predict mass first then change to carbon

# source("functions_and_packages/plot_objects.R")

# library(RVAideMemoire)
# library(visreg)
# library(nortest)
library(magicaxis)
library(plotrix)

chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

##read in final biomass
treemass <- read.csv("calculated_mass/harvest_mass_new.csv")
  treemass$Mab <- with(treemass, wf+wbr+stem_mass_dry)

rootmass <- read.csv("calculated_mass/root_mass_simple.csv")

harvestmass <- merge(rootmass, treemass[,c(1,6)])
harvestmass$location <- "wtc"

##read and format potted plants
pretree <- read.csv("raw csv/Biomass potted plants Dec 2007.csv")
pre_saligna <- pretree[pretree$species == "E.saligna",]

pre_saligna_roots <- pre_saligna[complete.cases(pre_saligna),]

pre_root <- pre_saligna_roots[, c("chamber", "rootmass", "wabvground")]
  names(pre_root)[3] <- "Mab" 
  names(pre_root)[2] <- "root_mass" 
pre_root$location <- "pot"

###rbind these data
rootshoot <- rbind(harvestmass[, c(1:2,6:7)], pre_root)
  rootshoot <- rootshoot[order(rootshoot$chamber),]
  rootshoot <- merge(rootshoot, chambersumm)
  rootshoot$treatments <- with(rootshoot, as.factor(paste(CO2_treatment,Water_treatment, sep="-")))

###relationship between root and shoot 

rs_mod <- lm(log10(root_mass) ~ log10(Mab), data=rootshoot)
# summary(rs_mod)
# anova(rs_mod)
# plotresid(rs_mod)
# visreg(rs_mod)
# rs_mod_coef <-data.frame(coef(rs_mod))

##ploting
# windows (7,7)
par(mar=c(5,5,1,1),las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0))
with(rootshoot, plot(log10(root_mass)~log10(Mab), axes=FALSE, type='n', ylab="Root Mass (g)", xlab="Shoot Mass (g)"))
ablineclip(rs_mod, lwd=2, col="black",x1=min(log10(rootshoot$Mab)), x2=max(log10(rootshoot$Mab)))

with(rootshoot[rootshoot$location == "pot",], points(log10(root_mass)~log10(Mab), pch=c(2,17)[Water_treatment],
     col=CO2_treatment,cex=1.5))
with(rootshoot[rootshoot$location == "wtc",], points(log10(root_mass)~log10(Mab), pch=c(1,19)[Water_treatment],
     col=CO2_treatment,cex=1.5))

magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)
legend("topleft", leglab3, pch=c(19,1,19,1, 17,19), col=c("blue", "blue", "red", "red", "black", "black"), inset = 0.01, bty='n')

# dev.copy2pdf(file= "master_scripts/paper_figs/rootshoot.pdf")
# dev.off()

#save coefficients and use them to predict with additive figures
# write.csv(rs_mod_coef, "stats/rootshootmodel.csv", row.names = FALSE)


###predict root at start of allometry (4/15-2008 for use with cumulative figure)---------------------------------------------


# ###C mass interpolated
# allomC <- read.csv("whole_tree_csv/tree_C_flux.csv")
# allomC$Date <- as.Date(allomC$Date)
# 
# allomfirst <- allomC[allomC$Date == "2008-04-15",c(1:2, 8:12)]
# #these starting values will need to be removed from harvest mass
# 
# ##ROOTS: will have to use R:S relationship to predict root from starting value R:S ratio
# rooteq <- read.csv("stats/rootshootmodel.csv")
# 
# ###to be correct do it with mass pred instead of C and double check results (rooteq based off mass not C)
# allomM <- read.csv("whole_tree_csv/tree_mass_Cflux.csv")
# allomM$Date <- as.Date(allomM$Date)
# massfirst <- allomM[allomM$Date == "2008-04-15",c(1:2, 8:12)]
# massfirst$aboveM <- with(massfirst, branch_start+leaf_start+litter_start+bole_start)
# massfirst$rootmass_pred <- with(massfirst, rooteq[2,1]*(log10(aboveM)) + rooteq[1,1])
# massfirst$rootmass_pred2 <- 10^(massfirst$rootmass_pred)
# massfirst$root_start <- massfirst$rootmass_pred2 * .5
# 
# ##add predicted root C mass back to allomc C dfr
# allomstart <- merge(allomfirst, massfirst[, c(1, 11)])
# 
# ###know we need to subtract start mass from harvest mass b4 we can compare with Cflux
# harvest <- read.csv("calculated_mass/chamber_carbon.csv") 
# harvest_corr <- merge(harvest, allomstart)
# 
# ##calculate root mass during 11 month period
# harvest_corr$root11 <- with(harvest_corr, rootC-root_start)
# 
# root_aprmar <- harvest_corr[, c(1, 3, 18:19)]
# 
# write.csv(root_aprmar, "calculated_mass/rootallometry.csv", row.names = FALSE)








