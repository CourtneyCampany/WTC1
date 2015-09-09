##the relationship between root mass and total mass at the end of the experiment and from harvested pots
##then use the regression to predict root mass at a given tree mass
##for additive plots mus this as tree mass does not equal the final biomass
##predict mass first then change to carbon
library(RVAideMemoire)
library(visreg)
library(nortest)
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

##read and format potted plants
pretree <- read.csv("raw csv/Biomass potted plants Dec 2007.csv")
pre_saligna <- pretree[pretree$species == "E.saligna",]

pre_saligna_roots <- pre_saligna[complete.cases(pre_saligna),]

pre_root <- pre_saligna_roots[, c("chamber", "rootmass", "wabvground")]
names(pre_root)[3] <- "Mab" 
names(pre_root)[2] <- "root_mass" 


###rbind these data
rootshoot <- rbind(harvestmass[, c(1:2,6)], pre_root)
rootshoot <- rootshoot[order(rootshoot$chamber),]
rootshoot <- merge(rootshoot, chambersumm)
rootshoot$treatments <- with(rootshoot, as.factor(paste(CO2_treatment,Water_treatment, sep="-")))

###relationship between root and shoot 

rs_mod <- lm(log10(root_mass) ~ log10(Mab), data=rootshoot)
summary(rs_mod)
anova(rs_mod)
plotresid(rs_mod)
visreg(rs_mod)
rs_mod_coef <-data.frame(coef(rs_mod))

##ploting
with(rootshoot, plot(log10(root_mass)~log10(Mab), axes=FALSE, type='n'))
ablineclip(rs_mod, lwd=2, col="grey35",x1=min(log10(rootshoot$Mab)), x2=max(log10(rootshoot$Mab)))
with(rootshoot, points(log10(root_mass)~log10(Mab), pch=c(1,19)[Water_treatment],col=CO2_treatment,cex=2))
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)

#save coefficients and use them to predict with additive figures
write.csv(rs_mod_coef, "stats/rootshootmodel.csv", row.names = FALSE)



