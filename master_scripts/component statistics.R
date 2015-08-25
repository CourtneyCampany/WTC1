source("HFE chamber read data.R")
source("HFE Flux to C-Mass_interpolated")

require(effects)
library(outliers)

#STATS

#stem density
#TEST = are densities different (bark and wood)? YES
t.test(Tree_density_mean$bark_density,  Tree_density_mean$wood_density, var.equal = TRUE)
#TEST = are densities different across trts? NO
anova(lm(bark_density ~ CO2_treatment*Water_treatment, data=Tree_density_mean_trt))
anova(lm(wood_density ~ CO2_treatment*Water_treatment, data=Tree_density_mean_trt))
#TEST = are individual trees different from chamber mean? YES
 meanbarkD <- mean(Tree_density_mean$bark_density_cm)
meanwoodD <- mean(Tree_density_mean$wood_density_cm)
wilcox.test(Tree_density_mean$bark_density_cm, mu = meanbarkD)
wilcox.test(Tree_density_mean$wood_density_cm, mu = meanwoodD)


#leaf mass models (leafarea was best model, others omitted from this script)
# TEST =  treatment effects within model. NO
leafarea_model_trt <- lm(leafmass ~ leafarea * CO2_treatment * Water_treatment, data = leafM)
summary(leafarea_model_trt)
anova(leafarea_model_trt)

#twig mass model
# TEST =  treatment effects within model. NO
twigmass_model_trt <- lm(twigmass ~ leafMtoheight * CO2_treatment * Water_treatment, data= twigleaf_harv)
summary(twigmass_model_trt)
anova(twigmass_model_trt)


#Root modelling
#tree #6 looks really bad, 
#tree #3 is statistically an outlier
#linear root models are total shit without removing, remove 6 for now
chisq.out.test(root_est$abovemass, variance=var(root_est$CO2_treatment), opposite = FALSE)
chisq.out.test(root_est$abovemass, variance=var(root_est$Water_treatment), opposite = FALSE)
boxplot(abovemass ~ Water_treatment, data = root_est)

#Fine Root model
#TEST = treatment effects. NO
Froot_model_trt <- lm(leafmass ~ Lareabyheight * CO2_treatment * Water_treatment, data = root_est_no6)
summary(Froot_model_trt)
anova(Froot_model_trt)

#Coarse Root model
#TEST = treatment effects. YES
Croot_model_trt <- lm(Crootmass ~  RSratio * CO2_treatment * Water_treatment, data = root_est)
summary(Croot_model_trt)
anova(Croot_model_trt)

#TEST, may need to do seperate equations by layer, would have have to determine layers back in time???????
#neither the bumber of 2order branches or twig dry mass vary by canopy layer
br2order_test <- lm(br2order ~ layerno, data = twig_est)
summary(br2order_test)
boxplot(br2order ~ layerno, data = twig_est)
tukey_br2order <- glht(br2order_test, linfct = mcp(layerno = "Tukey"))
summary(tukey_br2order)

brtwig_test <- lm(DWbrlt1 ~ layerno, data = twig_est)
summary(brtwig_test)
boxplot(DWbrlt1 ~ layerno, data = twig_est)
tukey_twig <- glht(brtwig_test, linfct = mcp(layerno = "Tukey"))
summary(tukey_twig)

#GRAPHS

#STATS_Graphs

#leaf mass models visualize model/test for normality/plot
plot(allEffects(leafarea_model_trt))
shapiro.test(residuals(leafarea_model_trt))
with(twigleaf_harv, plot(leafarea, leafmass, 
                         pch=c(1,19)[Water_treatment], col=CO2_treatment,
                         ylim = c(0, 10000), xlim = c(0, 60)))
abline(leafmass_model)

#twig mass model visualize model/test for normality
plot(allEffects(twigmass_model_trt))
shapiro.test(residuals(twigmass_model_trt))


#Fine Root model visualize model/normality/plot
plot(allEffects(Froot_model_trt))
shapiro.test(residuals(Froot_model_trt))

with(root_est_no6, plot(Lareabyheight, Frootmass, 
                        pch=c(1,19)[Water_treatment], col=CO2_treatment,
                        ylim = c(0, 15000), xlim = c(0, 10)))
abline(Froot_model)

#Coarse Root model visualize model/normality/plot
plot(allEffects(Croot_model_trt))
shapiro.test(residuals(Croot_model_trt))
with(root_est, plot(RSratio, Crootmass, 
                    pch=c(1,19)[Water_treatment], col=CO2_treatment,
                    ylim = c(0,30000), xlim = c(0, 2)))
abline(Croot_model)

#treatment series for plotting
elevdry <- subset(tree_stats, CO2_treatment == "elevated" & Water_treatment == "dry")
elevwet <- subset(tree_stats, CO2_treatment == "elevated" & Water_treatment == "wet")
ambdry <- subset(tree_stats, CO2_treatment == "ambient" & Water_treatment == "dry")
ambwet <- subset(tree_stats, CO2_treatment == "ambient" & Water_treatment == "wet")
#graph
plot(treeC ~ date, data = elevdry, pch = 19, col = "darkolivegreen4", ylim = c(0,40000))
points(treeC ~ date, data = elevwet, pch = 21, bg = "darkgoldenrod3")
points(treeC ~ date, data = ambdry , pch = 21, bg = "red")
points(treeC ~ date, data = ambwet, pch = 21, bg = "blue")

#DATa grapsh

#leaf area over time, the linear interpolated values should track this
meanLA <- aggregate(cbind(LAestlin, LAlittercumlin) ~ Date, data = leafA_est, FUN = mean)
with (meanLA, plot(Date, LAestlin))  
with (meanLA, plot (Date, LAlittercumlin))

#add flux to WUE

#Final Graph

pdf("Flux to Mass.pdf", onefile=TRUE)

for(i in 1:12){
  dataset<-chamflux_cum[[i]]
  with(dataset, plot(Date, CO2cum,
                     main=names(chamberflux_time)[i],
                     pch=c(1,19)[Water_treatment], 
                     col=CO2_treatment,
                     ylab=expression(CO2flux~(g ~ C))
      points()               
  ))
}

dev.off()


# TIP: sequence of dates
# seq.Date(from=as.Date("2008-4-15"), length=12, by="month")
