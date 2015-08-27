#read data for cumulative C flux and harvest mass
dayfluxtomass <- read.csv("whole_tree_csv/harvest mass and carbon flux_newroots.csv")   


#CO2 flux to harvest mass scatter plot

#linear model to show fit on graph
diurnalmodel <- lm(treeC ~ Cflux, data = dayfluxtomass)
summary(diurnalmodel)

# library(nortest) 
# ad.test(dayfluxtomass$treeC)
# ad.test(dayfluxtomass$Cflux)
# shapiro.test(dayfluxtomass$treeC)

modeltrt<- lm(treeC ~ Cflux*CO2_treatment*Water_treatment, data = dayfluxtomass)
anova(modeltrt)

getP <- function(x)anova(x)[[5]][1]
getP(diurnalmodel)

##plot with total flux
leglab2 = c(expression(paste(aCO[2], " " ,"wet")), expression(paste(aCO[2], " " ,"dry")),
            expression(paste(eCO[2], " " ,"wet")), expression(paste(eCO[2], " " ,"dry")))

palette (c("red", "blue"))

windows (8,6)
par(mar=c(5,6,1,1),las=1, cex.axis=1, cex.lab=1.25, mgp=c(3.5,1,0))

plot(1,type='n', ylab = "",
     xlab=expression(Chamber~Flux~(g ~ C)),
     xlim = c(0, 30000),
     ylim = c(0, 30000))

title(ylab=expression(Whole~Tree~Carbon~(g ~ C)), mgp=c(4,1,0))

legend("topleft", leglab2, pch=c(1,1, 19, 19), col=c("blue", "red", "blue", "red"), inset = 0.01)
abline(diurnalmodel, lwd=2)
abline(0, 1, lty=2, lwd=2)
points(treeC ~ Cflux, data = dayfluxtomass,pch=c(1,19)[CO2_treatment],col=Water_treatment, cex=1.5)

mtext("p < 0.0001, R = 0.83", side = 1, line = -3, cex = 1)

dev.copy2pdf(file= "master_scripts/paper_figs/WTCI_fluxmass.pdf")
dev.off()


