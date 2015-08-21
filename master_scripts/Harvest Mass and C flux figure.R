#read data for cumulative C flux and harvest mass
dayfluxtomass <- read.csv("whole tree csv/harvest mass and carbon flux_newroots.csv")   


#CO2 flux to harvest mass scatter plot

#linear model to show fit on graph
diurnalmodel <- lm(treeC ~ Cflux, data = dayfluxtomass)
summary(diurnalmodel)

modeltrt<- lm(treeC ~ Cflux*CO2_treatment*Water_treatment, data = dayfluxtomass)
anova(modeltrt)

getP <- function(x)anova(x)[[5]][1]
getP(diurnalmodel)

##plot with total flux
leglab2 = c(expression(paste(a,"[",CO[2], "]", " " ,"wet")), expression(paste(a,"[",CO[2], "]", " " ,"dry")),
            expression(paste(e,"[",CO[2], "]", " " ,"wet")), expression(paste(e,"[",CO[2], "]", " " ,"dry")))

palette (c("red", "blue"))

windows (10,10)
par(cex.lab = 1.2, mar=c(5,5,1,1))

plot(treeC ~ Cflux, data = dayfluxtomass,
     pch=c(1,19)[CO2_treatment],col=Water_treatment, cex=1.3,
     ylab = expression(Whole~Tree~Carbon~(g ~ C)),
     xlab=expression(Chamber~Flux~(g ~ C)),
     xlim = c(0, 30000),
     ylim = c(0, 30000))

legend("topleft", leglab2, pch=c(1,1, 19, 19), col=c("blue", "red", "blue", "red"), inset = 0.03)
abline(diurnalmodel)
abline(0, 1, lty=2)

mtext("p < 0.0001, R = 0.83", side = 1, line = -3, cex = 0.9)

dev.copy2pdf(file= "output/WTCI_fluxmass.pdf")


