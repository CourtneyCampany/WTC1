###this script visualizes allocation to different components by treatment

##need final harvest values (not treatmen means)
tree_C <- read.csv("master_scripts/harvest_chamber.csv")

##need TBCA to add to roots
tbca <- read.csv("calculated_mass/TBCA.csv")

tree_C <- merge(tree_C, tbca)

##calculated component fractions
tree_C$tree_total <- with(tree_C, branchC+boleC+leafC+litterC+frootC_all+CrootC)

tree_C$lmf <- with(tree_C, leafC_litterC/tree_total)
tree_C$smf <- with(tree_C, (branchC+boleC)/tree_total)
tree_C$rmf <- with(tree_C, (frootC_all + CrootC)/tree_total)

### for another set of figures look at fluxes to total flux
tree_C$fbf <- with(tree_C, (TBCA/CO2cum))
tree_C$lflux <- with(tree_C, (leafC_litterC/CO2cum))

##order by treatment
tree_C <- tree_C[order(tree_C$treatment),]


cols <- c("black", "blue", "red", "forestgreen")

at.y1 <- seq(0, by=.03, length.out=5)
at.y2 <- seq(.4, by=.05, length.out=5)
at.y3 <- seq(.3, by=.1, length.out=3)


at.x <- seq(1, by=.15, length.out = 4)

###Lets try a 3 panel box plotwith LMF, RMF, SMF by treatment
windows(7,7)

par(cex.axis=1.51, cex.lab=1.51, las=1,mgp=c(4,1,0),mfrow=c(3,1),  omi=c(.5,0,0.1,0.1))  

par(mar=c(0,7,2,2))
with(tree_C, boxplot(lmf~treatment, border=cols, ylab=lmflab, boxwex = .1, xaxt='n',yaxt='n', xlab="",ylim=c(0, .125),
                   whisklty=1, whisklwd=2, boxlwd=2, staplelwd=2, at=at.x, xlim=c(.9,1.5)))
axis(2, at=at.y1, labels = TRUE)

par(mar=c(0,7,0,2))
with(tree_C, boxplot(smf~treatment, border=cols, ylab=smflab, boxwex = .1, xaxt='n',yaxt='n', xlab="", ylim=c(.4,.65),
                     whisklty=1, whisklwd=2, boxlwd=2, staplelwd=2,at=at.x, xlim=c(.9,1.5)))
axis(2, at=at.y2, labels = TRUE)

par(mar=c(8.5,7,0,2))
with(tree_C, boxplot(rmf~treatment, border=cols, ylab=rmflab, boxwex = .1, xaxt='n',yaxt='n', ylim=c(.25, .55),
                     whisklty=1, whisklwd=2, boxlwd=2, staplelwd=2, show.names=TRUE,at=at.x, xlim=c(.9,1.5)))
axis(1, at=at.x ,labels=FALSE)
axis(2, at=at.y3, labels = TRUE)

text(at.x-.05 , .07,boxlab, xpd=TRUE, srt=45, cex=1.5)



####TBCA and LMF to flux
at.y4 <- seq(.3, by=.1, length.out=4)
at.y5 <- seq(0, by=.03, length.out=4)


windows(7,7)
par(cex.axis=1.2, cex.lab=1.2, las=1,mgp=c(4,1,0),mfrow=c(2,1),  omi=c(.5,0,0.1,0.1))  

par(mar=c(0,7,2,2))
with(tree_C, boxplot(fbf~treatment, border=cols, ylab="belowground flux", boxwex = .1, xaxt='n', xlab="",yaxt='n',
                     whisklty=1, whisklwd=2, boxlwd=2, staplelwd=2))
axis(2, at=at.y4, labels = TRUE)

par(mar=c(7,7,0,2))
with(tree_C, boxplot(lflux~treatment, border=cols, ylab="leaf flux", boxwex = .1,  xlab="", yaxt='n',xaxt='n',
                     whisklty=1, whisklwd=2, boxlwd=2, staplelwd=2))
axis(2, at=at.y5, labels = TRUE)

axis(1, at=1:4 ,labels=FALSE)
text(1:4 + .375 , -.03,boxlab, xpd=TRUE, srt=315, cex=1.2)
     