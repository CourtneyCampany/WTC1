library(scales)
library(wesanderson)

leaflab <- expression(Leaf~Area~~(m^2))
meanlalab <- expression(Daily~Leaf~Area~~(m^2))
treefluxlab <- "Tree Carbon Flux  (g C)"

tbcalab <- "TBCA  (g)"
fslab <- expression(F[s]~residual)

ypos <- c(2.5,1,0)

trtlab <-c(expression(aCO[2]),expression(eCO[2]), "Wet", "Dry")

leglab2 = c(expression(paste(aCO[2], ", " ,"wet")), expression(paste(aCO[2], ", " ,"dry")),
            expression(paste(eCO[2], ", " ,"wet")), expression(paste(eCO[2], ", " ,"dry")))


rmflab <- expression(RMF~~(g~g^-1))
smflab <- expression(SMF~~(g~g^-1))
lmflab <- expression(LMF~~(g~g^-1))


boxlab <- c(expression(atop(aCO[2], Dry)), expression(atop(aCO[2], Wet)), expression(atop(eCO[2], Dry)),
            expression(atop(eCO[2], Wet)))


dayClab <- c("Carbon Flux", "Bole", "+Branch", " +Leaf and Litter", "  +Root")

belowfluxlab <- c("Tree Carbon Flux", "TBCA", fslab)

Mablab <- "Aboveground Carbon  (g C)"
treeclab <- "Whole Tree Carbon  (g C)"
