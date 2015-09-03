library(scales)
library(wesanderson)

leaflab <- expression(Leaf~Area~~(m^2))
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


boxlab <- c(expression(atop(aCO[2],Drought)), expression(atop(aCO[2],Control)), expression(atop(eCO[2],Drought)),
            expression(atop(eCO[2],Control)))


dayClab <- c("Carbon Flux", "Bole", "+Branch", " +Leaf and Litter", "  +Fine Root", "   +Coarse Root")

belowfluxlab <- c("Tree Carbon Flux", "TBCA", fslab)

Mablab <- "Aboveground Carbon Mass  (g C)"
treeclab <- "Whole Tree Carbon  (g C)"
