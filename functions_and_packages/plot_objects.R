library(scales)
library(wesanderson)

leaflab <- expression(Leaf~Area~~(m^2))


ypos <- c(2.5,1,0)

trtlab <-c(expression(aCO[2]),expression(eCO[2]), "Drought", "Control")


rmflab <- expression(RMF~~(g~g^-1))
smflab <- expression(SMF~~(g~g^-1))
lmflab <- expression(LMF~~(g~g^-1))

boxlab <- c(expression(aCO[2]-Drought), expression(aCO[2]-Control), expression(eCO[2]-Drought), expression(eCO[2]-Control))


dayClab <- c("Carbon Flux", "Bole", "+Branch", " +Leaf and Litter", "  +Coarse", "   +Fine Root")
