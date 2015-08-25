#function to plot one WTC chamber

plotonechamber <- function(ch){
  
  windows (7,10)
  par(cex.axis = 0.9,#,  
      #omi = c(1,1,0.2,0.2),  # outer margin (inches)
      mar=c(5,5,1,1))  # margin around plots (they are tight together)
  
  palette(c("black","blue","orange","forestgreen","firebrick2", "mediumorchid1"))
  
  xAT <- seq.Date(from=as.Date("2008-4-1"), length=13, by="month")
  LWD <- 2
  CEX <- 2 
  
  #First Panel
  plot(CO2cum ~ Date, 
       data = treeC,
       type = 'n',  
       axes = FALSE, ann = FALSE)
  axis.Date(1, at = xAT, labels = T)
  axis(2)
  
  points(CO2cum ~ Date,
         data = treeC,
         subset = chamber == ch,
         pch = 19, lwd=LWD,
         type = 'l',
         col = palette()[1])
  points(boleC ~ Date, 
         data = treeC,
         subset = chamber == ch,
         pch = 18, lwd=LWD,
         type = 'l',
         col = palette()[2])
  points(branchC ~ Date , 
         data = treeC,
         subset = chamber == ch,
         pch=17, lwd=LWD,
         type = 'l',
         col=palette()[3])
  points(leafC+litterC ~ Date , 
         data = treeC,
         subset=chamber == ch,
         pch = 15, lwd=LWD,
         type = 'l',
         col=palette()[4])
  #points(litterC ~ Date, 
  #data=treeflux,
  #subset = chamber == ch,
  #pch = 15, lwd=LWD,
  #type = 'l',
  #col = palette()[5])
  points(coarseroot_mass ~ Date , 
         data = root,
         subset=chamber == ch,
         pch = 21, cex=CEX,
         type = 'p',
         bg=palette()[5])
  
  points(fineroot_mass ~ Date , 
         data = root,
         subset=chamber == ch,
         pch = 21, cex=CEX,
         type = 'p',
         bg=palette()[6])
  
  box()
  
  leglab <- c("Chamber flux", "Bole", "Branch", "Leaf+Litter", "Coarse Root", "Fine Root")
  
  
  legend("topleft",leglab, lty = c(1, 1, 1, 1, NA, NA), pch = c(NA, NA, NA, NA, 21, 21),lwd=LWD,  
         col = c("black","blue","orange","forestgreen","black", "black"), 
         pt.bg = c("firebrick2", "mediumorchid1"), bty='n', pt.cex=CEX, title = expression(bold("WTC Carbon Flow")), inset = 0.03,)
  #Axis titles
  # mtext("Date", side = 1, outer = TRUE, line = -2.5, cex = 1.3)
  mtext("Carbon (g)", side = 2, outer=TRUE, line = -2.5, cex = 1.3)
  
  #title(main= with(data= treeflux, subset=chamber == ch, paste(CO2_treatment, Water_treatment, sep=" ")))
  #title(main= with(data= treeflux, subset=chamber == ch, outer=TRUE, paste(unique(CO2_treatment), unique(Water_treatment), sep=" "))) 
  
}



#......................................
# plot function that plots flux vs aboveground mass


plotflux_abovemass <- function(ch){
  
  windows (7,10)
  par(cex.axis = 0.9,  # axis label size
      omi = c(1,1,0.2,0.2),  # outer margin (inches)
      mar = c(0,0,0,0))  # margin around plots (they are tight together)
  
  
  #First Panel
  plot(boleC + branchC + leafC + litterC ~ CO2cum, 
       data = treeC, xlim = c(0, 15000),
       ylim = c(0, 15000),
       subset = chamber == ch)
  
  abline(0,1)
  
  box()
  
  #leglab2 <- c("Chamber flux", "abovegroundC")
  
  
  #legend("topleft",leglab,lwd=LWD, col = palette(), bty='n', title = "WTC Carbon Flow", inset = 0.03,)
  #Axis titles
  #mtext("Date", side = 1, outer = TRUE, line = 3, cex = 1.3)
  #mtext("Carbon (g)", side = 2, outer=TRUE, line = 3, cex = 1.3)
  
}




