# Decide on where to put ticks on the X axis, which is a POSIXct object ('DateTime').


windows (7,10)
par(cex.axis = 0.9,  # axis label size
    mfrow = c(4,3),  # rows, columns  plots
    omi = c(1,1,0.2,0.2),  # outer margin (inches)
    mar = c(0,0,0,0))  # margin around plots (they are tight together)

plotchamber <- function(ch){
  
  palette(c("black","blue","orange","forestgreen","brown"))
  
  xAT <- seq.Date(from=as.Date("2008-4-1"), length=13, by="month")
  LWD <- 2
  
  #First Panel
  plot(CO2cum ~ Date, 
       data = treeflux,
       type = 'n',  
       axes = FALSE, ann = FALSE)
  axis.Date(1, at = xAT, labels = T)
  axis(2)
  
  points(CO2cum ~ Date,
         data = treeflux,
         subset = chamber == ch,
         pch = 19, lwd=LWD,
         type = 'l',
         col = palette()[1])
  points(boleC ~ Date, 
         data = treeflux,
         subset = chamber == ch,
         pch = 18, lwd=LWD,
         type = 'l',
         col = palette()[2])
  points(branchC ~ Date , 
         data = treeflux,
         subset = chamber == ch,
         pch=17, lwd=LWD,
         type = 'l',
         col=palette()[3])
  points(leafC ~ Date , 
         data = treeflux,
         subset=chamber == ch,
         pch = 15, lwd=LWD,
         type = 'l',
         col=palette()[4])
  points(litterC ~ Date, 
         data=treeflux,
         subset = chamber == ch,
         pch = 15, lwd=LWD,
         type = 'l',
         col = palette()[5])
  box()
  legend("top",ch,bty='n')
}

plotchamber("ch09")

chamnrs <- levels(treeflux$chamber)
for(i in 1:length(chamnrs)){
  plotchamber(chamnrs[i])
}


#Axis titles
mtext("Date", side = 1, outer = TRUE, line = 3, cex = 1.3)
mtext("Carbon (g)", side = 2, outer=TRUE, line = 3, cex = 1.3)
