
######TREE CARBON FLUX (from floor date set at 0) by chamber
source("functions and packages/load packages.R")
source("functions and packages/functions.R")


# read root mass and total treemass with C flux
treeC <- read.csv("whole_tree_csv/tree_C_flux.csv")
  treeC$Date <- as.Date(treeC$Date)
  treeC$abovegroundC <- with(treeC, boleC+branchC+leafC+litterC)

#test both root data sets (with or without very fine roots)
root <- read.csv("calculated mass/root_mass.csv")
  root$Date <- as.Date(root$Date)

#root values need to be additive to aboveground C 
finalC <- subset(treeC, Date=="2009-03-16")
  finalC <- subset(finalC, select = c("chamber", "Date", "abovegroundC"))
  finalC <- merge(finalC, root)

  finalC$coarseabove <- with(finalC, abovegroundC+CrootC)
  finalC$frootabove <- with(finalC, coarseabove+frootC)
  finalC$allrootabove <- with(finalC, coarseabove+frootC_all)

#function that plots plant additive plant componenhts vs cumulative C flux 
plot_treeC <- function(ch){
  
  windows (7,10)
  par(cex.axis = 0.9, 
      #omi = c(1,1,0.2,0.2),  # outer margin (inches)
      mar=c(5,5,1,1))  # margin around plots (they are tight together)
  
  palette(c("black","sienna1","sienna2","sienna3","sienna4", "tomato4"))
  
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
         type = 'l',lty=3,
         col = palette()[2])
  points(boleC+branchC ~ Date , 
         data = treeC,
         subset = chamber == ch,
         pch=17, lwd=LWD,
         type = 'l',lty=2,
         col=palette()[3])
  points(boleC+branchC+leafC+litterC ~ Date , 
         data = treeC,
         subset=chamber == ch,
         pch = 15, lwd=LWD,
         type = 'l',lty=5,
         col=palette()[4])
  
  points(coarseabove ~ Date , 
         data = finalC,
         subset=chamber == ch,
         pch = 21, cex=CEX,
         type = 'p',
         bg=palette()[5])
  
  points(allrootabove ~ Date , 
         data = finalC,
         subset=chamber == ch,
         pch = 21, cex=CEX,
         type = 'p',
         bg=palette()[6])
  
  box()
  
  leglab <- c("Carbon Flux", "Bole", "+Branch", " +Leaf and Litter", "  +Coarse", "   +Fine Root")
  
  
  legend("topleft",leglab, lty = c(1, 1, 1, 1, NA, NA), pch = c(NA, NA, NA, NA, 21, 21),lwd=LWD,  
         col = c("black","sienna1","sienna2","sienna3","sienna4", "tomato4"), pt.bg = c("sienna4", "tomato4"),
          bty='n', pt.cex=CEX, title = expression(bold("WTCI Tree Carbon")), inset = 0.01)
  #Axis titles
  # mtext("Date", side = 1, outer = TRUE, line = -2.5, cex = 1.3)
  mtext("Carbon (g)", side = 2, outer=TRUE, line = -2.5, cex = 1.3)
  
  title(main = ch, line=-1) 
}

#test chamber
plot_treeC("ch11") #(choose which froot to use:frootabove or allrootabove)

#loop through all
chamnrs <- levels(treeC$chamber)

for(i in 1:length(chamnrs)){
  plot_treeC(chamnrs[i])
  mypath <- file.path("output/chamber plots", paste("carbon_flow","chamnrs[i]",".pdf", sep=""))
  dev.copy2pdf(file=mypath)
}




