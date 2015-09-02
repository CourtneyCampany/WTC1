######TREE CARBON FLUX (from floor date set at 0) by treatment means
source("functions_and_packages/load_packages.R")
source("functions_and_packages/functions.R")

# read root mass and total treemass with C flux
treeC <- read.csv("whole_tree_csv/tree_C_flux.csv")
  treeC$Date <- as.Date(treeC$Date)
  treeC$abovegroundC <- with(treeC, boleC+branchC+leafC+litterC)

#test both root data sets (with or without very fine roots)
root <- read.csv("calculated_mass/root_mass.csv")
  root$Date <- as.Date(root$Date)

#root values need to be additive to aboveground C 
finalC <- subset(treeC, Date=="2009-03-16")
  finalC <- subset(finalC, select = c("chamber", "Date", "abovegroundC"))
  finalC <- merge(finalC, root)

  finalC$coarseabove <- with(finalC, abovegroundC+CrootC)
  finalC$frootabove <- with(finalC, coarseabove+frootC)
  finalC$allrootabove <- with(finalC, coarseabove+frootC_all)


#-------------------------------------------------------------------------------------------------------
#summary tables of additive components for plotting

# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])
  
treeC_means <- merge(treeC, chambersumm)
  treeC_means$treatment <- as.factor(paste(treeC_means$CO2_treatment, treeC_means$Water_treatment, sep="-"))
  treeC_means$bolebranch <- with(treeC_means, boleC+branchC)
  treeC_means$aboveC <- with(treeC_means, boleC+branchC+leafC+litterC)

#aboveground components
fluxC_means <- ddply(treeC_means, .(treatment, Date), summarize,
                     CO2cum = round(mean(CO2cum), 2),  
                     boleC = round(mean(boleC), 2),
                     bolebranch = round(mean(bolebranch), 2),
                     aboveC = round(mean(aboveC), 2))

fluxC_se <- ddply(treeC_means, .(treatment, Date), summarize,
                  CO2cum_se = round(se(CO2cum), 3), 
                  boleC_se = round(se(boleC), 3),
                  bolebranch_se = round(se(bolebranch), 3),
                  aboveC_se = round(se(aboveC), 3))

treeC_stats <- merge(fluxC_means, fluxC_se)    

write.csv(treeC_stats, "calculated_mass/treeC_day.csv", row.names=FALSE)

#roots
finalC_means <- merge(finalC, chambersumm)
  finalC_means$treatment <- as.factor(paste(finalC_means$CO2_treatment, finalC_means$Water_treatment, sep="-"))

rootC_means <- ddply(finalC_means, .(treatment, Date), summarize,                 
                     coarseabove = round(mean(coarseabove),2),
                     allrootabove = round(mean(allrootabove), 2))

rootC_se <- ddply(finalC_means, .(treatment, Date), summarize,                 
                  coarseabove_se = round(se(coarseabove),3),
                  allrootabove_se = round(se(allrootabove), 3))

rootC_stats <- merge(rootC_means, rootC_se)

# plot treatment means
plot_trt_means <- function(trt){
  
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
       data = treeC_stats,
       type = 'n',  
       axes = FALSE, ann = FALSE)
  axis.Date(1, at = xAT, labels = T)
  axis(2)
  
  points(CO2cum ~ Date,
         data = treeC_stats,
         subset = treatment == trt,
         pch = 19, lwd=LWD,
         type = 'l',
         col = palette()[1])
  points(boleC ~ Date, 
         data = treeC_stats,
         subset = treatment == trt,
         pch = 18, lwd=LWD,
         type = 'l',lty=3,
         col = palette()[2])
  points(bolebranch ~ Date , 
         data = treeC_stats,
         subset = treatment == trt,
         pch=17, lwd=LWD,
         type = 'l',lty=2,
         col=palette()[3])
  points(aboveC ~ Date , 
         data = treeC_stats,
         subset = treatment == trt,
         pch = 15, lwd=LWD,
         type = 'l',lty=5,
         col=palette()[4])
  
  points(coarseabove ~ Date , 
         data = rootC_stats,
         subset = treatment == trt,
         pch = 21, cex=CEX,
         type = 'p',
         bg=palette()[5])
  
  points(allrootabove ~ Date , 
         data = rootC_stats,
         subset = treatment == trt,
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
  
  title(main = trt, line=-1) 
}


#loop through all
trt_lvl <- levels(rootC_stats$treatment)


for(i in 1:length(trt_lvl)){
  par(mfrow=c(2,2))
  plot_trt_means(trt_lvl[i])
  mypath <- file.path("output", paste("carbon_balance_", trt_lvl[i], ".pdf", sep=""))
  dev.copy2pdf(file=mypath)
}
