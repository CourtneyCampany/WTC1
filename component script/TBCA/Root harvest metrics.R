# chamber treatments
chambersumm <- read.csv("HFE chamber treatments.csv")
#root harvest
rootM <- read.csv("HFE Fine root cores final harvest.csv")
rootM$wr510 <- as.numeric(rootM$wr510)
rootM$rootmass_tot <-  with(rootM, veryfine + wrlt2	+ wr25	+ wr510	+ wrgt10)
#means for each chamber by depth (from 5 cores)
rootM_mean <- aggregate(cbind(veryfine, wrlt2, wr25,	wr510,	wrgt10, rootmass_tot) ~ chamber * lower, data=rootM, FUN=mean)
names(rootM_mean)[2] <- "coredepth"


#root data across time
root<-read.csv("Rootlengthmass.csv")
names(root)[1] <- "chamber"
root<-subset(root, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
root <- merge(root, chambersumm, by = "chamber")
root$DATE <-as.POSIXct(as.character(root$DATE) ,tz="GMT",format="%d/%m/%Y")
str(root)

root$coredepth <- if(root$DEPTH == 0-15){"15"} 
else if(root$DEPTH == 15-30){"30"} 
else {"70"}

branch_position$layer <- if(branch_position$height_mid < mean_layer$no1){"1"
} else if(branch_position$heightground < mean_layer$no2 & branch_position$heightground > mean_layer$no1){
  "2"
} else if(branch_position$heightground < mean_layer$no3 & branch_position$heightground > mean_layer$no2) {
  "3"
} else if(branch_position$heightground < mean_layer$no4 & branch_position$heightground > mean_layer$no3){
  "4"
} else {"5"}