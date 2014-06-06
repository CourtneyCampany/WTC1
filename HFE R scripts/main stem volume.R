treeD<-read.csv("HFE Tree Diameters all.csv")
treemain<-subset(treeD, treeD$Stemsegmnt == 1)
treemain$Date <-as.character(treemain$Date)
treemain$Date <-as.Date(treemain$Date)

treeH<-read.csv("HFE Tree Height Fixed.csv")
names(treeH)[3]<-"Pathlength"
treeH$Date <-as.character(treeH$Date)
treeH$Date <-as.Date(treeH$Date)
treeH$Pathlength<-treeH$Pathlength*100
treeH$Diameter<-ifelse(treeH$Pathlength >0, "0", "NA")
treeH$Stemsegmnt<-ifelse(treeH$Pathlength >0, "1", "NA")

wholetree<-rbind(treemain, treeH)

chamberorder<-order(wholetree$chamber, by=wholetree$Date)
wholetree<-wholetree[chamberorder,]
wholetree$Diameter<-as.numeric(ifelse(wholetree$Diameter>0, wholetree$Diameter, 0))
wholetree$Volume<-((((wholetree$Diameter/2)^2)*(pi))*wholetree$Pathlength)
wholetree$Volume<-round(wholetree$Volume, 4)


treevolume<-subset(wholetree, select = c(chamber, Date, Volume, Pathlength))
treevolume_sp<-split(treevolume, treevolume[, c("chamber", "Date")])



library(reshape)
PV5<-melt(treevolume_sp, id=c("chamber", "Date"), measured=c("Pathlength", "Volume"))
TV1<-cast(PV5, value + variable ~ chamber + Date)

pathvolume<-lapply(treevolume_sp, function(x)diff(x$Volume))
PV4<-meld(pathvolume, Volume~chamber+Date+Pathlength)


##???treevolume_sp<-droplevels()


library(reshape)
treevolume1<-sparseby(treevolume, INDICES = list(c("chamber", "Date")),FUN=NULL, GROUPNAMES = TRUE)
###need to meld here???
treevolume1<-cast(treevolume, Volume~chamber+Date+Pathlength)



#need a function to subtract 
#solve this and then do somethings similar by subsetting each split stem
#then add the different data together
