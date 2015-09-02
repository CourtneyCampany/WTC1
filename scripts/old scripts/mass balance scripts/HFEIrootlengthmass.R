# chamber treatments
chambersumm <- read.csv("HFE chamber treatments.csv")

#root data with treatments
root<-read.csv("Rootlengthmass.csv")
names(root)[1] <- "chamber"
root<-subset(root, chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
root <- merge(root, chambersumm, by = "chamber")
root$DATE <-as.POSIXct(as.character(root$DATE) ,tz="GMT",format="%d/%m/%Y")

#extract months
root$Month <- months(root$DATE)
monthSorted <- unique(root$Month)
root$Month <- factor(root$Month, levels=monthSorted)

bymonth<-split(root1,root1$Month)
head(root1)
str(root1)


#run summary, make table
library(doBy)
root1<-root1[complete.cases(root1)]
summaryBy (root_length + root_mass ~ depth, data=root1, FUN=c(mean, sd))


modellegnth<- lm(log10(root_length)~log10(treatment), data=root)
summary(model)

#diagnostic plots
plot(modellength, which=c(3,2))
#plot of residuals should have no pattern
#QQplot should fit to line

#make figures

pdf("root_length x treatment by Month.pdf", onefile=TRUE)

for(i in 1:12){
  dataset<-bymonth[[i]]
  with(dataset, plot(root_length, treatment,
                     main=names(bymonth)[i],
                     #add treatment legend
                     #add treatment palette color scheme
                     pch=19,
                     col=4,
                     ylim=c(0,5),
                     xlim=c(0, 3000),
                     xlab=expression(PAR~(mu * mol ~ m^-2 ~ s^-1)),
                     ylab=expression(VPD~(kPa))
  ))
}

dev.off()



