
#litter
litter_HFE<- read.csv("HFE leaf litter 2008-2009.csv")
litter_HFE$Date <- as.character(litter_HFE$Date)
litter_HFE$Date <- as.Date(litter_HFE$Date)

#merge litter with Leaf Area est(this dataframe has all days and is basis for other linear interpolations)
#leaf area estimates
leafA_est <- read.csv("HFE LA estimates alldates.csv")

litter_HFE <- merge(litter_HFE, subset(leafA_est, select c = ("Date", "chamber", "LAestlin")), by = c("Date", "chamber"), all=TRUE)

#number of days between litter collections
septdays <- as.numeric(as.Date("2008-09-17") - as.Date("2008-04-15"))
octdays <- as.numeric(as.Date("2008-10-28") - as.Date("2008-09-17"))
novdays <- as.numeric(as.Date("2008-11-15") - as.Date("2008-10-28"))
decdays <- as.numeric(as.Date("2008-12-11") - as.Date("2008-11-15"))
jandays <- as.numeric(as.Date("2009-01-15") - as.Date("2008-12-11"))
mardays <- as.numeric(as.Date("2009-03-16") - as.Date("2009-01-15"))



#add litter flux (sum littermass between dates)
littertosept <- subset(litter_HFE, litter_HFE$Date >= "2008-04-15" & litter_HFE$Date <= "2008-09-17")
littertosept$dayflux <- littertosept$leaflitter/septdays

littertooct <- subset(litter_HFE, litter_HFE$Date > "2008-09-17" & litter_HFE$Date <= "2008-10-28") 
littertooct$dayflux <- littertooct$leaflitter/octdays 

littertonov <- subset(litter_HFE, litter_HFE$Date > "2008-10-28" & litter_HFE$Date <= "2008-11-15")  
littertonov$dayflux <- littertonov$leaflitter/novdays

littertodec <- subset(litter_HFE, litter_HFE$Date > "2008-11-15" & litter_HFE$Date <= "2008-12-11")
littertodec$dayflux <- littertodec$leaflitter/decdays

littertojan <- subset(litter_HFE, litter_HFE$Date > "2008-12-11" & litter_HFE$Date <= "2009-01-15")
littertojan$dayflux <- littertojan$leaflitter/jandays

littertomar <- subset(litter_HFE, litter_HFE$Date > "2009-01-15" & litter_HFE$Date <= "2009-03-16")
littertomar$dayflux <- littertomar$leaflitter/mardays


