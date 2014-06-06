
setwd("C:/Users/90919620/Google Drive/HFE Database")

# chamber treatments
chambersumm <- read.csv("HFE chamber treatments.csv")

# Litter dataframes
litter_harv <- read.csv("HFE Litter after final harvest.csv")

litter_HFE<- read.csv("HFE leaf litter 2008-2009.csv")
litter_HFE$date <- as.character(litter_HFE$date)
litter_HFE$date <- as.Date(litter_HFE$date)

littertosept <- subset(litter_HFE, litter_HFE$date >= "2008-04-15" & litter_HFE$date <= "2008-09-17")
littertooct <- subset(litter_HFE, litter_HFE$date > "2008-09-17" & litter_HFE$date <= "2008-10-28") 
littertonov <- subset(litter_HFE, litter_HFE$date > "2008-10-28" & litter_HFE$date <= "2008-11-15")    
littertodec <- subset(litter_HFE, litter_HFE$date > "2008-11-15" & litter_HFE$date <= "2008-12-11")
littertojan <- subset(litter_HFE, litter_HFE$date > "2008-12-11" & litter_HFE$date <= "2009-01-15")
littertomar <- subset(litter_HFE, litter_HFE$date > "2009-01-15" & litter_HFE$date <= "2009-03-16")

sept_litter <- aggregate(leaflitter ~ chamber, data = littertosept, FUN = sum)                                            
oct_litter <- aggregate(leaflitter ~ chamber, data = littertooct, FUN = sum)
nov_litter <- aggregate(leaflitter ~ chamber, data = littertonov, FUN = sum)
dec_litter <- aggregate(leaflitter ~ chamber, data = littertodec, FUN = sum)                     
jan_litter <- aggregate(leaflitter ~ chamber, data = littertojan, FUN = sum) 
mar_litter <- aggregate(leaflitter ~ chamber, data = littertomar, FUN = sum) 

#add date and rbind
sept_litter$date <- "2008-09-01"
oct_litter$date <- "2008-10-01"
nov_litter$date <- "2008-11-01"
dec_litter$date <- "2008-12-01"
jan_litter$date <- "2009-01-01"
mar_litter$date <- "2009-03-16"

#accumulated litter between dates, sept is from april to sept
litterflux <- rbind(sept_litter, oct_litter, nov_litter, dec_litter, jan_litter, mar_litter)
litterflux$date <- as.Date(litterflux$date)
chamberorder<-order(litterflux$chamber, by=litterflux$date)
litterflux<-litterflux[chamberorder,]





