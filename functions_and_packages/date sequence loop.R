#read chamber treatments for levels of loop
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
cham <- subset(chambersumm, inside_or_outside_WTC == "inside")

#empty list
datels <- list()

#date seq loop
datemaker <- for (i in unique(cham$chamber)){
  datels[[i]] <- data.frame(Date = seq(from = as.Date("2008-3-14"), to = as.Date("2009-3-16"), by = "day"), 
                         chamber = i, bole_mass = "")}

# row-bind everything together:
dateseq <- do.call(rbind,datels)
                 
                    
#-----------------------------------------------------------------------------------------------------------  
#example with not using a loop, using repeat command                     
Date  <-  seq(from = as.Date("2008-3-14"), to = as.Date("2009-3-16"), by = "day")
chamber <- unique(cham$chamber)
                    
                    
  Date2 <- rep(Date,12)
  head(Date2)
  chamber2 <- rep(chamber,each=length(Date))
  dateseq <- data.frame(as.Date(Date2),chamber2)                
                    