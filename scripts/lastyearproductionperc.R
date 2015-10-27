###what percentage of growth in last year, assume patterns are similar

###estimate last year value
mass11 <- read.csv("master_scripts/Cmassflux11.csv")

##harvest mass
harvestmass<- read.csv("calculated_mass/chamber_carbon.csv")
names(harvestmass)[9] <- "harvestCmass"


##extract mass only and merge

lastyearproduction <- merge(mass11[, c(1:3,10)], harvestmass[,c(1,9)])
lastyearproduction$percproduction <- with(lastyearproduction, 1-((harvestCmass-treeC)/harvestCmass))

#on average the production was this much in final year:
mean(lastyearproduction$percproduction)
##75.2%


###do the same for biomass with no litter (thats turnover)