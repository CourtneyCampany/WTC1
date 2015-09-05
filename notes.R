test <- read.csv("calculated_mass/chamber_carbon.csv")


testleaf <- read.csv("calculated_mass/leaf_carbon.csv")
testleaf$Date <- as.Date(testleaf$Date)

leaflast <- testleaf[testleaf$Date == max(testleaf$Date),]

###branches without cwd missed harvest totals
testbranch <- read.csv("calculated_mass/branch mass.csv")
testbranch$Date <- as.Date(testbranch$Date)
lastbranch <- testbranch[testbranch$Date == max(testbranch$Date),]
lastbranch$br_carbon <- lastbranch$branch_mass * .5

###branches with cwd added equals final mass ......
testbranch2 <- read.csv("calculated_mass/branch_mass_cwd.csv")
testbranch2$Date <- as.Date(testbranch2$Date)
lastbranch2 <- testbranch2[testbranch2$Date == max(testbranch2$Date),]
lastbranch2$br_carbon <- lastbranch2$branch_mass * .5


###orginally considered LAlitter to be litter mass, must use SLA to calculate litter
###litter is estimated with LA so only need to fix one place then do stats and add to analyses
###possible to only use to LA potential in final analysis
### estimating LITTER mass every day can be used for cumulative figure.....
### but for final harvest I dont know how to deal with this


##is estimated Litter cumultaice?
testlitter <- testleaf[, c(1:2, 15)]
testlitter <-testlitter[order(testlitter$Date),]

litterch2 <- testlitter[testlitter$chamber == "ch02",]
###should be good to use in harvest file if subtract from start date

