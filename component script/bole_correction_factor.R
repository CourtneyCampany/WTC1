####Correction factors for boles and branches

###read in bole havest
treemass <- read.csv("raw csv/HFE final DM totals.csv")
bole_harvest <- treemass[1:12, c("chamber",  "ws")]

###read in bole predicted (volume, density, bark:wood, etc)
bole <- read.csv("calculated_mass/bole310 mass.csv")
bole$Date <- as.Date(bole$Date)

bole_last <- bole[bole$Date == max(bole$Date),]


###Read in simplified bole mass calculation
bole2 <- read.csv("calculated_mass/bole_mass_pred_simple.csv")
bole2$Date <- as.Date(bole2$Date)
bole2_last <- bole2[bole2$Date == max(bole2$Date),]


###how different are they?????

bole_pred <- merge(bole_harvest, bole_last)
bole_pred$diff <- with(bole_pred, (bole_mass-ws)/bole_mass)

bole2_pred <- merge(bole_harvest, bole2_last)
bole2_pred$diff <- with(bole2_pred, (bole_mass-ws)/bole_mass)


##calculate correction factor
bole_last <- bole[bole$Date == max(bole$Date),]


treeC <- read.csv("whole_tree_csv/tree_C_flux.csv")
treeC$Date <- as.Date(treeC$Date)

treeC_last <- treeC[treeC$Date == max(treeC$Date),]


bole_correction <- merge(harvestbole, bolepred_last[, c(2,4)])