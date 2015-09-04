####Correction factors for boles and branches

###read in bole predict and harvest mass
treemass <- read.csv("raw csv/HFE final DM totals.csv")
totals <- treemass[1:12, c("chamber", "wf", "wbr", "ws", "wr")]

finalC <- read.csv("calculated_mass/chamber_carbon.csv")

bole <- read.csv("calculated_mass/bole310 mass.csv")
bole$Date <- as.Date(bole$Date)

##calculate correction factor
bole_last <- bole[bole$Date == max(bole$Date),]


treeC <- read.csv("whole_tree_csv/tree_C_flux.csv")
treeC$Date <- as.Date(treeC$Date)

treeC_last <- treeC[treeC$Date == max(treeC$Date),]


bole_correction <- merge(harvestbole, bolepred_last[, c(2,4)])