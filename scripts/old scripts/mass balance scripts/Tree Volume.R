#branchs + stems

branch <- read.csv("HFE branch volume.csv")
branch <- branch[, 2:3]
names(branch)[2] <- "Branch_Volume"
stem <- read.csv("HFE stem volume.csv")
stem <- subset(stem, select = c("chamber", "Volume", "CO2_treatment", "Water_treatment"))
names(stem)[2] <- "Stem_Volume"


tree <- cbind(branch, stem [, -1])
tree$woodyvolume <- with(tree, Branch_Volume + Stem_Volume)


