leaf1 <- read.csv("raw csv/Leaf area samples April 2008.csv")
leaf1 <- leaf1[complete.cases(leaf1),]


leaf2 <- read.csv("raw csv/HFE final harvest leaf samples.csv")
leaf2$SLA <- with(leaf2, laminaarea/(laminaweight+petioleweight))

library(doBy)

leaf1_agg <- summaryBy(SLA ~chamber, data= leaf1, FUN=mean, keep.names=TRUE)
leaf2_agg <- summaryBy(SLA ~chamber, data= leaf2, FUN=mean, keep.names=TRUE)

