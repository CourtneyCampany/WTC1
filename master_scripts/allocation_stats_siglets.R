###stats for component


##need final harvest values (not treatmen means)
tree_C <- read.csv("master_scripts/harvest_chamber.csv")

##need TBCA to add to roots
tbca <- read.csv("calculated_mass/TBCA.csv")

tree_C <- merge(tree_C, tbca)