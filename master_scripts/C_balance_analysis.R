# read root mass and total treemass with C flux
root <- read.csv("calculated mass/root_mass.csv")
  root$Date <- as.Date(root$Date)

#treeC <- read.csv("whole_tree_csv/Tree Flux and Mass.csv")

treeC <- read.csv("whole_tree_csv/tree_mass_Cflux.csv")
  treeC$Date <- as.Date(treeC$Date)

##source packages and functions
source("functions and packages/load packages.R")
source("functions and packages/HFEI quick graph functions.R")


## quick plotting using functions
plotonechamber("ch12")
#dev.copy2pdf(file= "output/WTCI_chamber12.pdf")

chamnrs <- levels(treeC$chamber)
for(i in 1:length(chamnrs)){
  plotonechamber(chamnrs[i])
}


plotflux_abovemass("ch01")

chamnrs2 <- levels(treeC$chamber)
for(i in 1:length(chamnrs)){
  plotflux_abovemass(chamnrs2[i])
}

#PAPER FIGURES
source("Paper Figures/Harvest Mass and C flux figure.R")
dev.copy2pdf(file= "output/WTCI_fluxharvestmass.pdf")
