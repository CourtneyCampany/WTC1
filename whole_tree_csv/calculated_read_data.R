
# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
  chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
  chambersumm <- droplevels(chambersumm[,1:3])

#interpolated aboveground components and fluxes
fluxmass <- read.csv("whole_tree_csv/tree_C_flux.csv")

#roots
rootmass <- read.csv("calculated mass/root_mass.csv")
  rootmass$Date <- as.Date("2009-03-16")


