##take harvest frewsh mass and wet/dry conversion from harvest cookies to calculated stem dry mass


# chamber treatments
chambersumm <- read.csv("raw csv/HFE chamber treatments.csv")
chambersumm <- subset(chambersumm, inside_or_outside_WTC == "inside")
chambersumm <- droplevels(chambersumm[,1:3])


stem_fresh <- read.csv("raw csv/HFE final harvest layer stem fresh mass.csv")


#stem density cookies from harvest to get wet conversion
stem_density <- read.csv("raw csv/HFE wood density cookies.csv")
  stem_density <- subset(stem_density, chamber %in% unique(chambersumm$chamber))
  stem_density <- droplevels(stem_density)