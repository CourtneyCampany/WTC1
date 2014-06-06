# chamber treatments
chambersumm <- read.csv("HFE chamber treatments.csv")

##write in harvest mass and get twig mass, leaf area, and leaf mass
twigM <- read.csv("HFE final harvest biomass by layer.csv")
twigM <- subset(twigM, select =c("chamber",  "layerno", "Wleaf", "Wbrlt1", "Wbrgt1", "Ws"))
names(twigM)[3:6] <- c("leafmass", "twigmass", "branchmass", "stemmass")
twigM$treemass <- with(twigM, leafmass + twigmass + branchmass + stemmass)

##tree totals(sum layers)
treeM <- aggregate(treemass ~ chamber, FUN = sum, data = twigM)
treeM_trt <- merge(treeM, chambersumm, by = "chamber")
treeM_trt <- subset(treeM_trt, treeM_trt$inside_or_outside_WTC == "inside")
treeM_trt$TreeC <- treeM_trt$treemass *.5

with(treeM_trt, tapply(TreeC, list(CO2_treatment, Water_treatment), FUN = mean))