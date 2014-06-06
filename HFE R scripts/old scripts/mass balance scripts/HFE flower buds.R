branches <- read.csv("HFE final harvest Sample Branches.csv")
DWbranches <- read.csv("HFE final harvest Sample Branches dry weight.csv")

flowerbud <- subset(branches, select = c("chamber", "layerno",	"branchnumber", "Wflowerbuds"))
budDW <- subset(DWbranches, select = c("chamber", "layerno", "Wflwrbud", "DWflwrbud"))
budDW <- subset(budDW, !is.na(DWflwrbud))
budDW$DWperc <- with(budDW, DWflwrbud/Wflwrbud)

DWperc <- mean(budDW$DWperc)

flowerbud$DWflowerbuds <- flowerbud$Wflowerbuds * DWperc

chambersumm <- read.csv("HFE chamber treatments.csv")

flowerbud  <- subset(flowerbud , chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"])
flowerbud = droplevels(flowerbud)

