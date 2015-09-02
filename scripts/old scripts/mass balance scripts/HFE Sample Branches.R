library(multcomp)

chambersumm <- read.csv("HFE chamber treatments.csv")

branches <- read.csv("HFE final harvest Sample Branches.csv")
twig_est <- aggregate(cbind(diameter, length, br2order)~ chamber + layerno, data = branches, FUN=mean)

DWbranches <- read.csv("HFE final harvest Sample Branches dry weight.csv")
DWbranches <- subset(DWbranches, select = c("chamber",  "layerno", "Wbrlt1",  "Wbrgt1", "DWbrlt1", "DWbrgt1"))
DWbranches$percDWbranch <- with(DWbranches, DWbrgt1 / Wbrgt1)
DWbranches$percDWtwig <-  with(DWbranches, DWbrlt1 /  Wbrlt1)

twig_est <- merge(twig_est, subset(DWbranches, select = c("chamber",  "layerno", "percDWbranch", "percDWtwig", "DWbrlt1", "DWbrgt1"),
                  by = c("chamber" , "layerno")))
twig_est$BR_DWbyL <- with(twig_est, DWbrgt1/length)
twig_est$BR_LbyD <- with(twig_est, length/diameter)


chamberorder<-order(twig_est$chamber, by=twig_est$layerno)
twig_est <- twig_est[chamberorder,]
row.names(twig_est)<-NULL

twig_est  <- subset(twig_est , chamber %in% chambersumm$chamber[chambersumm$inside_or_outside_WTC=="inside"]) 
twig_est <- droplevels(twig_est)

twig_est$layerno <- as.factor(twig_est$layerno )

##model to determine number of 2order branches per branch (using branch legnth, good for exisitng data)

#neither the bumber of 2order branches or twig dry mass vary by canopy layer
br2order_test <- lm(br2order ~ layerno, data = twig_est)
summary(br2order_test)
boxplot(br2order ~ layerno, data = twig_est)
tukey_br2order <- glht(br2order_test, linfct = mcp(layerno = "Tukey"))
summary(tukey_br2order)


brtwig_test <- lm(DWbrlt1 ~ layerno, data = twig_est)
summary(brtwig_test)
boxplot(DWbrlt1 ~ layerno, data = twig_est)
tukey_twig <- glht(brtwig_test, linfct = mcp(layerno = "Tukey"))
summary(tukey_twig)

#twig branch number model, use branch length too estimate the number of 2order branches
br2order_model <- lm(br2order ~ length, data = twig_est)
summary(br2order_model)

#extract coefficients
br2orderpred <- as.data.frame(coef(br2order_model))
names(br2orderpred)[1] <- "br2order_coef"


#twig mass model, use number of 2order branches to estimate twig mass
twigmass_model <- lm(DWbrlt1 ~ br2order, data = twig_est)
summary(twigmass_model)

#extract coefficients
twigmassrpred <- as.data.frame(coef(twigmass_model))
names(twigmasspred)[1] <- "twigmass_coef"













