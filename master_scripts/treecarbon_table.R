####redo with treatments from harvest and not interpolated data*****
library(doBy)
source("functions_and_packages/functions.R")

tree_C <- read.csv("calculated_mass/chamber_carbon.csv")
tree_C$treatments <- with(tree_C, interaction(CO2_treatment, Water_treatment))

treeC_agg <- summaryBy(.~treatments, data=tree_C, FUN=c(mean,se))

##reorder
treeC_agg2 <- treeC_agg[c(1,3,2,4),]

####START HERE to double check new data


###Data table/stacked bar plot / boxplot of component fractions

#table <- read.csv("master_scripts/harvest_trt_means.csv")


##need to split dataframe and combine with pasted mean(se)

table_means <- treeC_agg2[, c(2:9)]
table_se <- treeC_agg2[, c(10:17)]

trts <- treeC_agg2[, 1]
trts2 <- c("aCO~2~-dry", "aCO~2~-wet", "eCO~2~-dry", "eCO~2~-wet")

###paste and round means and se together
v1 <- data.frame(paste0(sprintf("%5.1f", round(table_means[,1], 1)), " (", sprintf("%4.1f", round(table_se[,1],1)),")"))
v2 <- data.frame(paste0(sprintf("%4.1f", round(table_means[,2], 1)), " (", sprintf("%3.1f", round(table_se[,2],1)),")"))
v3 <- data.frame(paste0(sprintf("%4.1f", round(table_means[,3], 1)), " (", sprintf("%3.1f", round(table_se[,3],1)),")"))
v4 <- data.frame(paste0(sprintf("%4.1f", round(table_means[,4], 1)), " (", sprintf("%3.1f", round(table_se[,4],1)),")"))
v5 <- data.frame(paste0(sprintf("%4.1f",round(table_means[,5], 1)), " (", sprintf("%3.1f", round(table_se[,5],1)),")"))
v6 <- data.frame(paste0(sprintf("%3.1f",round(table_means[,6],1)), " (", sprintf("%2.1f",round(table_se[,6],1)),")"))
# v7 <- data.frame(paste0(sprintf("%4.1f",round(table_means[,7], 1)), " (", sprintf("%3.1f",round(table_se[,7],1)),")"))

tree_table <- cbind(trts2, v1)
tree_table <- cbind(tree_table, v2)
tree_table <- cbind(tree_table, v3)
tree_table <- cbind(tree_table, v4)
tree_table <- cbind(tree_table, v5)
tree_table <- cbind(tree_table, v6)
# tree_table <- cbind(tree_table, v7)


##order the variables
tree_table <- tree_table[, c(1,5,4,6,7,3,2)]

#co2lab <- expression(CO^2~Flux)

vars <- c("Treatment", "Bole", "Branch", "Leaf", "Litter", "Root", "Tree C flux")

colnames(tree_table) <- vars


##add sigletters
siglets <- read.csv("Stats/p_sigs/sigletters.csv")

tree_table[[2]] <- paste(tree_table[[2]], siglets[[1]])
tree_table[[3]] <- paste(tree_table[[3]], siglets[[2]])
tree_table[[4]] <- paste(tree_table[[4]], siglets[[3]])
tree_table[[5]] <- paste(tree_table[[5]], siglets[[4]])
tree_table[[6]] <- paste(tree_table[[6]], siglets[[6]])
tree_table[[7]] <- paste(tree_table[[7]], siglets[[6]])

###three columns with P by treatments and interactions
###sigletters by numbers as done previously

# Pinter <- read.csv("Stats/p_sigs/P_interactions.csv")
# Pinter <- round(Pinter, 3)
# 
# Pco2 <- read.csv("Stats/p_sigs/P_co2.csv")
# Pco2 <- round(Pco2, 3)
# Pco2a <- as.character(Pco2)
# 
# Ph2o <- read.csv("Stats/p_sigs/P_h20.csv")
# Ph2o <- round(Ph2o, 3)
# 
# plab1 <- expression(paste(CO[2]),"* Drought (P)", sep=" ")
# 
# plab2 <- expression(paste(CO[2]),"treatment (P)", sep=" ")
# plab3 <- "Drought treatment (P)"
# 
# # P1 <- c(plab1, Pinter[,1])
# P2 <- as.expression(c(plab2, Pco2[,1]))
# P2a <- as.character(P2)
# P3 <- c(plab3, Ph2o[,1])
# 
# 
# tree_table$Treatment <- as.character(tree_table$Treatment)
# 
# tree_table2 <- rbind(tree_table, P3)
# tree_table3 <- rbind(tree_table2, lab3)
# 
# 
# 
# lab <- expression(CO[2],a)
# 
# lab1 <- as.character(lab)
# 
# P="P"
# lab2 <- bquote(paste(CO[2]~treatment~.(P)*","*.(Pco2[1,])*","*.(Pco2[2,])*","*.(Pco2[3,])*","*.(Pco2[4,])*","*.(Pco2[5,])*","*.(Pco2[6,])))
# 
# lab3 <- bquote(CO[2]~treatment~.(P)*","*.(Pco2[1,])*","*.(Pco2[2,])*","*.(Pco2[3,])*","*.(Pco2[4,])*","*.(Pco2[5,])*","*.(Pco2[6,]))


write.csv(tree_table, "master_scripts/data_table.csv", row.names=FALSE)






