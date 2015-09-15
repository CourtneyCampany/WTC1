####redo with treatments from harvest and not interpolated data*****
library(doBy)
source("functions_and_packages/functions.R")

tree_C <- read.csv("calculated_mass/chamber_carbon.csv")
tree_C$treatments <- with(tree_C, interaction(CO2_treatment, Water_treatment))

treeC_agg <- summaryBy(.~treatments, data=tree_C, FUN=c(mean,se))

####START HERE to double check new data


###Data table/stacked bar plot / boxplot of component fractions

#table <- read.csv("master_scripts/harvest_trt_means.csv")


##need to split dataframe and combine with pasted mean(se)

table_means <- treeC_agg[, c(2:9)]
table_se <- treeC_agg[, c(10:17)]

trts <- treeC_agg[, 1]
trts2 <- c("aCO2-dry", "aCO2-wet", "eCO2-dry", "eCO2-wet")

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

###three columns with P by treatments and interactions
###sigletters by numbers as done previously

##Add siglettters eventually

# write.csv(tree_table, "master_scripts/data_table.csv", row.names=FALSE)







