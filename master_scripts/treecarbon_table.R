####redo with treatments from harvest and not interpolated data*****

###Data table/stacked bar plot / boxplot of component fractions

table <- read.csv("master_scripts/harvest_trt_means.csv")

##need to split dataframe and combine with pasted mean(se)

table_means <- table[, c(2:3, 6,8:9)]
table_se <- table[, c(10:11, 14, 16:17)]

trts <- table[, 1]

###paste and round means and se together
v1 <- data.frame(paste0(sprintf("%4.1f", round(table_means[,1], 1)), " (", sprintf("%3.1f", round(table_se[,1],1)),")"))
v2 <- data.frame(paste0(sprintf("%4.1f", round(table_means[,2], 1)), " (", sprintf("%3.1f", round(table_se[,2],1)),")"))
v3 <- data.frame(paste0(sprintf("%4.1f", round(table_means[,3], 1)), " (", sprintf("%3.1f", round(table_se[,3],1)),")"))
v4 <- data.frame(paste0(sprintf("%5.1f", round(table_means[,4], 1)), " (", sprintf("%4.1f", round(table_se[,4],1)),")"))
v5 <- data.frame(paste0(sprintf("%5.1f",round(table_means[,5], 1)), " (", sprintf("%4.1f", round(table_se[,5],1)),")"))
# v6 <- data.frame(paste0(sprintf("%4.1f",round(table_means[,6],1)), " (", sprintf("%3.1f",round(table_se[,6],1)),")"))
# v7 <- data.frame(paste0(sprintf("%4.1f",round(table_means[,7], 1)), " (", sprintf("%3.1f",round(table_se[,7],1)),")"))

tree_table <- cbind(trts, v1)
tree_table <- cbind(tree_table, v2)
tree_table <- cbind(tree_table, v3)
tree_table <- cbind(tree_table, v4)
tree_table <- cbind(tree_table, v5)
# tree_table <- cbind(tree_table, v6)
# tree_table <- cbind(tree_table, v7)

##order the variables
tree_table <- tree_table[, c(1:4,6,5)]

#co2lab <- expression(CO^2~Flux)

vars <- c("Treatment", "Bole", "Branch", "Leaf", "Root", "Tree C flux")

colnames(tree_table) <- vars

##Add siglettters eventually

write.csv(tree_table, "master_scripts/data_table.csv", row.names=FALSE)

##make manuscript table-----------------------------------------------------------------------------------------------------
##Here: print to console but then in manuscript use in code chunk to hopefully print in word


library(pixiedust)

tree_table2 <- dust(tree_table)
tree_table2 <- tree_table2 + sprinkle(part="head", bold=TRUE)
tree_table2 <- tree_table2 + sprinkle(part="body", valign="left")

tree_table2 <- tree_table2 + sprinkle(part="head", border="bottom", border_thickness=2, valign="middle")
tree_table2 <- tree_table2 + sprinkle(part="head", border="bottom", border_thickness=2, valign="middle")

tree_table2 <- tree_table2 + sprinkle_colnames

print(tree_table2)






