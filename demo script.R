# MTS main function
# Chang Hyun Lee
# created 2014.07.09

# set the working directory
setwd("./Google Drive/MTS/MTS project")

# import all the necessary modules
source("MTS.R")

vw_normal <- read.csv("LM_Healthy_Labels_1.csv")
vw_normal <- vw_normal[, !(colnames(vw_normal) %in% c("X", "id"))]
vw_abnormal <- read.csv("LM_Sickbigger1_Labels_1.csv")
vw_abnormal <- vw_abnormal[, !(colnames(vw_abnormal) %in% c("X", "id"))]

columns <- colnames(vw_normal)
columns_except_target <- columns[!(columns %in% c("target"))]

# normalize the dataset
vw_normal[, columns_except_target] <- normalize(vw_normal[, columns_except_target] , vw_normal[, columns_except_target] )
vw_abnormal[, columns_except_target] <- normalize(vw_normal[, columns_except_target], vw_abnormal[, columns_except_target])

vars <- columns_except_target[grep("var", columns_except_target)]
numeric_vars <- vars[-which(vars == "var7" | vars == "var8")]
geodem_vars <- columns_except_target[grep("geo", columns_except_target)]
weather_vars <- columns_except_target[grep("weather", columns_except_target)]

vars_to_test <- append(append(numeric_vars,geodem_vars), weather_vars[seq(1,22)])
vars_to_test <- weather_vars[seq(23,85)]
vars_to_test <- weather_vars[seq(86, 149)]
vars_to_test <- weather_vars[seq(150,length(weather_vars))]

MTS(vw_normal[, vars_to_test], vw_abnormal[, vars_to_test])


# data visualization
# it starts with 2, since vw_data[1,] is target
vw_normal[, columns_except_target] <- normalize(vw_normal[, columns_except_target], vw_normal[, columns_except_target])
vw_abnormal[, columns_except_target] <- normalize(vw_normal[, columns_except_target], vw_abnormal[, columns_except_target])
vw_data <- rbind(vw_normal, vw_abnormal)

start_col = 6
nVar_col = 4
end_col = start_col+nVar_col
splom(~vw_data[start_col:end_col], groups = target, data = vw_data, col = c("red", "blue") )
legend("topright",pch =1, col = c("red", "blue"), legend=c("1", "Other months"))

# compare that with logistic regression
vw_data <- rbind(vw_normal, vw_abnormal)
vw_labels <- vw_data[, 'target']
vw_data <- vw_data[,!(colnames(vm_data) %in% c("target"))]
aModel <- glm.fit(vw_data[,c("var10", "var11", "var13", "var17")], vm_labels, iter = 100)