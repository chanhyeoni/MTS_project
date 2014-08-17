# The application of the MTS
# the demo script for testing the MTS on the Liberty Mutual Peril Cost dataset
# Chang Hyun Lee
# created 2014.07.09


# import all the necessary modules
source("MTS.R")

# read the dataset
setwd("..")
dataset <- read.csv("./data/liberty_mutual.csv")
vw_normal <- dataset[dataset$target ==0, ]
vw_abnormal <- dataset[dataset$target ==1, ]
vw_normal <- vw_normal[, !(colnames(vw_normal) %in% c("X", "id"))]
vw_abnormal <- vw_abnormal[, !(colnames(vw_abnormal) %in% c("X", "id"))]

# get the necessary subsets of the variables
columns <- colnames(vw_normal)
columns_except_target <- columns[!(columns %in% c("target"))]
vars <- columns_except_target[grep("var", columns_except_target)]
numeric_vars <- vars[-which(vars == "var7" | vars == "var8")]
geodem_vars <- columns_except_target[grep("geo", columns_except_target)]
weather_vars <- columns_except_target[grep("weather", columns_except_target)]

# normalize the dataset
vw_normal[, columns_except_target] <- normalize(vw_normal[, columns_except_target] , vw_normal[, columns_except_target] )
vw_abnormal[, columns_except_target] <- normalize(vw_normal[, columns_except_target], vw_abnormal[, columns_except_target])

# variables to test upon
vars_to_test <- append(append(numeric_vars,geodem_vars), weather_vars)

# read the orthogonal array file
orthoarray_filename <- "L256.csv"

# test the dataset on the MTS
MTS(vw_normal[, vars_to_test], vw_abnormal[, vars_to_test], orthoarray_filename)