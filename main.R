# MTS main function
# Chang Hyun Lee
# created 2014.07.09

# set the working directory
setwd("./Google Drive/MTS/MTS project")

# import all the necessary modules
source("MTS.R")

############# Liberty Mutual Fire Peril Loss Cost  #############
#dataset_1 <- read.csv("liberty_mutual_train.csv", nrow = 100000)
#dataset_1[(is.na(dataset_1))] <- 0
#write.csv(dataset_1, "dataset_1.csv")
#dataset_2 <- read.csv("liberty_mutual_train.csv", skip = 100000, nrow = 100000, col.names = columns)
#write.csv(dataset_2, "dataset_2.csv", row.names = FALSE)
#dataset_3 <- read.csv("liberty_mutual_train.csv", skip = 200000, nrow = 100000,  col.names = columns)
#write.csv(dataset_3, "dataset_3.csv", row.names = FALSE)
#dataset_4 <- read.csv("liberty_mutual_train.csv", skip = 300000, nrow = 100000,  col.names = columns)
#write.csv(dataset_4, "dataset_4.csv", row.names = FALSE)
#dataset_5 <- read.csv("liberty_mutual_train.csv", skip = 400000,  col.names = columns)
#write.csv(dataset_5, "dataset_5.csv", row.names = FALSE)

# load the dataset
dataset <- read.csv("dataset_1.csv")
columns <- colnames(dataset)

# as a result of data analysis, it was found that the set of policy variables
# should not be used in this experiment, since they have categorical values
# instead of numerical ones. Turning them into numeric values and estimating
# mahalanobis distance does not translate in the same way the original values
# of the variables represent
# so drop the var columns of the dataset
# also, when looking at the dataset, the set of crime variables have lots of NAs,
# and including them will also affect the results
dataset <- dataset[, -grep("var", columns)]

# remove the records with the missing values. Filling the values with zero or
# any non-zero values may also put bias in the original data. Because it is missing
# it is unknown how far the values would be from the mean of the data.
dataset <- na.omit(dataset)

# drop the unnessary columns and retain them as separate vectors
# id, target, and dummy variables should be separated, since they are not supposed to
# be the dataset for calcualting mahalanobis distances
id <- dataset[, "id"]
labels <- dataset[, "target"]
dataset <- dataset[, !(colnames(dataset) %in% c("id", "target", "dummy"))]

# for the purpose of this research, we are going to label the labels 
# with the value greater than 1 as 1, and the value less than 1 as 0
# (the record whose ratio is greater than or equal to 1 is the risky record)
labels[labels >= 1] = 1
labels[labels < 1 ] = 0

# separate them from positive and negative examples
abnormal<- dataset[labels >= 1, ]
normal <- dataset[labels < 1, ]


############################## Prof. Van Wagenen's Dataset ##############################
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

dataset <- MTS(vw_normal[, vars_to_test], vw_abnormal[, vars_to_test])


new_normal <- dataset[[1]]
new_abnormal <- dataset[[2]]

# after running another set of variables, do
new_normal <- cbind(new_normal, dataset[[1]])
new_abnormal <- cbind(new_abnormal, dataset[[2]])





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