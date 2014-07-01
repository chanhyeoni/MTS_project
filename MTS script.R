# Changhyun Lee
# Mahalanobis-Taguchi System sample code for medical diagnosis
# 2014.06.24

getwd()
setwd("./Desktop/MTS/MTS sample for health")




normalize_mean <- function(dataFrame, k){
	# obtain the mean meatrix
	mean_data <- apply(dataFrame, 2, mean)
	mean_data_mat <- matrix(mean_data, nrow = k, ncol = dim(dataFrame)[1] )
	mean_data_mat <- t(mean_data_mat)
	
	return (mean_data_mat)
}

normalize_sd <- function(dataFrame, k){
	#obtain standard deviation matrix
	sd_data <- apply(dataFrame, 2, sd)
	sd_data_mat <- matrix(sd_data, nrow = k, ncol = dim(dataFrame)[1]  )
	sd_data_mat <- t(sd_data_mat)
	
	return (sd_data_mat)
}


mahalanobis_distance <- function(k, data, inv_corr){
	#calculate the mahalanobis distnace
	
	# divide the values by k (do this AFTER finding the correlation matrix?????)
	front <- data/k
	
	
	# change the data.frame into matrix framework
	data <- as.matrix(data)
	inv_corr <- as.matrix(inv_corr)
	front <- as.matrix(front)
	
	# compute the distance	
	distances <- (front %*% inv_corr) %*% t(data)
	
	# obtain the diagonals
	group <- diag(distances)
			
	return (group)
}


function <- plot_result(ref_group, outside_group){
	# plot the data in order to show the classification result
	ref_group_data <- data.frame(distances = ref_group, label = "reference")
	outside_group_data <- data.frame(distances = outside_group, label = "outside")
	dataset <- rbind(ref_group_data, outside_group_data)	
	nData <- 1:dim(dataset)[1]
	
	dataset <- data.table(dataset)
	dataset_grouped <- dataset[, , by = dataset$label]
	
	g <- ggplot(data = dataset, aes(x = nData, y = distances, color = label))
	g + geom_point() + geom_line() + ggtitle("the result")	
	
}





k = 4

# read the data
library(xlsx)
healthy<- read.xlsx("healthy_group.xlsx", sheetIndex = 1, header = 1)
unhealthy <- read.xlsx("abnormal_group.xlsx", sheetIndex = 1, header = 1)
# need to put header as 1 because it reads the first row of the excel as the attributes

healthy <- healthy[, 2:length(colnames(healthy))]
unhealthy <- unhealthy[, 2:length(colnames(unhealthy))]


#normalize the data
mean_healthy_mat <- normalize_mean(healthy, k)
sd_healthy_mat <- normalize_sd(healthy, k)
	
healthy <- (healthy - mean_healthy_mat)/sd_healthy_mat
row_unhealthy <- dim(unhealthy)[1]
unhealthy <- (unhealthy - (mean_healthy_mat[seq(1:row_unhealthy), ]) )/(sd_healthy_mat[seq(1:row_unhealthy), ])

# find the inverse of correlation matrix for the healthy group
corr <- cor(healthy, healthy)
inv_corr <- solve(corr)

# estimate the distances and obtain the diagonal parts of the matrix
ref_group <- mahalanobis_distance(k, healthy, inv_corr)
outside_group <- mahalanobis_distance(k, unhealthy, inv_corr)

# plot the result
install.packages("ggplot2")
library(ggplot2)
plot_result(ref_group, outside_group)



























