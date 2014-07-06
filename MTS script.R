# Changhyun Lee
# Mahalanobis-Taguchi System sample code for medical diagnosis
# 2014.06.24

getwd()
setwd("./Desktop/MTS/MTS sample for health")

# necessary package should be installed
install.packages("ggplot2")
library(ggplot2)

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
	nData_ref <- 1:dim(ref_group_data)[1]
	nData_outside <- 1:dim(outside_group_data)[1]
	
	dataset <- data.table(dataset)
	dataset_grouped <- dataset[, , by = dataset$label]
	
	with(dataset, plot(x = nData_ref, y = ref_group_data$distances, type = 'l', col = 'red'), type = 'n')
	with(dataset, points(x = nData_outside, y = outside_group_data$distances, type = 'l', col = 'blue'), type = 'n')
	
	
	g <- ggplot(data = dataset, aes(x = nData, y = distances, color = label))
	g + geom_point() + geom_line() + ggtitle("the result")	
	
}



function <- orth_array(k, ortho_array_mat){
	# generate the orthogonal array based upon the value of the k    
	# here, I will try to use the system that generates k+1 rows

	print ''' but I need to REALLY figure out how to do this thing'''
	arr <- ortho_array_mat[seq(1,k+1), seq(1, k)]
	
	return(arr)
	
}


# 아직도 해야 할 일
# generate the number of rungs depending on the k-values
# generate the number of runs depending on the number of rows in the array
function <- generate_runs(k, ortho_array, unhealthy) {
	# k : int
	# ortho_array, unhealthy : matrix
	
	row_ortho_array <- dim(ortho_aray)[1]
	for (i in row_ortho_array){
		# if it is one, it is TRUE; otehrwise, it is false
		one_or_two <-ortho_array[i,(ortho_array[i, ]==1)]			
	}

}


function <- SN_ratio(unhealthy_matrices) {
	# generate the signal-to-noise ratio
	nRow= dim(unhealthy_matrices)[1]
	mat = 1/(unhealthy_matrices*unhealthy_matrices)
	mat = colSums(mat)
	mat = mat/nRow
	mat = -10 * log10(mat)
	return(t(mat))
}

function <- create_use_dont_use_matrix(unhealthy_matrices, sn_ratio){
	# takes a unhtalthy matrices and s/n ratio to make the use / don't use matrices
	
	# replicate the number of SN ratio such that it has the same dimension
	# as the unhealthy matrices
	nCol= dim(unhealthy_matrices)[2]
	sn_ratio <- matrix(rep(sn_ratio,nCol), ncol = nCol)
	
	# multiply unhealthy matrices and s/n ratio
	result <- unhealthy_matrices * sn_ratio
	
	# average for each variable
	mat <- colMeans(result)
	
	return t(mat)
}


function <- avr_SN_ratio(unhealthy_matrices){
	# compute the average value of signal-to-noise ratio 
	# depending on the value of taguchi arrays
	
	# call the SN_ratio function
	sn_ratio <- SN_ratio(unhealthy_matrics)
	
	##### use_matrix computation #####
	# change the values of 2 into 0
	unhealthy_matrices[unhealthy_matrices==2] = 0
	# return the use matrix
	use_matrix <- create_use_dont_use_matrix(unhealthy_matrices, sn_ratio)
	
	##### don't_use_matrix computation #####
	# change the values of 1 into 2
	unhealthy_matrices[unhealthy_matrices == 1] = 2
	# change the values of 0 into 1
	unhealthy_matrices[unhealthy_matrices == 0] = 1
	# change the values of 2 into 0
	unhealthy_matrices[unhealthy_matrices == 2] = 0
	
	dont_use_matrix <- create_use_dont_use_matrix(unhealthy_matrices, sn_ratio)
	
	# combine the matrix
	on_off_matrix <- data.frame(ON = use_matrix, OFF = dont_use_matrix)
	on_off_matrix$delta <- (use_matrix - dont_use_matrix)
	
	return (on_off_matrix)
}


function <- dim_reduction(on_off_matrix, threshold, var_names){
	# carry out the dimensionality reduction and return the variables that are 
	# selected
	print ''' HOW DO WE GO ABOUT DOING THIS??'''
	print ''' extracting k important features, or putting the threshold values??'''
	index <- rownames(on_off_matrix[which(on_off_matrix$delta > threshold), ])
	
	return (var_names[index])
	
	
}


function <- mts(healthy, unhealthy, k=length(colnames(healthy)), threshold){
	# calling all the functions above in order to run MTS system
	
	# obtain the variable names
	var_names <- colnames(healthy)
	
	# generate otrhogonal array
	ortho_array <- read.table("orthoarray.csv", sep = ",", header = FALSE)
    ortho_array_mat <- as.matrix(ortho_array)
    
    
    ortho_array <- ortho_array(k, ortho_array_mat)
	# normalize the data
	mean_healthy_mat <- normalize_mean(healthy, k)
	sd_healthy_mat <- normalize_sd(healthy, k)
	healthy <- (healthy - mean_healthy_mat)/sd_healthy_mat
	row_unhealthy <- dim(unhealthy)[1]
	unhealthy <- (unhealthy - (mean_healthy_mat[seq(1:row_unhealthy), ])/(sd_healthy_mat[seq(1:row_unhealthy), ])
	
	# generate the runs
	runs <- generate_runs(k, ortho_array, unhealthy)

	# find the inverse of correlation matrix for the healthy group
	corr <- cor(healthy, healthy)
	inv_corr <- solve(corr)
	
	# estimate the distances and obtain the diagonal parts of the matrix
	ref_group <- mahalanobis_distance(k, healthy, inv_corr)
	outside_group <- mahalanobis_distance(k, unhealthy, inv_corr)
	
	# plot the result
	plot_result(ref_group, outside_group)
	
	# generate all the mahalanobis distances for the unhealthy group
	unhealthy_matrices <- generate_mh_distances_for_unhealthy(runs, unhealthy)
	
	# compute the average S/N ratios and generate the on&off matrix
	on_off_matrix <- avr_SN_ratio(sn_ratio)
	
	# start the dimensionality reduction
	variables <- dim_reduction(on_off_matrix, threshold, var_names)
	
	# start with the new k number
	k = length(variables)
	
	
}



# read the data
library(xlsx)
healthy<- read.xlsx("healthy_group.xlsx", sheetIndex = 1, header = 1)
unhealthy <- read.xlsx("abnormal_group.xlsx", sheetIndex = 1, header = 1)
# need to put header as 1 because it reads the first row of the excel as the attributes

# extract the needed data
healthy <- healthy[, 2:length(colnames(healthy))]
unhealthy <- unhealthy[, 2:length(colnames(unhealthy))]

# run the MTS
threshold = 10
mts(healthy, unhealthy, threshold)





























