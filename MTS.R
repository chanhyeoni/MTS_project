# Mahalanobis-Taguchi System module
# Chang Hyun Lee
# created 2014.06.24

# necessary package should be installed
library(ggplot2)
library(data.table)
library(xlsx)
library(MASS)
library(lattice)
require(reshape2)

mean_matrix <- function(dataFrame){
	# obtain the mean meatrix
	mean_data <- apply(dataFrame, 2, mean)
  mean_data <- t(as.matrix(mean_data))
  times <- dim(dataFrame)[1]
	mean_data_mat <- matrix(rep((mean_data), times), ncol = ncol(dataFrame), byrow = TRUE)
	
	return (mean_data_mat)
}

sd_matrix <- function(dataFrame){
	#obtain standard deviation matrix
	sd_data <- apply(dataFrame, 2, sd)
  sd_data <- as.matrix(sd_data)
  times <- dim(dataFrame)[1]
	sd_data_mat <- matrix(rep((sd_data), times), ncol = ncol(dataFrame), byrow = TRUE)
	
	return (sd_data_mat)
}

normalize <- function(normal_data, data){
  # normalize the data
  mean_normal_mat <- mean_matrix(normal_data)
  sd_normal_mat <- sd_matrix(normal_data)
  nrow = (dim(data)[1])
  normalized_data <- (as.matrix(data) - mean_normal_mat[seq(1:nrow), ])/(sd_normal_mat[seq(1:nrow), ]) 
  
  return (normalized_data)
}

mahalanobis_dist <- function(data, corr){
	#calculate the mahalanobis distnace and returns the values diagonally positioned in the matrix
  
  k <- dim(data)[2] # the number of variables
  front <- as.matrix(data)/k
  inv_corr <- ginv(corr)
  
  dist <- front %*% inv_corr %*% t(as.matrix(data))
  group <- diag(dist)
	return (group)
}

plot_result<- function(ref_group, outside_group){
	# plot the data in order to show the classification result
  
  g <- ggplot(data = data.table(ref_group), 
              aes(x = seq(1, length(ref_group)), y = ref_group, colour = "the reference group")) + geom_point()
  g <- g + geom_point(data = data.table(outside_group), 
                      aes(x = seq(1, length(outside_group)), y = outside_group, colour = "the outside group"))
  cb <- c("#0072B2", "#D55E00")
  g <- g + ggtitle("The Mahalanobis Distances for the Referece and the Outside Groups") + 
    xlab("the data") + ylab("MD") + scale_colour_manual(name = "Group", values=cb)
  print(g)
	
}

ortho_array_144 <- as.matrix(read.csv("oa144.csv", header = FALSE))
ortho_array_64 <- as.matrix(read.csv("orthoarray.csv", header = FALSE))

make_ortho_arr<- function(nVariables=dim(ortho_array_64)[2], nRuns=dim(ortho_array_64)[1]){
	# k is the user-defined input (the number of runs),
  # and based upon the number of inputs,
  # the orthogonal array will be generated
  ortho_arr <- ortho_array_64[seq(1,nRuns), seq(1, nVariables)]

	return(ortho_arr)
}



generate_runs <- function(ortho_arr, normal, abnormal) {
	# generates the matrix that has the result of the distances for the samples
	# ortho_arr : the orthogonal array generated from the excel file
  # normal, abnormal : the normal and abnormal variables
  # corr : the correlation matrix
  
  # MUST!!! : the column dimension for the three matrices MUST be the same
	runs_matrices <- matrix(nrow = dim(abnormal)[1], ncol = dim(ortho_arr)[1])
  # starts with the number of columns being the number of runs
	bool_val <-(ortho_arr==1)
	for (run in seq(1,dim(bool_val)[1])){
		# if it is one, it is TRUE; otehrwise, it is false
		bool_run <- bool_val[run, ]
    new_normal <- as.matrix(normal[, bool_run])
		new_abnormal <- as.matrix(abnormal[, bool_run])
    corr <- cor(new_normal, new_normal)
		dist <- t(as.matrix((mahalanobis_dist(new_abnormal, corr))))
		runs_matrices[, run] <- dist
	}
  
	return (runs_matrices)
}


SN_ratio <- function(runs) {
	# generate the signal-to-noise ratio
	nRow= dim(runs)[1]
	mat = 1/(runs*runs)
	mat = colSums(mat)
	mat = mat/nRow
	mat = -10 * log10(mat)
	#return (t(mat))
	return (t(t(mat)))
}


create_use_dont_use_matrix<- function(ortho_arr, sn_ratio){
	# takes a unhtalthy matrices and s/n ratio to make the use / don't use matrices
	
	# replicate the number of SN ratio such that it has the same dimension
	# as the unhealthy matrices
	nCol <- dim(ortho_arr)[2]
	sn_ratio <- matrix(rep(sn_ratio,nCol), ncol = nCol)

	# multiply unhealthy matrices and s/n ratio
	result <- ortho_arr * sn_ratio
	
	# average for each variable
	mat <- colMeans(result)
	return ((mat))
}


avr_SN_ratio<- function(runs_matrices, ortho_arr, var_names){
	# compute the average value of signal-to-noise ratio 
	# depending on the value of taguchi arrays
	
	# call the SN_ratio function
	sn_ratio <- SN_ratio(runs_matrices)
	
	##### use_matrix computation #####
	# change the values of 2 into 0
	ortho_arr[ortho_arr==2] = 0
	# return the use matrix
	use_matrix <- create_use_dont_use_matrix(ortho_arr, sn_ratio)
  
	##### don't_use_matrix computation #####
	# change the values of 1 into 2
	ortho_arr[ortho_arr == 1] = 2
	# change the values of 0 into 1
	ortho_arr[ortho_arr == 0] = 1
	# change the values of 2 into 0
	ortho_arr[ortho_arr == 2] = 0
  
	dont_use_matrix <- create_use_dont_use_matrix(ortho_arr, sn_ratio)
	
	# combine the matrix
	avr_sn_ratio <- data.frame(ON = use_matrix, OFF = dont_use_matrix, row.names = var_names)
	avr_sn_ratio$delta <- (use_matrix - dont_use_matrix)
	
	return (avr_sn_ratio)
}


graph_SN_ratio <- function(avr_sn_ratio){
 sn_ratio_ordered <- avr_sn_ratio[order(-avr_sn_ratio$delta), ]
 p <- qplot(x = rownames(sn_ratio_ordered), y = sn_ratio_ordered$delta, data = sn_ratio_ordered, 
            main = "the Signal-to-Noise", xlab = "variables", ylab = "delta")
 print (p)
  #p <- geom_rect()
}

show_deltas <- function(avr_sn_ratio){
  # shows the deltas along with the variable names as well as
  # asking for the number of variables to select
  ratio_ordered <- avr_sn_ratio[order(-avr_sn_ratio$delta), ]
  things_to_print <- data.frame(variables = rownames(ratio_ordered), delta = ratio_ordered$delta)
  print (things_to_print)
  nVariables <- as.numeric(readline(prompt = "how many variables do you want to select?: "))
  return (list(ratio_ordered, nVariables))
}

dim_reduction<- function(data, ratio_ordered, nVariables){
	# carry out the dimensionality reduction and return the matrix with different variables 
  selected_vars <- rownames(ratio_ordered[seq(1,nVariables), ])
  print (selected_vars)
	
	return (data[, selected_vars])
}

MTS <- function(normal, abnormal){
  # normal and abnormal : the datasets that do not include the labels

  ################ MAHALANOBIS DISTANCE ################ 
  # find the correlations
  corr <- cor(normal, normal)
  
  # estimate the mahalanobis distances
  ref_group <- mahalanobis_dist(normal, corr)
  outside_group <- mahalanobis_dist(abnormal, corr)
  outside_group <- outside_group[-which(outside_group == max(outside_group))]
  
  # plot the result
  plot_result(ref_group, outside_group)
  readline(prompt = "Hit Enter to contiue ")
     
  ################ TAGUCHI ARRAY ################
  # make an orthogonal array
  nVariables <- as.numeric(readline(prompt = "how many variables?: "))
  nRuns <- as.numeric(readline(prompt = "how many runs?: "))
  ortho_arr <- make_ortho_arr(nVariables, nRuns)
  nCols <- seq(1, dim(ortho_arr)[2])
  var_names <- colnames(normal[, nCols])
  
  # comput the nosie-to-signal ratio
  runs <- generate_runs(ortho_arr, normal[, nCols], abnormal[, nCols])
  avr_sn_ratio <- avr_SN_ratio(runs, ortho_arr, var_names)
  graph_SN_ratio(avr_sn_ratio)
  readline(prompt = "Hit Enter to contiue ")
  
  # make the dimensionality reduction and generate new normal and abnormal data
  ls <- show_deltas(avr_sn_ratio)
  normal <- dim_reduction(normal, ls[[1]],ls[[2]])
  abnormal <- dim_reduction(abnormal, ls[[1]],ls[[2]])
  
  ################ MAHALANOBIS DISTANCE (the second) ################ 
  # find the correlations
  corr <- cor(normal, normal)
  
  # estimate the mahalanobis distances
  ref_group <- mahalanobis_dist(normal, corr)
  outside_group <- mahalanobis_dist(abnormal, corr)
  outside_group <- outside_group[-which(outside_group == max(outside_group))]
  
  # plot the result
  plot_result(ref_group, outside_group)
  
  return (list(normal, abnormal))
}