# server-side script of MTS application

# get the working directory right
# setwd("./Google Drive/MTS/MTS project")

# INCLUDE THE SOURCE CODE
library(shiny)
source("MTS.R")

ls_datasets <- function(dataset){
  # splits the dataset into normal and abnormal parts
  # split the dataset into normal and abnormal
  normal <- dataset[dataset$target == 1, ]
  abnormal <- dataset[dataset$target==0, ]
  # remove the unncessary columns
  normal <- normal[, !(colnames(normal) %in% c("X", "id"))]
  abnormal <- abnormal[, !(colnames(abnormal) %in% c("X", "id"))]
  
  # get the necessary subsets of the variables
  columns <- colnames(normal)
  # exclude the target variables
  columns_except_target <- columns[!(columns %in% c("target"))]
  
  # normalize the dataset
  normal[, columns_except_target] <- normalize(normal[, columns_except_target] , normal[, columns_except_target] )
  abnormal[, columns_except_target] <- normalize(normal[, columns_except_target], abnormal[, columns_except_target])
  
  # create variables selection options
  vars <- columns_except_target[grep("var", columns_except_target)]
  numeric_vars <- vars[-which(vars == "var7" | vars == "var8")]
  geodem_vars <- columns_except_target[grep("geo", columns_except_target)]
  weather_vars <- columns_except_target[grep("weather", columns_except_target)]
  
  # variables to test upon
  vars_to_test <- append(append(numeric_vars,geodem_vars), weather_vars)
  normal <- normal[, vars_to_test]
  abnormal <- abnormal[, vars_to_test]
  
  return (list('normal'= normal, 'abnormal'= abnormal))
}

mahalanobis_process <- function(normal, abnormal){
  # find the correlations
  corr <- cor(normal, normal)
  
  # estimate the mahalanobis distances
  ref_group <- mahalanobis_dist(normal, corr)
  outside_group <- mahalanobis_dist(abnormal, corr)
  outside_group <- outside_group[-which(outside_group == max(outside_group))]
  
  return (list('ref'=ref_group, 'outside'=outside_group))
}

readFile <- function(inFile){
  if(is.null(inFile)){
    return (NULL)
  }
  dataset <- read.csv(inFile$datapath)
  return(dataset)
}

show_sn_ratio <- function(ortho_filename, normal, abnormal){
  nVariables <- dim(abnormal)[2]
  ortho_arr <- make_ortho_arr(ortho_filename, nVariables)
  nCols <- seq(1, dim(ortho_arr)[2])
  var_names <- colnames(normal[, nCols])
  
  # comput the nosie-to-signal ratio
  runs <- generate_runs(ortho_arr, normal[, nCols], abnormal[, nCols])
  avr_sn_ratio <- avr_SN_ratio(runs, ortho_arr, var_names)
  return (get_ordred_sn_ratio(avr_sn_ratio))
}

shinyServer(function(input, output, session) {
  get_dataset <- reactive({readFile(input$dataset)})
  nSelectedVariables <- reactive({(input$nVariables)})

  output$data <- renderTable({
    dataset <- get_dataset()
    dataset[seq(1,20), ]
    }
  )
  
  output$mts_first <- renderPlot({
    dataset <- get_dataset()
    ls_data <- ls_datasets(dataset)
    normal <- ls_data[[1]]
    abnormal <- ls_data[[2]]
    
    ls_MD_groups <- mahalanobis_process(normal, abnormal)
    ref_group <- ls_MD_groups[[1]]
    outside_group <- ls_MD_groups[[2]]
    plot_result(ref_group, outside_group)
    
  })
  
  output$sn_ratio <- renderTable({
    dataset <- get_dataset()
    ls_data <- ls_datasets(dataset)
    normal <- ls_data[[1]]
    abnormal <- ls_data[[2]]
    
    ortho_filename <- "./data/L256.csv"
    show_sn_ratio(ortho_filename, normal, abnormal)
  })
  
  output$mts_second <- renderPlot({
    dataset <- get_dataset()
    ls_data <- ls_datasets(dataset)
    normal <- ls_data[[1]]
    abnormal <- ls_data[[2]]
    
    ortho_filename <- "./data/L256.csv"
    ratio_ordered<-show_sn_ratio(ortho_filename, normal, abnormal)
    
    normal <- dim_reduction(normal, ratio_ordered,nSelectedVariables)
    abnormal <- dim_reduction(abnormal, ratio_ordered,nSelectedVariables)
    
    ls_MD_groups <- mahalanobis_process(normal, abnormal)
    ref_group <- ls_MD_groups[[1]]
    outside_group <- ls_MD_groups[[2]]
    
    
    plot_result(ref_group, outside_group)
    
  })
  
  
 
  }
)