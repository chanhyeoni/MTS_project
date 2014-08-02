# server-side script of MTS application

# get the working directory right
# setwd("./Google Drive/MTS/MTS project")

# INCLUDE THE SOURCE CODE
library(shiny)
source("MTS.R")





################ TAGUCHI ARRAY ################
# make an orthogonal array
nVariables <- as.numeric(readline(prompt = "how many variables?: "))
nRuns <- as.numeric(readline(prompt = "how many runs?: "))
ortho_arr <- make_ortho_arr(nVariables, nRuns)
nCols <- seq(1, dim(ortho_arr)[2])
var_names <- colnames(normal[, nCols])

# compute the nosie-to-signal ratio
runs <- generate_runs(ortho_arr, normal[, nCols], abnormal[, nCols])
avr_sn_ratio <- avr_SN_ratio(runs, ortho_arr, var_names)
graph_SN_ratio(avr_sn_ratio)

# make the dimensionality reduction and generate new normal and abnormal data
ls <- show_deltas(avr_sn_ratio)
normal <- dim_reduction(normal, ls[[1]],ls[[2]])
abnormal <- dim_reduction(abnormal, ls[[1]],ls[[2]])


################ MAHALANOBIS DISTANCE (the second) ################ 
# find the correlations
corr <- cor(normal, normal)

# estimate the mahalanobis distances
ref_group_2 <- mahalanobis_dist(normal, corr)
outside_group_2 <- mahalanobis_dist(abnormal, corr)
outside_group_2 <- outside_group[-which(outside_group == max(outside_group))]


############################################## MTS ##############################################

shinyServer(function(input, output) {
  # set the working directory
  setwd("/Google Drive/MTS/MTS project")
  
  
  dInput = reactive({
    in.file = input$file1
    
    if (is.null(in.file))
      return(NULL)
    
    if (input$rownames) {
      read.table(in.file$datapath, header=input$header, sep=input$sep,
                 quote=input$quote, row.names=1, dec=input$dec)
    } else {
      read.table(in.file$datapath, header=input$header, sep=input$sep,
                 quote=input$quote, dec=input$dec)
    }
  }
  )
  
  output$normal_group <- renderTable({
    d.input= dInput()
    if(is.null(d.input)){
      return (NULL)
    }
    head(dInput())
  })
  #output$abnormal_group <- renderTable({
  #  vw_abnormal <- read.csv(input$abnormal_group)
  #  vw_abnormal <- vw_abnormal[, !(colnames(vw_abnormal) %in% c("X", "id"))]
  #  head(vw_abnormal)
  #}
  #)
  
  ## plot the mahalanobis distances
  #output$mahalanobis_plots_1 <- renderPlot({
  #  # pre-process the data
    
  #  columns <- colnames(vw_normal)
  #  columns_except_target <- columns[!(columns %in% c("target"))]
    
    
    
    ################ MAHALANOBIS DISTANCE ################ 
    # normal and abnormal : the datasets that do not include the labels
    # normalize the dataset
  #  normal <- normalize(vw_normal[, columns_except_target], vw_normal[, columns_except_target])
  #  abnormal <- normalize(vw_normal[, columns_except_target], vw_abnormal[, columns_except_target])
    
    # find the correlations
  #  corr <- cor(normal, normal)
    
    # estimate the mahalanobis distances
  #  ref_group_1 <- mahalanobis_dist(normal, corr)
  #  outside_group_1 <- mahalanobis_dist(abnormal, corr)
    
    # plot the result
  # plot_result(ref_group_1, outside_group_1)
  #  }
  #  )
  
  #nVariables <- as.numeric(readline(prompt = "how many variables?: "))
  #nRuns <- as.numeric(readline(prompt = "how many runs?: "))
  
  # graph the average signal-to-noise ratios
  #output$avr_sn_ratio <- renderPlot(graph_SN_ratio(avr_sn_ratio))
  
  # graph the second mahalanobis distances
  #output$mahalanobis_plots_2 <- renderPlot(plot_result(ref_group_2, outside_group_2))
  

    
  }
)