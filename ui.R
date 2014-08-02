# client-side version of the MTS applications
# Chang Hyun Lee

library(shiny)
source("MTS.R")

ortho_array <- as.matrix(read.csv("orthoarray.csv"))

shinyUI(pageWithSidebar(
  # MAKE SURE YOU ALWAYS HAVE A COMMMA (,)
  # headerPanel
  headerPanel("The Mahalanobis-Taguchi Strategy"),
  titlePanel("The MTS and its Applications"),
  
  # side bar
  sidebarLayout(position = "right",
    sidebarPanel(
      h2("Choose The Dataset"),
      helpText("Please choose the dataset you would like to work on"),
      actionButton("browse", "Browse"),
      fileInput("normal_group", label = "The normal group dataset",
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      )
    ),
    mainPanel(
      h4("Abstract"),
      p("The Mahalanobis-Taguchi System (MTS) is a statistical and forecasting method 
        that classifies data into normal and abnormal groups. Given a multi-dimensional 
        dataset, the MTS first estimates the Mahalanobis distances between the scaled 
        normal data and the mean of the normal data, as well as the distances between 
        the abnormal and the normal mean. Then, the Taguchi technique is employed to 
        calculate the average signal-to-noise ratio of each variable or feature and to 
        carry out feature selections. Next, the Mahalanobis distance is estimated again, 
        but using the selected features. The final analysis delivers a more interpretable 
        result explaining the ‘important’ features that contribute to the classification."),
      h6("the normal group", align = "center"),
      tableOutput("normal_group"),
      h6("the abnormal group", align = "center"),
      tableOutput("abnormal_group"),
      plotOutput("mahalanobis_plots_1"),
      plotOutput("mahalanobis_plots_1")
    )
    
  )
)
)