# client-side version of the MTS applications
# Chang Hyun Lee

library(shiny)
source("MTS.R")

#ortho_array <- as.matrix(read.csv("orthoarray.csv"))

abstract_sidepanel <- sidebarPanel(
  h2("Choose The Dataset"),
  helpText("Please choose the dataset you would like to work on"),
  fileInput(
    'dataset', 'Choose file to upload', 
    accept = c('text/csv','text/comma-separated-values','text/tab-separated-values',
               'text/plain','.csv','.tsv')
    )
)
abstract_mainpanel <- mainPanel(
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
  h6("The Dataset", align = "center"),
  tableOutput("data")
)

mts_first_sidepanel <- sidebarPanel(h6("The 1st Run of MD", align = "center"))
mts_first_mainpanel <- mainPanel(plotOutput("mts_first"))

sn_ratio_sidepanel <- sidebarPanel(
  h6("The Signal-to-Ratio", align = "center"),
  helpText("The chart to the right is the signal-to-noise ratios"),
  helpText("Please choose the number of variables to select"),
  numericInput("nVariables", label = 'the number of variables to select', value = 2)
)
sn_ratio_mainpanel <- mainPanel(tableOutput("sn_ratio"))

mts_second_sidepanel <- sidebarPanel(h6("The 2nd Run of MD", align = "center"))
mts_second_mainpanel <- mainPanel(plotOutput("mts_second"))

abstract_tabpanel <- tabPanel("Home", sidebarLayout(abstract_sidepanel,abstract_mainpanel))
mts_first_tabpanel <- tabPanel("The 1st MD", sidebarLayout(mts_first_sidepanel, mts_first_mainpanel))
sn_ratio_tabpanel <- tabPanel("The Signal-to-Ratio", sidebarLayout(sn_ratio_sidepanel, sn_ratio_mainpanel))
mts_second_tabpanel <- tabPanel("The 2nd MD", sidebarLayout(mts_second_sidepanel, mts_second_mainpanel))

                              
shinyUI(
  navbarPage("The Mahalanobis-Taguchi System", 
             abstract_tabpanel,
             mts_first_tabpanel,
             sn_ratio_tabpanel,
             mts_second_tabpanel)
)
