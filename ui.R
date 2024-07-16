library(shiny)
library(plotly)

# Define the user interface
shinyUI(fluidPage(
  includeCSS("styles.css"),
  titlePanel("State Rankings by Category"),
  
  # Existing section for State Rankings by Category
  fluidRow(
    column(4, 
           sidebarPanel(
             selectInput("metric", "Metric:", choices = c("Overall Score", 
                                                          "Disbursement", 
                                                          "Fatalities",
                                                          "Bridges",
                                                          "Congestion Hours",
                                                          "Pavement Roughness")),
             uiOutput("submetric_ui")
           )
    ),
    column(8, 
           mainPanel( 
             plotlyOutput("interactivePlot")
           )
    )
  ),
  
  # New section for Map
  hr(),  # Add a horizontal line to separate sections
  fluidRow(
    column(4,
           sidebarPanel(
             selectInput("map_metric", "Map Metric:", choices = c("Overall Score", 
                                                                  "Bridges",
                                                                  "Congestion Hours",
                                                                  "Rural Fatalities",
                                                                  "Urban Fatalities",
                                                                  "Other Fatalities",
                                                                  "Rural Opa Pavement Roughness",
                                                                  "Rural Interstate Pavement Roughness",
                                                                  "Urban Opa Pavement Roughness",
                                                                  "Urban Interstate Pavement Roughness",
                                                                  "Admin Disbursement",
                                                                  "Capital Disbursement",
                                                                  "Maintenance Disbursement",
                                                                  "Other Disbursement"))
           )
    ),
    column(8,
           mainPanel(
             plotOutput("mapPlot")
           )
    )
  )
))
