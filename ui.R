library(shiny)
library(plotly)

# Define the user interface
shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),  
  # Add the navbar
  tags$nav(
    class = "navbar navbar-default navbar-custom",
    tags$div(class = "container-fluid",
             tags$div(class = "navbar-left",
                      tags$a(href = "#", tags$img(src = "logo.png", height = "40px"))
             ),
             tags$div(class = "navbar-title", "")
    )
  ),
  
  # State Rankings by Category
  fluidRow(
    column(12,  # Use 12 columns for full width
           h3("State Rankings by Category"),
           sidebarLayout(
             sidebarPanel(
               selectInput("metric", "Metric:", choices = c("Overall Score", 
                                                            "Disbursement", 
                                                            "Fatalities",
                                                            "Bridges",
                                                            "Congestion Hours",
                                                            "Pavement Roughness")),
               uiOutput("submetric_ui")
             ),
             mainPanel(
               plotlyOutput("interactivePlot", height = "auto", width = "auto")
             )
           )
    )
  ),
  
  # New section for Map
  hr(),  # Add a horizontal line to separate sections
  fluidRow(
    column(12,
           sidebarLayout(
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
             ),
             mainPanel(
               plotOutput("mapPlot", height = "400px")
             )
           )
    )
  )
))
