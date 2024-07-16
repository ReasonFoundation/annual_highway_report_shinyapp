library(shiny)
library(plotly)

# Define the user interface
shinyUI(fluidPage(
  includeCSS("styles.css"),
  titlePanel("State Rankings by Category"),
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
      plotlyOutput("interactivePlot")
    )
  )
))
