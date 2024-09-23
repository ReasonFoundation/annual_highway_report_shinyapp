library(shiny)
library(plotly)
library(DT)

source("charts_tables.R")
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
               selectInput("category", "Select Table", 
                           choices = c(
                             "Overall Rank", 
                             "Highway Performance Ranking by Category",
                             "Overall Highway Performance Ranking Trends",
                             "State-controlled Highway Miles",
                             "State-controlled Highway Mileage by System Width",
                             "Capital and Bridge Disbursements", 
                             "Maintenance Disbursements",
                             "Administrative Disbursements",
                             "Other Disbursements",
                             "Percent Rural Interstate Mileage in Poor Condition",
                             "Percent Urban Interstate Mileage In Poor Condition",
                             "Percent Rural Other Principal Arterial Mileage In Poor Condition",
                             "Percent Urban Other Principal Arterial Mileage In Poor Condition",
                             "Annual Peak Hours Spent In Congestion Per Auto Commuter",
                             "Percent Structurally Deficient Bridges",  
                             "Fatality Rate Per 100 Million Rural Vehicle-Miles", 
                             "Fatality Rate Per 100 Million Urban Vehicle-Miles",
                             "Fatality Rate Per 100 Million Other Vehicle-Miles"
                             
                             )
               )
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Table", DTOutput("ranking_table")),
                 tabPanel("Figure", plotlyOutput("ranking_map"))
               )
             )
           )
    )
  )
))
