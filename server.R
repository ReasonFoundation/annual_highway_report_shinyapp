library(shiny)
library(ggplot2)
library(ggbump)
library(dplyr)
library(stringr)
library(plotly)
library(readxl)
library(httr)
library(tidyr)
source("charts.R")

# Define the server logic
shinyServer(function(input, output, session) {
  
  # Default metric
  observe({
    if (is.null(input$metric)) {
      updateSelectInput(session, "metric", selected = "Overall Score")
    }
  })
  
  submetric_choices <- reactive({
    switch(input$metric,
           "Overall Score" = NULL,
           "Bridges" = NULL,
           "Congestion Hours" = NULL, 
           "Pavement Roughness" = c("Urban Interstate Pavement Roughness", 
                                    "Rural Interstate Pavement Roughness", 
                                    "Urban Opa Pavement Roughness", 
                                    "Rural Opa Pavement Roughness"),
           "Disbursement" = c("Admin Disbursement", 
                              "Capital Disbursement", 
                              "Maintenance Disbursement", 
                              "Other Disbursement"),
           "Fatalities" = c("Rural Fatalities", 
                            "Urban Fatalities", 
                            "Other Fatalities"),
           NULL)  # Default case
  })
  
  observe({
    updateSelectInput(session, "submetric", choices = submetric_choices())
  })
  
  output$submetric_ui <- renderUI({
    if (!is.null(input$metric) && is.null(submetric_choices())) {
      return(NULL)
    }
    selectInput("submetric", "Submetric:", choices = submetric_choices())
  })
  
  output$interactivePlot <- renderPlotly({
    req(input$metric)
    
    # Check if the selected metric does not have submetrics
    if (input$metric %in% c("Overall Score", "Bridges", "Congestion Hours")) {
      no_submetric_ranking_plots(ranking_df, input$metric)
    } else {
      req(input$submetric)
      submetric <- input$submetric
      # plot for metrics with submetrics
      create_ranking_plot(ranking_df, input$metric, submetric)
    }
  })
  
  output$mapPlot <- renderPlot({
    req(input$map_metric)
    
    #check of df_for_map loaded right
    if(!inherits(df_for_map, "sf")){
      stop("df_for_map is not an sf object")
    }
    
    # Generate the map plot based on the selected map metric
    p_map <- create_maps(df_for_map, input$map_metric)
    p_map
    #ggplotly(p_map)
  })
  
})
