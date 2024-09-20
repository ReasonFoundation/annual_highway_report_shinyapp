library(shiny)
library(DT)
library(ggplot2)

source("charts_tables.R")  


shinyServer(function(input, output, session) {
  
  # Define table mappings based on the categories in selectInput
  category_tables <- list(
    "Overall Rank" = table1_2_overall_score_rank,
    "Highway Performance Ranking by Category" = table3_all_categories,
    "Overall Highway Performace Ranking Trends" = table4_ranking_trend,
    "State-controlled Highway Miles" = table5_state_controlled_miles, 
    "State-controlled Highway Mileage by System Width"= table6_state_controlled_mileage_width,
    "Capital and Bridge Disbursements"= table7_capital_disbursement, 
    "Maintenance Disbursements"= table8_maintenance_disbursement, 
    "Administrative Disbursements"= table9_admin_disbursement,
    "Other Disbursements" = table10_other_disbursement,
    "Percent Rural Interstate Mileage in Poor Condition" = table11,
    "Percent Urban Interstate Mileage In Poor Condition" = table12,
    "Percent Rural Other Principal Arterial Mileage In Poor Condition" = table13,
    "Percent Urban Other Principal Arterial Mileage In Poor Condition" = table14,
    "Annual Peak Hours Spent In Congestion Per Auto Commuter" = table15,
    "Percent Structurally Deficient Bridges" = table16,  
    "Fatality Rate Per 100 Million Rural Vehicle-Miles" = table17,  
    "Fatality Rate Per 100 Million Urban Vehicle-Miles" = table18,  
    "Fatality Rate Per 100 Million Other Vehicle-Miles" = table19
  )
  
  # Define map category mappings to the corresponding metric for maps
  category_maps <- list(
    "Overall Rank" = "Overall Score",  # Column in df_for_map
    "Highway Performance Ranking by Category" = "Capital Disbursement Score"
    
  )
  
  # Render the table based on the selected category
  output$ranking_table <- renderDT({
    selected_category <- input$category
    table_to_show <- category_tables[[selected_category]]
    
    table_to_show  
  })
  

  output$ranking_map <- renderPlot({
    selected_category <- input$category
    
    # Get the corresponding metric 
    metric <- category_maps[[selected_category]]
    
    if (!is.null(metric)) {
      create_maps(df_for_map, metric)  
    }
  })
})
