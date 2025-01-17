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
      submetric <- NULL
    } else {
      req(input$submetric)
      submetric <- input$submetric
    }
    
    # Generate the interactive plot
    create_ranking_plot(ranking_df, input$metric, submetric)
  })
})


#### BUMP chart
library(readxl)
library(httr)
library(tidyr)
library(ggplot2)
library(stringr)
library(dplyr)
library(ggbump)
library(plotly)
library(viridisLite)
library(maps)
library(sf)
library(wesanderson)

####Preparing data####
url <- "https://raw.githubusercontent.com/thuy2020/Annual-Highway-Report/report_2022/output/AHR_data_2022.xlsx"
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))

sheet1 <- read_excel(tf, sheet = 1)
sheet3 <- read_excel(tf, sheet = 3)
sheet2 <- read_excel(tf, sheet = 2) 

ranking_df <- sheet2 %>% 
  select(state, contains("rank")) %>% 
  pivot_longer(cols = -state, names_to = "category", values_to = "ranking") %>% 
  mutate(category = str_replace_all(category, "_", " "),
         category = str_to_title(category),
         category = str_remove_all(category, "(Rank)|(Poor)"),
         category = str_remove_all(category, "(Per 100m Vmt)|(Percent)|(Perlm)"),
         category = str_replace_all(category, "State Avg Congestion Hours", "Congestion Hours"),
         category = str_squish(category))   

####Map function####
states_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

df_for_map <- ranking_df %>% 
  pivot_wider(names_from = category, values_from = ranking) %>% 
  filter(state != "United States") %>% 
  mutate(state = str_to_lower(state)) %>% 
  left_join(states_map, by = c("state" = "ID")) %>% 
  st_as_sf() %>% 
  #using a projected coordinate system (not longitude/latitude)
  #(Coordinate Reference System WGS84 (crs = 4326).
  st_transform(crs = 4326) 


wes_palette <- wes_palette("Darjeeling1", n = 5, type = "continuous")

# Function to generate maps for each specified ranking column
create_maps <- function(df_for_map, metric) {
  
  p_map <- ggplot(data = df_for_map) +
    geom_sf(aes(geometry = geom, fill = !!rlang::sym(metric)), color = "white") +
    geom_sf_text(aes(geometry = geom, label = !!rlang::sym(metric)), 
                 check_overlap = TRUE, size = 3, color = "white") +
    #scale_fill_viridis_c(option = "D", direction = -1) +  
    scale_fill_gradientn(colors = wes_palette) +  
    labs(title = paste("Rankings by", metric)) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),   
      axis.title.y = element_blank(),  
      axis.text.x = element_blank(),  
      axis.text.y = element_blank(),
      legend.position = "none")
  
  return(p_map)
}

create_maps(df_for_map, "Overall Score") 

####Bump chart function####

create_ranking_plot <- function(ranking_df, metric, submetric) {
  
  d <- ranking_df %>%
    filter(str_detect(category, metric))%>% 
    mutate(top_state = ifelse(ranking <= 10 & category == submetric, "Top 10", "Others")) %>% 
    mutate(category_wrapped = str_wrap(category, width = 10))
  
  # Set colors based on the metric
  line_color <- switch(tolower(metric),
                       "disbursement" = "#FF5733",
                       "fatalities" = "#C70039",
                       "pavement roughness" = "purple") 
  
  # Get the rightmost category
  rightmost_category <- max(d$category_wrapped)
  
  # Create a ggbump chart highlighting top 10 states
  p <- d %>%
    ggplot(aes(x = category_wrapped, y = ranking, group = state, 
               color = top_state, size = top_state,
               text = paste("State: ", state, "<br>Category: ", category, "<br>Ranking: ", ranking))) +
    geom_bump() +
    geom_point(aes(color = top_state, size = top_state)) +
    geom_text(data = d %>% filter(category == submetric & 
                                    top_state == "Top 10"), 
              aes(x = rightmost_category,label = state), 
              hjust = 0, vjust = 0.5, size = 1.5, nudge_x = 0.3, color = line_color) +
    scale_color_manual(values = c("Top 10" = line_color, "Others" = "gray")) +
    scale_size_manual(values = c("Top 10" = 0.5, "Others" = 0.1)) +
    theme_minimal() +
    labs(title = paste("Rankings by", str_to_title(metric)),
         x = "",
         y = "") +
    theme(
      legend.position = "none",
      axis.text.x = element_text(vjust = 0.5, hjust = 1)
    )
  
  # Convert the ggplot2 plot to an interactive plot using plotly
  interactive_plot <- ggplotly(p, tooltip = "text")
  
  # Customize the tooltip appearance
  interactive_plot <- interactive_plot %>%
    layout(hoverlabel = list(
      bgcolor = "white",
      font = list(size = 10)
    ))
  
  # Display the interactive plot
  return(interactive_plot)
}

#Testing
metric <- "Pavement Roughness"
submetric <- "Urban Opa Pavement Roughness"
create_ranking_plot(ranking_df, metric, submetric)


####No submetric ranking####

point_color = c("Overall Score" = "#008E75", 
                "Bridges" = "#900C3F", 
                "Congestion Hours" = "#CA3311")

# Define the function to generate plots for each metric
no_submetric_ranking_plots <- function(ranking_df, metrics) {
  
  for (metric in metrics) {
    p <- ranking_df %>% 
      filter(category == metric) %>% 
      ggplot(aes(x = ranking, y = reorder(state, ranking))) +
      geom_col(fill = "gray", width = 0.1) +
      geom_point(size = 3, color = point_color[metric]) +
      theme_minimal() +
      labs(title = paste("Rankings by", metric),
           x = "Ranking",
           y = "")+
      theme(axis.text.y = element_text(size = 6))
    
    return(p)
    
  }
}

# List of metrics to loop through
metrics <- c("Bridges")
#, "Bridges", "Congestion Hours"
# Generate the plots
no_submetric_ranking_plots(ranking_df, metrics)