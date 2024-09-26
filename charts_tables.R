library(readxl)
library(tidyr)
library(ggplot2)
library(stringr)
library(dplyr)
library(ggbump)
library(plotly)
library(sf)
library(usmap)
library(janitor)
library(wesanderson)


score_rankings <- readxl::read_excel("data/AHR_data 28th_summing_components_columns.xlsx", sheet = 2)
AHR_data <- readxl::read_excel("data/AHR_data 28th_summing_components_columns.xlsx", sheet = 1)
disbursement_data <- readxl::read_excel("data/AHR_data 28th_summing_components_columns.xlsx", sheet = 3)

####Tables####

#TABLE 1: OVERALL HIGHWAY PERFORMANCE RANKINGS

table1_2_overall_score_rank <- score_rankings %>% select(state, overall_score_rank) %>% 
  arrange(overall_score_rank) %>% 
  datatable(option = list(lengtheMenu = c(10),
                          pageLength = 10))

#TABLE 3: HIGHWAY PERFORMANCE RANKINGS BY CATEGORY

table3_all_categories <- score_rankings %>% 
  select(state, 
         overall_score_rank,
         #Disbursement
         capital_disbursement_perlm_score_rank, 
         maintenance_disbursement_perlm_score, 
         admin_disbursement_perlm_score_rank, 
         other_disbursement_perlm_score_rank,
         
         #Pavement roughness
         rural_interstate_poor_percent_score_rank,
         urban_interstate_poor_percent_score_rank,
         
         rural_OPA_poor_percent_score_rank,
         urban_OPA_poor_percent_score_rank,
         
         state_avg_congestion_hours_score_rank,
         
         poor_bridges_percent_score_rank,
         
         #fatality
         rural_fatalities_per_100m_VMT_score_rank,
         urban_fatalities_per_100m_VMT_score_rank,
         other_fatalities_per_100m_VMT_score_rank
  ) %>% datatable()


#TABLE 4: OVERALL HIGHWAY PERFORMANCE RANKING TRENDS

table4_ranking_trend <- read.csv("data/Score_Rankings_in27threport.csv") %>% 
  clean_names() %>% 
  select(1:4) %>% 
  left_join(score_rankings %>% select(state, overall_score_rank)) %>% 
  rename(x2021 = overall_score_rank) %>% 
  mutate(change20_21 = (x2020 - x2021),
         change19_21 = (x2019 - x2021),
         change18_21 = (x2018 - x2021)) %>% 
  datatable(option = list(lengtheMenu = c(10),
                          pageLength = 10))


#TABLE 5: STATE-CONTROLLED HIGHWAY MILES, 2020


table5_state_controlled_miles <- AHR_data %>% 
  arrange(desc(state_tot_lane_miles)) %>% 
  mutate(`2021 Size` = ifelse(row_number() == 1, "-", row_number() - 1)) %>% 
  select(`2021 Size`, state, state_tot_lane_miles) %>% 
  datatable(rownames = FALSE) %>% 
  formatStyle("state_tot_lane_miles",'text-align' = 'right') %>% 
  formatRound("state_tot_lane_miles", digits = 0)


#TABLE 6: STATE-CONTROLLED HIGHWAY MILEAGE BY SYSTEM WIDTH, 2020
#Note: 27th report says "To treat all states equally we use lane-miles as
#opposed to center-line miles in our calculations"

table6_state_controlled_mileage_width <- AHR_data %>% 
  arrange(desc(state_tot_lane_miles)) %>% 
  mutate(`2021 Size` = ifelse(row_number() == 1, "-", row_number() - 1)) %>% 
  select(`2021 Size`, state, SHA_ratio, state_tot_lane_miles, SHA_miles) %>% 
  arrange(desc(SHA_ratio)) %>% 
  
  datatable(rownames = FALSE) %>% 
  formatStyle(3:4,'text-align' = 'right') %>% 
  formatRound("SHA_ratio", digit = 2) %>% 
  formatRound("state_tot_lane_miles", digit = 0)

#TABLE 7: CAPITAL AND BRIDGE DISBURSEMENTS

table_disbursement <- function (disbursement_cat) {disbursement_data %>% 
    filter(str_detect(key_metrics, disbursement_cat)) %>% 
    select(-key_metrics) %>% 
    mutate(adjusted_ratio = value/exp_value) %>% 
    arrange(value) %>% 
    mutate(`2021 Rank` = row_number()) %>% 
    datatable(rownames = FALSE) %>% 
    formatStyle(2:4,'text-align' = 'right') %>% 
    formatRound(2:3, digit = 0) %>% 
    formatRound(4, digit = 2) 
}

table7_capital_disbursement <- table_disbursement("capital")

#TABLE 8: MAINTENANCE DISBURSEMENTS

table8_maintenance_disbursement <-table_disbursement("maintenance")

#TABLE 9: ADMINISTRATIVE DISBURSEMENTS
table9_admin_disbursement <-table_disbursement("admin")

#TABLE 10: OTHER DISBURSEMENTS

table10_other_disbursement <- table_disbursement("other")

#TABLE 11: PERCENT RURAL INTERSTATE MILEAGE IN POOR CONDITION
#NOTE: missing Weighted Average

table_cat <- function(cat) {
  AHR_data %>% select(state, {{cat}}) %>% 
    arrange(is.na({{cat}}), {{cat}}) %>% 
    mutate(`2021 Rank` = row_number()) %>% 
    datatable(rownames = FALSE) %>% 
    formatStyle(2,'text-align' = 'right') %>% 
    formatRound(2, digit = 2)
}

table11 <-table_cat(rural_interstate_poor_percent)

#TABLE 12: PERCENT URBAN INTERSTATE MILEAGE IN POOR CONDITION

table12 <-table_cat(urban_interstate_poor_percent)

#TABLE 13: PERCENT RURAL OTHER PRINCIPAL ARTERIAL MILEAGE IN POOR CONDITION

table13 <-table_cat(rural_OPA_poor_percent)

#TABLE 14: PERCENT URBAN OTHER PRINCIPAL ARTERIAL MILEAGE IN POOR CONDITION

table14 <-table_cat(urban_OPA_poor_percent)

#TABLE 15: ANNUAL PEAK HOURS SPENT IN CONGESTION PER AUTO COMMUTER

table15 <-table_cat(state_avg_congestion_hours)

#TABLE 16: PERCENT STRUCTURALLY DEFICIENT BRIDGES

table16 <-table_cat(poor_bridges_percent)

#TABLE 17: FATALITY RATE PER 100 MILLION RURAL VEHICLE-MILES

table17 <-table_cat(rural_fatalities_per_100m_VMT)

#TABLE 18: FATALITY RATE PER 100 MILLION URBAN VEHICLEMILES

table18 <-table_cat(urban_fatalities_per_100m_VMT)

#TABLE 19: FATALITY RATE PER 100 MILLION OTHER VEHICLE-MILES

table19 <-table_cat(other_fatalities_per_100m_VMT)

####Maps####


ranking_df <- score_rankings %>% 
  select(state, contains("rank")) %>% 
  pivot_longer(cols = -state, names_to = "category", values_to = "ranking") %>% 
  mutate(category = str_replace_all(category, "_", " "),
         category = str_to_title(category),
         category = str_remove_all(category, "(Rank)|(Poor)"),
         category = str_remove_all(category, "(Per 100m Vmt)|(Percent)|(Perlm)"),
         category = str_replace_all(category, "State Avg Congestion Hours", "Congestion Hours"),
         category = str_squish(category))   

df_for_map <- ranking_df %>% 
  pivot_wider(names_from = category, values_from = ranking) %>% 
  filter(state != "United States")

data_usmap <- df_for_map %>% 
  rename(full = state) %>% 
  left_join(us_map()) 

#wes_palette <- wes_palette("Darjeeling1", n = 100, type = "continuous")

# Function to generate maps for each specified ranking column
create_maps <- function(df_for_map, metric) {
  
  # Extract the usmap data and convert it to an sf object
  usmap_data <- us_map(regions = "states")
  usmap_sf <- st_as_sf(usmap_data) %>% 
    left_join(data_usmap, by = "full")  # Join using the correct column
  
  # Calculate centroids for each state
  centroids <- st_centroid(usmap_sf)
  
  # Extract x and y coordinates of the centroids
  centroids_coords <- as.data.frame(st_coordinates(centroids))
  centroids_coords$full <- usmap_sf$full  
  centroids_coords$metric <- usmap_sf[[metric]]  
  
  
  p_map <- plot_usmap("states", data = data_usmap, values = metric,
                      labels = TRUE, 
                      label_color = "white") +
    scale_fill_continuous(low = "#F0F9E7", high = "#207DB7", guide = "none",
                          labels = TRUE) +
    
    geom_text(data = centroids_coords, 
              aes(x = X, y = Y, label = metric), 
              color = "black", size = 4) +
    
    labs(title = paste("Rankings by", metric)) + 
    theme(panel.background = element_blank(), 
          line = element_line(size = 0.02),
          legend.position = "none")
  
  p_map <- ggplotly(p_map, width = 900, height = 720)
  return(p_map)
}

# test "Overall Score"
create_maps(df_for_map, "Overall Score")


####Figures####
overall_score_changes <- read.csv("data/Score_Rankings_in27threport.csv") %>% 
  clean_names() %>% 
  select(1:4) %>% 
  left_join(score_rankings %>% select(state, overall_score_rank)) %>% 
  rename(x2021 = overall_score_rank) %>% 
  pivot_longer(cols = 2:5, names_to = "Year", values_to = "Rank") %>% 
  mutate(Year = str_remove(Year, "x")) %>% 
  mutate(Year = as.numeric(Year))


# Base ggplot with grey lines by default

plot_overall_changes <- overall_score_changes %>% 
  ggplot(aes(x = Year, y = Rank, group = state, 
             text = paste("State:", state, "<br>Rank:", Rank))) +  
  geom_line(aes(color = case_when(
    state %in% (overall_score_changes %>% filter(Year == 2021 & Rank <= 10) %>% pull(state)) ~ "#5F8065", 
    state %in% (overall_score_changes %>% filter(Year == 2021 & Rank > 10 & Rank <= 20) %>% pull(state)) ~ "#275066",
    state %in% (overall_score_changes %>% filter(Year == 2021 & Rank > 20 & Rank <= 30) %>% pull(state)) ~ "#BB9D7A",
    state %in% (overall_score_changes %>% filter(Year == 2021 & Rank > 30 & Rank <= 40) %>% pull(state)) ~ "#944C35",
    state %in% (overall_score_changes %>% filter(Year == 2021 & Rank > 40 & Rank <= 50) %>% pull(state)) ~ "#881F24",
    TRUE ~ "grey")), 
    size = 0.3, alpha = 0.4) + 
  geom_point(aes(color = case_when(
    state %in% (overall_score_changes %>% filter(Year == 2021 & Rank <= 10) %>% pull(state)) ~ "#5F8065", 
    state %in% (overall_score_changes %>% filter(Year == 2021 & Rank > 10 & Rank <= 20) %>% pull(state)) ~ "#275066",
    state %in% (overall_score_changes %>% filter(Year == 2021 & Rank > 20 & Rank <= 30) %>% pull(state)) ~ "#BB9D7A",
    state %in% (overall_score_changes %>% filter(Year == 2021 & Rank > 30 & Rank <= 40) %>% pull(state)) ~ "#944C35",
    state %in% (overall_score_changes %>% filter(Year == 2021 & Rank > 40 & Rank <= 50) %>% pull(state)) ~ "#881F24",
    TRUE ~ "grey")), 
    size = 1.5, alpha = 0.6) + 
  scale_color_identity() + 
  theme_minimal() +
  theme(legend.position = "none") +  
  scale_y_reverse() + 
  labs(title = "Overall Score Changes by State", x = "Year", y = "Rank")

plot_overall_changes <- ggplotly(plot_overall_changes, tooltip = "text",
                                 width = 900, height = 720) 


# State-control 
library(forcats)
library(scales)


fig5_state_control <- AHR_data %>% 
  arrange(desc(state_tot_lane_miles)) %>% 
  mutate(`2021 Size` = ifelse(row_number() == 1, "-", row_number() - 1)) %>% 
  select(`2021 Size`, state, SHA_ratio, state_tot_lane_miles) %>% 
  arrange(desc(SHA_ratio)) %>% 
  mutate(SHA_ratio = round(SHA_ratio,2)) %>% 
  filter(state != "United States") %>% 
  
  ggplot(aes(fct_reorder(state, state_tot_lane_miles), state_tot_lane_miles, 
         text = paste("State:", state, "<br>SHA_ratio:", SHA_ratio, 
                      "<br>Lane Miles:", state_tot_lane_miles))) +
          geom_col(fill = "#345B8D")+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),  
    plot.margin = unit(c(1, 1, 1, 1), "cm"))+  
  
  labs(x = "",
       y = "Lane Miles")
  
plotly_fig5_state_control <- ggplotly(fig5_state_control, tooltip = "text", 
                                          width = 900, height = 720) 

#Table 6 - Figure 6 table6_state_controlled_mileage_width


fig6_SHA_ratio <- AHR_data %>% 
  arrange(desc(state_tot_lane_miles)) %>% 
  mutate(`2021 Size` = ifelse(row_number() == 1, "-", row_number() - 1)) %>% 
  select(`2021 Size`, state, SHA_ratio, state_tot_lane_miles, SHA_miles) %>% 
  mutate(SHA_ratio = round(SHA_ratio, 2)) %>% 
  
  ggplot(aes(fct_reorder(state, SHA_ratio), SHA_ratio, 
             text = paste("State:", state, "<br>SHA_ratio:", SHA_ratio, 
                          "<br>Lane Miles:", state_tot_lane_miles,
                          "<br>Centerline Mileage:", SHA_miles))) +
  geom_col(fill = "#008E75")+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),  
    plot.margin = unit(c(1, 1, 1, 1), "cm"))+  
  
  labs(x = "",
       y = "Ratio")

plotly_fig6_SHA_ratio <- ggplotly(fig6_SHA_ratio, tooltip = "text", 
                                          width = 900, height = 720)


