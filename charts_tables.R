library(readxl)
library(tidyr)
library(ggplot2)
library(stringr)
library(dplyr)
library(ggbump)
library(plotly)
library(maps)
library(sf)
library(rnaturalearth)
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
#Note: Missing "Centerline Mileage"

table6_state_controlled_mileage_width <- AHR_data %>% 
  arrange(desc(state_tot_lane_miles)) %>% 
  mutate(`2021 Size` = ifelse(row_number() == 1, "-", row_number() - 1)) %>% 
  select(`2021 Size`, state, SHA_ratio, state_tot_lane_miles) %>% 
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

#Map function
# states_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
# 
# us_states <- ne_states(country = "United States of America", 
#                        returnclass = "sf")

#Census 2022 TIGER/Line® Shapefiles: States (and equivalent). 
#https://www.census.gov/cgi-bin/geo/shapefiles/index.php
states_shapefile <- st_read("data/tl_2022_us_state/tl_2022_us_state.shp") %>% 
  clean_names() %>% 
  select(name, geometry)
  
alaska <- states_shapefile %>% filter(name == "Alaska")
hawaii <- states_shapefile %>% filter(name == "Hawaii")
mainland <- states_shapefile %>% filter(!name %in% c("Alaska", "Hawaii"))
# Function to scale and shift geometries while preserving CRS
scale_and_shift <- function(geometry, scale_factor, shift_x = 0, shift_y = 0) {
  geom <- st_geometry(geometry) * scale_factor + c(shift_x, shift_y)
  st_crs(geom) <- st_crs(geometry)  # Reassign the original CRS
  st_geometry(geometry) <- geom
  return(geometry)
}

# Scale and move Alaska
alaska_transformed <- scale_and_shift(alaska, scale_factor = 0.35, 
                                      shift_x = 2500000, shift_y = -2200000)

# Scale and move Hawaii
hawaii_transformed <- scale_and_shift(hawaii, scale_factor = 0.8, 
                                      shift_x = 4000000, shift_y = -1200000)

# Combine mainland, transformed Alaska, and Hawaii
all_states <- rbind(mainland, alaska_transformed, hawaii_transformed)

df_for_map <- ranking_df %>% 
  pivot_wider(names_from = category, values_from = ranking) %>% 
  filter(state != "United States") %>% 
  left_join(all_states, by = c("state" = "name"))
  
wes_palette <- wes_palette("Darjeeling1", n = 100, type = "continuous")

# Function to generate maps for each specified ranking column
create_maps <- function(df_for_map, metric) {

 
  
    p_map <- ggplot(data = df_for_map) +
      geom_sf(aes(geometry = geometry, fill = !!rlang::sym(metric)), color = "white") +
      geom_sf_text(aes(geometry = geometry, label = !!rlang::sym(metric)), 
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

#Bridges Score
#Other Fatalities Score
create_maps(df_for_map, "Overall Score") 



