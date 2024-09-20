

TABLE 1: OVERALL HIGHWAY PERFORMANCE RANKINGS

 
table1_2 <- score_rankings %>% select(state, overall_score_rank) %>% 
  arrange(overall_score_rank) %>% 
  datatable(option = list(lengtheMenu = c(10),
                          pageLength = 10))

TABLE 2: OVERALL HIGHWAY PERFORMANCE RANKINGS

IN ALPHABETICAL ORDER

 
score_rankings %>% select(state, overall_score_rank) %>% 
  arrange(state) %>% 
  datatable(option = list(lengtheMenu = c(10),
                          pageLength = 10))

#TABLE 3: HIGHWAY PERFORMANCE RANKINGS BY CATEGORY

 
table3 <- score_rankings %>% 
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



TABLE 4: OVERALL HIGHWAY PERFORMANCE RANKING TRENDS

 
table4 <- read.csv("data/Score_Rankings_in27threport.csv") %>% 
  clean_names() %>% 
  select(1:4) %>% 
  left_join(score_rankings %>% select(state, overall_score_rank)) %>% 
  rename(x2021 = overall_score_rank) %>% 
  mutate(change20_21 = (x2020 - x2021),
         change19_21 = (x2019 - x2021),
         change18_21 = (x2018 - x2021)) %>% 
  datatable(option = list(lengtheMenu = c(10),
                          pageLength = 10))


TABLE 5: STATE-CONTROLLED HIGHWAY MILES, 2020

 
table5 <- AHR_data %>% 
  arrange(desc(state_tot_lane_miles)) %>% 
  mutate(`2021 Size` = ifelse(row_number() == 1, "-", row_number() - 1)) %>% 
  select(`2021 Size`, state, state_tot_lane_miles) %>% 
  datatable(rownames = FALSE) %>% 
  formatStyle("state_tot_lane_miles",'text-align' = 'right') %>% 
  formatRound("state_tot_lane_miles", digits = 0)


TABLE 6: STATE-CONTROLLED HIGHWAY MILEAGE BY SYSTEM WIDTH, 2020

Note: Missing "Centerline Mileage"

 

table6 <- AHR_data %>% 
  arrange(desc(state_tot_lane_miles)) %>% 
  mutate(`2021 Size` = ifelse(row_number() == 1, "-", row_number() - 1)) %>% 
  select(`2021 Size`, state, SHA_ratio, state_tot_lane_miles) %>% 
  arrange(desc(SHA_ratio)) %>% 
  
  datatable(rownames = FALSE) %>% 
  formatStyle(3:4,'text-align' = 'right') %>% 
  formatRound("SHA_ratio", digit = 2) %>% 
  formatRound("state_tot_lane_miles", digit = 0)

TABLE 7: CAPITAL AND BRIDGE DISBURSEMENTS

 
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

table7 <- table_disbursement("capital")

TABLE 8: MAINTENANCE DISBURSEMENTS

 
table8 <-table_disbursement("maintenance")

TABLE 9: ADMINISTRATIVE DISBURSEMENTS

 
table9 <-table_disbursement("admin")

TABLE 10: OTHER DISBURSEMENTS

 
table10 <- table_disbursement("other")

TABLE 11: PERCENT RURAL INTERSTATE MILEAGE IN POOR CONDITION

NOTE: missing Weighted Average

 
table_cat <- function(cat) {
  AHR_data %>% select(state, {{cat}}) %>% 
    arrange(is.na({{cat}}), {{cat}}) %>% 
    mutate(`2021 Rank` = row_number()) %>% 
    datatable(rownames = FALSE) %>% 
    formatStyle(2,'text-align' = 'right') %>% 
    formatRound(2, digit = 2)
}

table11 <-table_cat(rural_interstate_poor_percent)

TABLE 12: PERCENT URBAN INTERSTATE MILEAGE IN POOR CONDITION

 
table12 <-table_cat(urban_interstate_poor_percent)

TABLE 13: PERCENT RURAL OTHER PRINCIPAL ARTERIAL MILEAGE IN POOR CONDITION

 
table13 <-table_cat(rural_OPA_poor_percent)

TABLE 14: PERCENT URBAN OTHER PRINCIPAL ARTERIAL MILEAGE IN POOR CONDITION

 
table14 <-table_cat(urban_OPA_poor_percent)

TABLE 15: ANNUAL PEAK HOURS SPENT IN CONGESTION PER AUTO COMMUTER

 
table15 <-table_cat(state_avg_congestion_hours)

TABLE 16: PERCENT STRUCTURALLY DEFICIENT BRIDGES

 
table16 <-table_cat(poor_bridges_percent)

TABLE 17: FATALITY RATE PER 100 MILLION RURAL VEHICLE-MILES

 
table17 <-table_cat(rural_fatalities_per_100m_VMT)

TABLE 18: FATALITY RATE PER 100 MILLION URBAN VEHICLEMILES

 
table18 <-table_cat(urban_fatalities_per_100m_VMT)

TABLE 19: FATALITY RATE PER 100 MILLION OTHER VEHICLE-MILES

 
table19 <-table_cat(other_fatalities_per_100m_VMT)

