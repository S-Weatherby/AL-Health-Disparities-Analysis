# Alabama Health Disparities Analysis
# Script: 04_demographic_analysis.R
# Author: Shelita
# Date: June 2025
# Purpose: Analyze racial and demographic disparities

library(tidyverse)


AL14_24SM2 <- read_csv("data/processed/AL14_24SM2_unified.csv")

##demographic (demo) analysis
###state level demo 

# Filter to 2020-2024 and State Total for demographic analysis
demo_data <- AL14_24SM2 %>%
  filter(
    year >= 2020,
    county == "State Total"
  ) %>%
  select(
    year,
    # Low Birth Weight by race
    pct_lbw, pct_lbw_aian, pct_lbw_asian, pct_lbw_black, 
    pct_lbw_hispanic, pct_lbw_white,
    # Child Poverty by race  
    pct_child_poverty, pct_child_poverty_aian, pct_child_poverty_asian,
    pct_child_poverty_black, pct_child_poverty_hispanic, pct_child_poverty_white
  )

# which racial groups have data across years
demo_data %>%
  summarise(
    across(contains("_aian"), ~sum(!is.na(.x))),
    across(contains("_asian"), ~sum(!is.na(.x))),
    across(contains("_black"), ~sum(!is.na(.x))),
    across(contains("_hispanic"), ~sum(!is.na(.x))),
    across(contains("_white"), ~sum(!is.na(.x)))
  )

# years with complete racial data
demo_analysis <- AL14_24SM2 %>%
  filter(
    year %in% c(2021, 2022, 2023),
    county == "State Total"
  ) %>%
  select(year, pct_lbw, pct_lbw_aian, pct_lbw_asian, pct_lbw_black, 
         pct_lbw_hispanic, pct_lbw_white,
         pct_child_poverty, pct_child_poverty_aian, pct_child_poverty_asian,
         pct_child_poverty_black, pct_child_poverty_hispanic, pct_child_poverty_white)

print(demo_analysis)

###county level
# County-level racial disparities for 2021-2023 WITH TBR ENHANCEMENT
county_disparities <- AL14_24SM2 %>%
  filter(
    year %in% c(2021, 2022, 2023),
    county != "State Total"
  ) %>%
  select(year, county, 
         pct_lbw_black, pct_lbw_white, 
         tbr_black, tbr_white,  # ENHANCED: ADD TBR variables
         pct_child_poverty_black, pct_child_poverty_white) %>%
  # Calculate racial gaps INCLUDING TBR
  mutate(
    lbw_gap = pct_lbw_black - pct_lbw_white,
    tbr_gap = tbr_black - tbr_white,  # ENHANCED: ADD TBR gap calculation
    poverty_gap = pct_child_poverty_black - pct_child_poverty_white
  ) %>%
  # Remove counties with insufficient data
  filter(!is.na(lbw_gap) | !is.na(tbr_gap) | !is.na(poverty_gap))

# ENHANCED: Look at the largest TBR disparities
print("Counties with Largest Teen Birth Rate Disparities (Black-White):")
county_disparities %>%
  group_by(county) %>%
  summarise(
    avg_lbw_gap = mean(lbw_gap, na.rm = TRUE),
    avg_tbr_gap = mean(tbr_gap, na.rm = TRUE),  # ENHANCED: TBR analysis
    avg_poverty_gap = mean(poverty_gap, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_tbr_gap)) %>%
  head(10)

# Look at the largest disparities (original poverty focus)
county_disparities %>%
  group_by(county) %>%
  summarise(
    avg_lbw_gap = mean(lbw_gap, na.rm = TRUE),
    avg_tbr_gap = mean(tbr_gap, na.rm = TRUE),
    avg_poverty_gap = mean(poverty_gap, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_poverty_gap)) %>%
  head(10)

# ENHANCED: Compare to state-level TBR gaps
state_gaps <- AL14_24SM2 %>%
  filter(year %in% c(2021, 2022, 2023), county == "State Total") %>%
  summarise(
    state_lbw_gap = mean(pct_lbw_black - pct_lbw_white, na.rm = TRUE),
    state_tbr_gap = mean(tbr_black - tbr_white, na.rm = TRUE),  # ENHANCED: ADD state TBR gap
    state_poverty_gap = mean(pct_child_poverty_black - pct_child_poverty_white, na.rm = TRUE)
  )

print("State-Level Gaps (2021-2023) WITH TBR:")
print(state_gaps)

# Which counties have the SMALLEST gaps (most equitable)?
county_disparities %>%
  group_by(county) %>%
  summarise(
    avg_lbw_gap = mean(lbw_gap, na.rm = TRUE),
    avg_tbr_gap = mean(tbr_gap, na.rm = TRUE),
    avg_poverty_gap = mean(poverty_gap, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(avg_poverty_gap) %>%
  head(10)

# ENHANCED: Create year-filterable disparity data for Tableau WITH TBR
tableau_disparities_enhanced <- county_disparities %>%
  select(year, county, lbw_gap, tbr_gap, poverty_gap) %>%  # ENHANCED: ADD tbr_gap
  mutate(
    year_filter = as.character(year),
    lbw_gap_category = case_when(
      lbw_gap >= 10 ~ "High Disparity (10+ points)",
      lbw_gap >= 5 ~ "Moderate Disparity (5-10 points)", 
      lbw_gap >= 0 ~ "Low Disparity (0-5 points)",
      lbw_gap < 0 ~ "Reverse Disparity",
      TRUE ~ "No Data"
    ),
    # ENHANCED: ADD TBR gap categories
    tbr_gap_category = case_when(
      tbr_gap >= 40 ~ "Extreme Disparity (40+ per 1000)",
      tbr_gap >= 20 ~ "High Disparity (20-40 per 1000)",
      tbr_gap >= 10 ~ "Moderate Disparity (10-20 per 1000)",
      tbr_gap >= 0 ~ "Low Disparity (0-10 per 1000)",
      tbr_gap < 0 ~ "Reverse Disparity (White > Black)",
      TRUE ~ "No Data"
    ),
    poverty_gap_category = case_when(
      poverty_gap >= 40 ~ "Extreme Disparity (40+ points)",
      poverty_gap >= 20 ~ "High Disparity (20-40 points)",
      poverty_gap >= 0 ~ "Moderate Disparity (0-20 points)",
      poverty_gap < 0 ~ "Reverse Disparity (White > Black)",
      TRUE ~ "No Data"
    )
  ) %>%
  # Add "All Years" aggregate data WITH TBR
  bind_rows(
    county_disparities %>%
      group_by(county) %>%
      summarise(
        year = 9999,
        year_filter = "All Years",
        lbw_gap = mean(lbw_gap, na.rm = TRUE),
        tbr_gap = mean(tbr_gap, na.rm = TRUE),  # ENHANCED: ADD TBR aggregate
        poverty_gap = mean(poverty_gap, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        lbw_gap_category = case_when(
          lbw_gap >= 10 ~ "High Disparity (10+ points)",
          lbw_gap >= 5 ~ "Moderate Disparity (5-10 points)", 
          lbw_gap >= 0 ~ "Low Disparity (0-5 points)",
          lbw_gap < 0 ~ "Reverse Disparity",
          TRUE ~ "No Data"
        ),
        # ENHANCED: ADD TBR categories for aggregate
        tbr_gap_category = case_when(
          tbr_gap >= 40 ~ "Extreme Disparity (40+ per 1000)",
          tbr_gap >= 20 ~ "High Disparity (20-40 per 1000)",
          tbr_gap >= 10 ~ "Moderate Disparity (10-20 per 1000)",
          tbr_gap >= 0 ~ "Low Disparity (0-10 per 1000)",
          tbr_gap < 0 ~ "Reverse Disparity (White > Black)",
          TRUE ~ "No Data"
        ),
        poverty_gap_category = case_when(
          poverty_gap >= 40 ~ "Extreme Disparity (40+ points)",
          poverty_gap >= 20 ~ "High Disparity (20-40 points)",
          poverty_gap >= 0 ~ "Moderate Disparity (0-20 points)",
          poverty_gap < 0 ~ "Reverse Disparity (White > Black)",
          TRUE ~ "No Data"
        )
      )
  ) %>%
  arrange(county, year)

# Check the output
tableau_disparities_enhanced %>%
  filter(county == "Jefferson") %>%
  select(year_filter, county, lbw_gap, tbr_gap, poverty_gap)

###2018 - 2024 Demo analysis WITH TBR ENHANCEMENT

demo_2018_2024 <- AL14_24SM2 %>%
  filter(
    year >= 2018,
    county != "State Total"
  ) %>%
  select(
    year, county,
    # Low Birth Weight
    pct_lbw, pct_lbw_black, pct_lbw_white, pct_lbw_hispanic,
    # Teen Birth Rate - ENHANCED: ADD THESE
    tbr, tbr_black, tbr_white, tbr_hispanic,
    # Child Poverty
    pct_child_poverty, pct_child_poverty_black, pct_child_poverty_white, pct_child_poverty_hispanic
  ) %>%
  # Calculate gaps INCLUDING TBR
  mutate(
    lbw_black_white_gap = pct_lbw_black - pct_lbw_white,
    lbw_hispanic_white_gap = pct_lbw_hispanic - pct_lbw_white,
    # ENHANCED: ADD TBR gaps
    tbr_black_white_gap = tbr_black - tbr_white,
    tbr_hispanic_white_gap = tbr_hispanic - tbr_white,
    poverty_black_white_gap = pct_child_poverty_black - pct_child_poverty_white,
    poverty_hispanic_white_gap = pct_child_poverty_hispanic - pct_child_poverty_white
  )

print("TBR Data Availability by Year:")
demo_2018_2024 %>%
  group_by(year) %>%
  summarise(
    counties_with_lbw_data = sum(!is.na(lbw_black_white_gap)),
    counties_with_tbr_data = sum(!is.na(tbr_black_white_gap)),  # ENHANCED: ADD TBR check
    counties_with_poverty_data = sum(!is.na(poverty_black_white_gap)),
    .groups = 'drop'
  )

state_trends_2018_2024 <- demo_2018_2024 %>%
  group_by(year) %>%
  summarise(
    # Average gaps across counties
    avg_lbw_black_white_gap = mean(lbw_black_white_gap, na.rm = TRUE),
    avg_lbw_hispanic_white_gap = mean(lbw_hispanic_white_gap, na.rm = TRUE),
    # ENHANCED: ADD TBR gaps
    avg_tbr_black_white_gap = mean(tbr_black_white_gap, na.rm = TRUE),
    avg_tbr_hispanic_white_gap = mean(tbr_hispanic_white_gap, na.rm = TRUE),
    avg_poverty_black_white_gap = mean(poverty_black_white_gap, na.rm = TRUE),
    avg_poverty_hispanic_white_gap = mean(poverty_hispanic_white_gap, na.rm = TRUE),
    
    # Number of counties with data
    counties_lbw = sum(!is.na(lbw_black_white_gap)),
    counties_tbr = sum(!is.na(tbr_black_white_gap)),  # ENHANCED: ADD TBR count
    counties_poverty = sum(!is.na(poverty_black_white_gap)),
    .groups = 'drop'
  )

print("Enhanced State Trends 2018-2024 WITH TBR:")
print(state_trends_2018_2024)

demographic_trends_detailed <- demo_2018_2024 %>%
  group_by(year) %>%
  summarise(
    # Overall rates
    avg_lbw = mean(pct_lbw, na.rm = TRUE),
    avg_tbr = mean(tbr, na.rm = TRUE),  # ENHANCED: ADD overall TBR
    avg_child_poverty = mean(pct_child_poverty, na.rm = TRUE),
    
    # Black-White gaps
    lbw_black_white_gap = mean(lbw_black_white_gap, na.rm = TRUE),
    tbr_black_white_gap = mean(tbr_black_white_gap, na.rm = TRUE),  # ENHANCED: ADD TBR gap
    poverty_black_white_gap = mean(poverty_black_white_gap, na.rm = TRUE),
    
    # Hispanic-White gaps  
    lbw_hispanic_white_gap = mean(lbw_hispanic_white_gap, na.rm = TRUE),
    tbr_hispanic_white_gap = mean(tbr_hispanic_white_gap, na.rm = TRUE),  # ENHANCED: ADD TBR Hispanic gap
    poverty_hispanic_white_gap = mean(poverty_hispanic_white_gap, na.rm = TRUE),
    
    # Data availability counts
    counties_with_lbw_data = sum(!is.na(lbw_black_white_gap)),
    counties_with_tbr_data = sum(!is.na(tbr_black_white_gap)),  # ENHANCED: ADD TBR availability
    counties_with_poverty_data = sum(!is.na(poverty_black_white_gap)),
    
    .groups = 'drop'
  ) %>%
  # Convert percentages to decimals for Tableau (TBR stays as rate per 1000)
  mutate(
    lbw_black_white_gap = lbw_black_white_gap / 100,
    poverty_black_white_gap = poverty_black_white_gap / 100,
    lbw_hispanic_white_gap = lbw_hispanic_white_gap / 100,
    poverty_hispanic_white_gap = poverty_hispanic_white_gap / 100,
    avg_lbw = avg_lbw / 100,
    avg_child_poverty = avg_child_poverty / 100
    # Note: TBR rates stay as-is (per 1000 births, not percentages)
  )

# ENHANCED: TBR trends for major counties
print("TBR Trends for Major Counties:")
major_counties <- c("Jefferson", "Mobile", "Madison", "Montgomery", "Tuscaloosa")

demo_2018_2024 %>%
  filter(county %in% major_counties) %>%
  select(year, county, tbr_black_white_gap, tbr_hispanic_white_gap, 
         poverty_black_white_gap, lbw_black_white_gap) %>%
  arrange(county, year)

##ENHANCED expanding on racial trends WITH TBR
# Create streamlined comprehensive race trends table (2017-2024) WITH TBR
comprehensive_race_trends <- AL14_24SM2 %>%
  filter(year >= 2017) %>%
  select(year, county, 
         # All racial breakdowns for LBW
         pct_lbw_aian, pct_lbw_asian, pct_lbw_black, pct_lbw_white, 
         pct_lbw_hispanic, pct_lbw_biracial, pct_lbw_nhopi,
         # All racial breakdowns for Child Poverty
         pct_child_poverty_aian, pct_child_poverty_asian, pct_child_poverty_black, 
         pct_child_poverty_white, pct_child_poverty_hispanic,
         # ENHANCED: All racial breakdowns for TBR
         tbr_aian, tbr_asian, tbr_black, tbr_white, tbr_hispanic, tbr_biracial, tbr_nhopi) %>%
  
  pivot_longer(
    cols = starts_with("pct_lbw_"),
    names_to = "race_group",
    values_to = "pct_lbw",
    names_prefix = "pct_lbw_"
  ) %>%
  
  left_join(
    AL14_24SM2 %>%
      filter(year >= 2017) %>%
      select(year, county, starts_with("pct_child_poverty_")) %>%
      pivot_longer(
        cols = starts_with("pct_child_poverty_"),
        names_to = "race_group",
        values_to = "pct_cip",
        names_prefix = "pct_child_poverty_"
      ),
    by = c("year", "county", "race_group")
  ) %>%
  
  # ENHANCED: ADD TBR join
  left_join(
    AL14_24SM2 %>%
      filter(year >= 2017) %>%
      select(year, county, starts_with("tbr_")) %>%
      pivot_longer(
        cols = starts_with("tbr_"),
        names_to = "race_group",
        values_to = "tbr_rate",
        names_prefix = "tbr_"
      ),
    by = c("year", "county", "race_group")
  ) %>%
  
  mutate(
    race = case_when(
      race_group == "aian" ~ "American Indian/Alaska Native",
      race_group == "asian" ~ "Asian",
      race_group == "black" ~ "Black", 
      race_group == "white" ~ "White",
      race_group == "hispanic" ~ "Hispanic",
      race_group == "biracial" ~ "Two or More Races",
      race_group == "nhopi" ~ "Native Hawaiian/Pacific Islander",
      TRUE ~ str_to_title(race_group)
    ),
    
    time_period = case_when(
      year %in% 2017:2019 ~ "Early Period (2017-2019): Basic Racial Data",
      year %in% 2020:2023 ~ "Expanded Period (2020-2023): Full Racial Data",
      year >= 2024 ~ "Recent (2024+): Enhanced Racial Data"
    ),
    
    # CONVERT TO DECIMALS 
    pct_lbw = as.numeric(as.character(pct_lbw)) / 100,
    pct_cip = as.numeric(as.character(pct_cip)) / 100,
    # Keep TBR as whole number (it's rate per 1,000, not percentage)
    tbr_rate = as.numeric(as.character(tbr_rate)),
    
    # Data availability flags
    has_lbw_data = !is.na(pct_lbw),
    has_cip_data = !is.na(pct_cip),
    has_tbr_data = !is.na(tbr_rate),
    
    race_data_start_year = case_when(
      race %in% c("Black", "White", "Hispanic") ~ 2017,
      race %in% c("American Indian/Alaska Native", "Asian") ~ 2020,
      race == "Two or More Races" ~ 2024,
      race == "Native Hawaiian/Pacific Islander" ~ 2024,
      TRUE ~ 2017
    )
  ) %>%
  
  select(year, county, race, pct_lbw, tbr_rate, pct_cip, time_period, 
         has_lbw_data, has_cip_data, has_tbr_data, race_data_start_year)

# Verify the conversion
print("Sample values after conversion:")
comprehensive_race_trends %>%
  filter(county == "State Total", year == 2021, race == "Black") %>%
  select(pct_lbw, pct_cip, tbr_rate)

# Check the structure
print("Final dataset structure:")
head(comprehensive_race_trends, 10)

# Verify data types (should be numeric for measures)
print("Data types for measures:")
sapply(comprehensive_race_trends[c("pct_lbw", "tbr_rate", "pct_cip")], class)

# Show data availability by race
print("Data availability by race:")
comprehensive_race_trends %>%
  group_by(race, race_data_start_year) %>%
  summarise(
    years_with_lbw = sum(has_lbw_data, na.rm = TRUE),
    years_with_cip = sum(has_cip_data, na.rm = TRUE),
    years_with_tbr = sum(has_tbr_data, na.rm = TRUE),
    .groups = 'drop'
  )

# Save demographic analysis results
write_csv(demographic_trends_detailed, "outputs/tables/demographic_trends_detailed.csv")
write_csv(comprehensive_race_trends, "outputs/tables/comprehensive_race_trends.csv")
write_csv(tableau_disparities_enhanced, "outputs/tables/tableau_disparities_enhanced.csv")
write_csv(state_trends_2018_2024, "outputs/tables/state_trends_2018_2024.csv")
