# Alabama Health Disparities Analysis
# Script: 05_tableau_export.R
# Author: Shelita
# Date: June 2025
# Purpose: Prepare all datasets for Tableau visualization

library(tidyverse)

AL14_24SM2 <- read_csv("data/processed/AL14_24SM2_unified.csv")

#============#
#Analysis
#============#

###State Trends
state_trends <- AL14_24SM2 %>% 
  filter(county == "State Total")

county_trends <- AL14_24SM2 %>% 
  filter(county != "State Total")

# Check the state trends data
glimpse(state_trends)

# See all 11 years 
state_trends %>% 
  select(year, pct_lbw, tbr, pct_hs_complete, pct_some_college, pct_child_poverty)

# Check data types
sapply(state_trends[c("year", "pct_lbw", "tbr", "pct_hs_complete", "pct_child_poverty")], class)

# Convert percentages to decimals for Tableau
state_trends_tableau <- state_trends %>%
  mutate(
    pct_lbw = pct_lbw / 100,
    pct_hs_complete = pct_hs_complete / 100,
    pct_some_college = pct_some_college / 100,
    pct_child_poverty = pct_child_poverty / 100
  )

# Convert percentage columns to decimals for Tableau in the main table
AL14_24SM2_tableau <- AL14_24SM2 %>%
  mutate(
    # Low Birth Weight percentages
    pct_lbw = pct_lbw / 100,
    pct_lbw_aian = pct_lbw_aian / 100,
    pct_lbw_asian = pct_lbw_asian / 100,
    pct_lbw_biracial = pct_lbw_biracial / 100,
    pct_lbw_black = pct_lbw_black / 100,
    pct_lbw_hispanic = pct_lbw_hispanic / 100,
    pct_lbw_nhopi = pct_lbw_nhopi / 100,
    pct_lbw_white = pct_lbw_white / 100,
    
    # Education percentages
    pct_hs_complete = pct_hs_complete / 100,
    pct_some_college = pct_some_college / 100,
    
    # Children in Poverty percentages
    pct_child_poverty = pct_child_poverty / 100,
    pct_child_poverty_aian = pct_child_poverty_aian / 100,
    pct_child_poverty_asian = pct_child_poverty_asian / 100,
    pct_child_poverty_black = pct_child_poverty_black / 100,
    pct_child_poverty_hispanic = pct_child_poverty_hispanic / 100,
    pct_child_poverty_white = pct_child_poverty_white / 100
  )

# Calculate the total change from 2014 to 2024
state_trends %>%
  filter(year %in% c(2014, 2024)) %>%
  select(year, pct_lbw, tbr, pct_hs_complete, pct_child_poverty) %>%
  summarise(
    lbw_change = pct_lbw[year == 2024] - pct_lbw[year == 2014],
    tbr_change = tbr[year == 2024] - tbr[year == 2014], 
    hs_change = pct_hs_complete[year == 2024] - pct_hs_complete[year == 2014],
    poverty_change = pct_child_poverty[year == 2024] - pct_child_poverty[year == 2014]
  )

# Quick verification of Tableau-ready data
AL14_24SM2_tableau %>%
  filter(county == "State Total") %>%
  select(year, county, pct_lbw, tbr, pct_hs_complete, pct_child_poverty) %>%
  head(3)

##indexing
# Pre-calculate percent changes for all counties
AL14_24SM2_indexed <- AL14_24SM2_tableau %>%
  group_by(county) %>%
  arrange(year) %>%
  mutate(
    lbw_pct_change = (pct_lbw - first(pct_lbw)) / first(pct_lbw),
    hs_pct_change = (pct_hs_complete - first(pct_hs_complete)) / first(pct_hs_complete),
    poverty_pct_change = (pct_child_poverty - first(pct_child_poverty)) / first(pct_child_poverty),
    tbr_pct_change = (tbr - first(tbr)) / first(tbr)
  ) %>%
  ungroup()

# Load demographic analysis results from previous step
demographic_trends_detailed <- read_csv("outputs/tables/demographic_trends_detailed.csv")
comprehensive_race_trends <- read_csv("outputs/tables/comprehensive_race_trends.csv")
tableau_disparities_enhanced <- read_csv("outputs/tables/tableau_disparities_enhanced.csv")

# Export all Tableau-ready datasets
write_csv(AL14_24SM2_tableau, "outputs/tableau/Al14_24SM2_Tableau.csv")
write_csv(AL14_24SM2_indexed, "outputs/tableau/Al14_24SM2_Indexed.csv")
write_csv(demographic_trends_detailed, "outputs/tableau/AL_Demo_Trends_18_24_Enhanced_TBR_Tableau.csv")
write_csv(tableau_disparities_enhanced, "outputs/tableau/Alabama_County_Disparities_Enhanced_with_TBR.csv")
write_csv(comprehensive_race_trends, "outputs/tableau/AL14_24SM2_Comprehensive_Race_Trends.csv")
