# Alabama Health Disparities Analysis
# Script: 02_data_standardization.R
# Author: Shelita Smith
# Date: June 20, 2025
# Purpose: Standardize variables across all years and create unified dataset

library(tidyverse)

# Load cleaned individual datasets (from 01_data_cleaning.R output)
# [Include your variable standardization code here - all the AL24SM2_, AL23SM2_, etc. sections]

###2024 
AL24SM2_ <- AL24SM2 %>% 
  select(
    # Basic identifiers
    year, county, 
    
    # Low Birth Weight (*racial data available)
    pct_lbw = percent_low_birthweight,
    pct_lbw_aian = percent_lbw_non_hispanic_aian,
    pct_lbw_asian = percent_lbw_non_hispanic_asian,
    pct_lbw_biracial = percent_lbw_non_hispanic_2_races,
    pct_lbw_black = percent_lbw_non_hispanic_black,
    pct_lbw_hispanic = percent_lbw_hispanic_all_races,
    pct_lbw_nhopi = percent_lbw_non_hispanic_native_hawaiian_and_other_pacific_islander,
    pct_lbw_white = percent_lbw_non_hispanic_white,
    
    # Teen Birth Rate (*racial data available)
    tbr = teen_birth_rate,
    tbr_aian = teen_birth_rate_non_hispanic_aian,
    tbr_asian = teen_birth_rate_non_hispanic_asian,
    tbr_biracial = teen_birth_rate_non_hispanic_2_races,
    tbr_black = teen_birth_rate_non_hispanic_black,
    tbr_hispanic = teen_birth_rate_hispanic_all_races,
    tbr_nhopi = teen_birth_rate_non_hispanic_native_hawaiian_and_other_pacific_islander,
    tbr_white = teen_birth_rate_non_hispanic_white,
    
    # Education (no racial breakdown available)
    num_hs_complete = number_completed_high_school,
    pop_25plus = population,
    pct_hs_complete = percent_completed_high_school,
    num_some_college = number_some_college,
    pop_25_44 = population_2,
    pct_some_college = percent_some_college,
    
    # Children in Poverty (*racial data available)
    pct_child_poverty = percent_children_in_poverty,
    pct_child_poverty_aian = percent_children_in_poverty_aian,
    pct_child_poverty_asian = percent_children_in_poverty_asian,
    pct_child_poverty_black = percent_children_in_poverty_black,
    pct_child_poverty_hispanic = percent_children_in_poverty_hispanic,
    pct_child_poverty_white = percent_children_in_poverty_white
  )

###2023
AL23SM2_ <- AL23SM2 %>% 
  select(
    year, county, 
    pct_lbw = percent_low_birthweight,
    pct_lbw_aian = percent_lbw_aian,
    pct_lbw_asian = percent_lbw_asian,
    pct_lbw_black = percent_lbw_black,
    pct_lbw_hispanic = percent_lbw_hispanic,
    pct_lbw_white = percent_lbw_white,
    
    tbr = teen_birth_rate,
    tbr_aian = teen_birth_rate_aian,
    tbr_asian = teen_birth_rate_asian,
    tbr_black = teen_birth_rate_black,
    tbr_hispanic = teen_birth_rate_hispanic,
    tbr_white = teen_birth_rate_white,
    
    num_hs_complete = number_completed_high_school,
    pop_25plus = population,
    pct_hs_complete = percent_completed_high_school,
    num_some_college = number_some_college,
    pop_25_44 = population_2,
    pct_some_college = percent_some_college,
    
    pct_child_poverty = percent_children_in_poverty,
    pct_child_poverty_aian = percent_children_in_poverty_aian,
    pct_child_poverty_asian = percent_children_in_poverty_asian,
    pct_child_poverty_black = percent_children_in_poverty_black,
    pct_child_poverty_hispanic = percent_children_in_poverty_hispanic,
    pct_child_poverty_white = percent_children_in_poverty_white
  ) %>%
  mutate(
    pct_lbw_biracial = NA, pct_lbw_nhopi = NA,
    tbr_biracial = NA, tbr_nhopi = NA
  ) %>%
  select(
    year, county,
    pct_lbw, pct_lbw_aian, pct_lbw_asian, pct_lbw_biracial, pct_lbw_black, 
    pct_lbw_hispanic, pct_lbw_nhopi, pct_lbw_white,
    tbr, tbr_aian, tbr_asian, tbr_biracial, tbr_black, 
    tbr_hispanic, tbr_nhopi, tbr_white,
    num_hs_complete, pop_25plus, pct_hs_complete,
    num_some_college, pop_25_44, pct_some_college,
    pct_child_poverty, pct_child_poverty_aian, pct_child_poverty_asian,
    pct_child_poverty_black, pct_child_poverty_hispanic, pct_child_poverty_white
  )

###2022
AL22SM2_ <- AL22SM2 %>% 
  select(
    year, county, 
    pct_lbw = percent_low_birthweight,
    pct_lbw_aian = percent_lbw_aian,
    pct_lbw_asian = percent_lbw_asian,
    pct_lbw_black = percent_lbw_black,
    pct_lbw_hispanic = percent_lbw_hispanic,
    pct_lbw_white = percent_lbw_white,
    
    tbr = teen_birth_rate,
    tbr_aian = teen_birth_rate_aian,
    tbr_asian = teen_birth_rate_asian,
    tbr_black = teen_birth_rate_black,
    tbr_hispanic = teen_birth_rate_hispanic,
    tbr_white = teen_birth_rate_white,
    
    num_hs_complete = number_completed_high_school,
    pop_25plus = population,
    pct_hs_complete = percent_completed_high_school,
    num_some_college = number_some_college,
    pop_25_44 = population_2,
    pct_some_college = percent_some_college,
    
    pct_child_poverty = percent_children_in_poverty,
    pct_child_poverty_aian = percent_children_in_poverty_aian,
    pct_child_poverty_asian = percent_children_in_poverty_asian,
    pct_child_poverty_black = percent_children_in_poverty_black,
    pct_child_poverty_hispanic = percent_children_in_poverty_hispanic,
    pct_child_poverty_white = percent_children_in_poverty_white
  ) %>%
  mutate(
    pct_lbw_biracial = NA, pct_lbw_nhopi = NA,
    tbr_biracial = NA, tbr_nhopi = NA
  ) %>%
  select(
    year, county,
    pct_lbw, pct_lbw_aian, pct_lbw_asian, pct_lbw_biracial, pct_lbw_black, 
    pct_lbw_hispanic, pct_lbw_nhopi, pct_lbw_white,
    tbr, tbr_aian, tbr_asian, tbr_biracial, tbr_black, 
    tbr_hispanic, tbr_nhopi, tbr_white,
    num_hs_complete, pop_25plus, pct_hs_complete,
    num_some_college, pop_25_44, pct_some_college,
    pct_child_poverty, pct_child_poverty_aian, pct_child_poverty_asian,
    pct_child_poverty_black, pct_child_poverty_hispanic, pct_child_poverty_white
  )

###2021
AL21SM2_ <- AL21SM2 %>% 
  select(
    year, county, 
    pct_lbw = percent_low_birthweight,
    pct_lbw_aian = percent_lbw_aian,
    pct_lbw_asian = percent_lbw_asian,
    pct_lbw_black = percent_lbw_black,
    pct_lbw_hispanic = percent_lbw_hispanic,
    pct_lbw_white = percent_lbw_white,
    
    tbr = teen_birth_rate,
    tbr_aian = teen_birth_rate_aian,
    tbr_asian = teen_birth_rate_asian,
    tbr_black = teen_birth_rate_black,
    tbr_hispanic = teen_birth_rate_hispanic,
    tbr_white = teen_birth_rate_white,
    
    num_hs_complete = number_completed_high_school,
    pop_25plus = population,
    pct_hs_complete = percent_completed_high_school,
    num_some_college = number_some_college,
    pop_25_44 = population_2,
    pct_some_college = percent_some_college,
    
    pct_child_poverty = percent_children_in_poverty,
    pct_child_poverty_aian = percent_children_in_poverty_aian,
    pct_child_poverty_asian = percent_children_in_poverty_asian,
    pct_child_poverty_black = percent_children_in_poverty_black,
    pct_child_poverty_hispanic = percent_children_in_poverty_hispanic,
    pct_child_poverty_white = percent_children_in_poverty_white
  ) %>%
  mutate(
    pct_lbw_biracial = NA, pct_lbw_nhopi = NA,
    tbr_biracial = NA, tbr_nhopi = NA
  ) %>%
  select(
    year, county,
    pct_lbw, pct_lbw_aian, pct_lbw_asian, pct_lbw_biracial, pct_lbw_black, 
    pct_lbw_hispanic, pct_lbw_nhopi, pct_lbw_white,
    tbr, tbr_aian, tbr_asian, tbr_biracial, tbr_black, 
    tbr_hispanic, tbr_nhopi, tbr_white,
    num_hs_complete, pop_25plus, pct_hs_complete,
    num_some_college, pop_25_44, pct_some_college,
    pct_child_poverty, pct_child_poverty_aian, pct_child_poverty_asian,
    pct_child_poverty_black, pct_child_poverty_hispanic, pct_child_poverty_white
  )

###2020
AL20SM2_ <- AL20SM2 %>% 
  select(
    year, county, 
    pct_lbw = percent_low_birthweight,
    pct_lbw_aian = percent_lbw_aian,
    pct_lbw_asian = percent_lbw_asian,
    pct_lbw_black = percent_lbw_black,
    pct_lbw_hispanic = percent_lbw_hispanic,
    pct_lbw_white = percent_lbw_white,
    
    tbr = teen_birth_rate,
    tbr_aian = teen_birth_rate_aian,
    tbr_asian = teen_birth_rate_asian,
    tbr_black = teen_birth_rate_black,
    tbr_hispanic = teen_birth_rate_hispanic,
    tbr_white = teen_birth_rate_white,
    
    num_hs_complete = cohort_size,
    pop_25plus = population,
    pct_hs_complete = high_school_graduation_rate,
    num_some_college = number_some_college,
    pop_25_44 = population,
    pct_some_college = percent_some_college,
    
    pct_child_poverty = percent_children_in_poverty,
    pct_child_poverty_aian = percent_children_in_poverty_aian,
    pct_child_poverty_asian = percent_children_in_poverty_asian,
    pct_child_poverty_black = percent_children_in_poverty_black,
    pct_child_poverty_hispanic = percent_children_in_poverty_hispanic,
    pct_child_poverty_white = percent_children_in_poverty_white
  ) %>%
  mutate(
    pct_lbw_biracial = NA, pct_lbw_nhopi = NA,
    tbr_biracial = NA, tbr_nhopi = NA
  ) %>%
  select(
    year, county,
    pct_lbw, pct_lbw_aian, pct_lbw_asian, pct_lbw_biracial, pct_lbw_black, 
    pct_lbw_hispanic, pct_lbw_nhopi, pct_lbw_white,
    tbr, tbr_aian, tbr_asian, tbr_biracial, tbr_black, 
    tbr_hispanic, tbr_nhopi, tbr_white,
    num_hs_complete, pop_25plus, pct_hs_complete,
    num_some_college, pop_25_44, pct_some_college,
    pct_child_poverty, pct_child_poverty_aian, pct_child_poverty_asian,
    pct_child_poverty_black, pct_child_poverty_hispanic, pct_child_poverty_white
  )

###2019
AL19SM2_ <- AL19SM2 %>% 
  select(
    year, county, 
    pct_lbw = percent_lbw,
    pct_lbw_black = percent_lbw_black,
    pct_lbw_hispanic = percent_lbw_hispanic,
    pct_lbw_white = percent_lbw_white,
    
    tbr = teen_birth_rate,
    tbr_black = teen_birth_rate_black,
    tbr_hispanic = teen_birth_rate_hispanic,
    tbr_white = teen_birth_rate_white,
    
    num_hs_complete = cohort_size,
    pop_25plus = population,
    pct_hs_complete = graduation_rate,
    num_some_college = number_some_college,
    pop_25_44 = population,
    pct_some_college = percent_some_college,
    
    pct_child_poverty = percent_children_in_poverty,
    pct_child_poverty_black = percent_children_in_poverty_black,
    pct_child_poverty_hispanic = percent_children_in_poverty_hispanic,
    pct_child_poverty_white = percent_children_in_poverty_white
  ) %>%
  mutate(
    pct_lbw_aian = NA, pct_lbw_asian = NA, pct_lbw_biracial = NA, pct_lbw_nhopi = NA,
    tbr_aian = NA, tbr_asian = NA, tbr_biracial = NA, tbr_nhopi = NA,
    pct_child_poverty_aian = NA, pct_child_poverty_asian = NA
  ) %>%
  select(
    year, county,
    pct_lbw, pct_lbw_aian, pct_lbw_asian, pct_lbw_biracial, pct_lbw_black, 
    pct_lbw_hispanic, pct_lbw_nhopi, pct_lbw_white,
    tbr, tbr_aian, tbr_asian, tbr_biracial, tbr_black, 
    tbr_hispanic, tbr_nhopi, tbr_white,
    num_hs_complete, pop_25plus, pct_hs_complete,
    num_some_college, pop_25_44, pct_some_college,
    pct_child_poverty, pct_child_poverty_aian, pct_child_poverty_asian,
    pct_child_poverty_black, pct_child_poverty_hispanic, pct_child_poverty_white
  )

###2018
AL18SM2_ <- AL18SM2 %>% 
  select(
    year, county, 
    pct_lbw = percent_lbw,
    pct_lbw_black = percent_lbw_black,
    pct_lbw_hispanic = percent_lbw_hispanic,
    pct_lbw_white = percent_lbw_white,
    
    tbr = teen_birth_rate,
    tbr_black = teen_birth_rate_black,
    tbr_hispanic = teen_birth_rate_hispanic,
    tbr_white = teen_birth_rate_white,
    
    num_hs_complete = cohort_size,
    pop_25plus = population,
    pct_hs_complete = graduation_rate,
    num_some_college = number_some_college,
    pop_25_44 = population,
    pct_some_college = percent_some_college,
    
    pct_child_poverty = percent_children_in_poverty,
    pct_child_poverty_black = percent_children_in_poverty_black,
    pct_child_poverty_hispanic = percent_children_in_poverty_hispanic,
    pct_child_poverty_white = percent_children_in_poverty_white
  ) %>%
  mutate(
    pct_lbw_aian = NA, pct_lbw_asian = NA, pct_lbw_biracial = NA, pct_lbw_nhopi = NA,
    tbr_aian = NA, tbr_asian = NA, tbr_biracial = NA, tbr_nhopi = NA,
    pct_child_poverty_aian = NA, pct_child_poverty_asian = NA
  ) %>%
  select(
    year, county,
    pct_lbw, pct_lbw_aian, pct_lbw_asian, pct_lbw_biracial, pct_lbw_black, 
    pct_lbw_hispanic, pct_lbw_nhopi, pct_lbw_white,
    tbr, tbr_aian, tbr_asian, tbr_biracial, tbr_black, 
    tbr_hispanic, tbr_nhopi, tbr_white,
    num_hs_complete, pop_25plus, pct_hs_complete,
    num_some_college, pop_25_44, pct_some_college,
    pct_child_poverty, pct_child_poverty_aian, pct_child_poverty_asian,
    pct_child_poverty_black, pct_child_poverty_hispanic, pct_child_poverty_white
  )

###2017
AL17SM2_ <- AL17SM2 %>% 
  select(
    year, county, 
    pct_lbw = percent_lbw,
    tbr = teen_birth_rate,
    
    num_hs_complete = cohort_size,
    pop_25plus = population,
    pct_hs_complete = graduation_rate,
    num_some_college = number_some_college,
    pop_25_44 = population,
    pct_some_college = percent_some_college,
    
    pct_child_poverty = percent_children_in_poverty,
    pct_child_poverty_black = percent_children_in_poverty_black,
    pct_child_poverty_hispanic = percent_children_in_poverty_hispanic,
    pct_child_poverty_white = percent_children_in_poverty_white
  ) %>%
  mutate(
    pct_lbw_aian = NA, pct_lbw_asian = NA, pct_lbw_biracial = NA, pct_lbw_black = NA,
    pct_lbw_hispanic = NA, pct_lbw_nhopi = NA, pct_lbw_white = NA,
    tbr_aian = NA, tbr_asian = NA, tbr_biracial = NA, tbr_black = NA,
    tbr_hispanic = NA, tbr_nhopi = NA, tbr_white = NA,
    pct_child_poverty_aian = NA, pct_child_poverty_asian = NA
  ) %>%
  select(
    year, county,
    pct_lbw, pct_lbw_aian, pct_lbw_asian, pct_lbw_biracial, pct_lbw_black, 
    pct_lbw_hispanic, pct_lbw_nhopi, pct_lbw_white,
    tbr, tbr_aian, tbr_asian, tbr_biracial, tbr_black, 
    tbr_hispanic, tbr_nhopi, tbr_white,
    num_hs_complete, pop_25plus, pct_hs_complete,
    num_some_college, pop_25_44, pct_some_college,
    pct_child_poverty, pct_child_poverty_aian, pct_child_poverty_asian,
    pct_child_poverty_black, pct_child_poverty_hispanic, pct_child_poverty_white
  )

###2016
AL16SM2_ <- AL16SM2 %>% 
  select(
    year, county, 
    pct_lbw = percent_lbw,
    tbr = teen_birth_rate,
    
    num_hs_complete = cohort_size,
    pop_25plus = population,
    pct_hs_complete = graduation_rate,
    num_some_college = number_some_college,
    pop_25_44 = population,
    pct_some_college = percent_some_college,
    
    pct_child_poverty = percent_children_in_poverty
  ) %>%
  mutate(
    pct_lbw_aian = NA, pct_lbw_asian = NA, pct_lbw_biracial = NA, pct_lbw_black = NA,
    pct_lbw_hispanic = NA, pct_lbw_nhopi = NA, pct_lbw_white = NA,
    tbr_aian = NA, tbr_asian = NA, tbr_biracial = NA, tbr_black = NA,
    tbr_hispanic = NA, tbr_nhopi = NA, tbr_white = NA,
    pct_child_poverty_aian = NA, pct_child_poverty_asian = NA, pct_child_poverty_black = NA,
    pct_child_poverty_hispanic = NA, pct_child_poverty_white = NA
  ) %>%
  select(
    year, county,
    pct_lbw, pct_lbw_aian, pct_lbw_asian, pct_lbw_biracial, pct_lbw_black, 
    pct_lbw_hispanic, pct_lbw_nhopi, pct_lbw_white,
    tbr, tbr_aian, tbr_asian, tbr_biracial, tbr_black, 
    tbr_hispanic, tbr_nhopi, tbr_white,
    num_hs_complete, pop_25plus, pct_hs_complete,
    num_some_college, pop_25_44, pct_some_college,
    pct_child_poverty, pct_child_poverty_aian, pct_child_poverty_asian,
    pct_child_poverty_black, pct_child_poverty_hispanic, pct_child_poverty_white
  )

###2015
AL15SM2_ <- AL15SM2 %>% 
  select(
    year, county, 
    pct_lbw = percent_lbw,
    tbr = teen_birth_rate,
    
    num_hs_complete = cohort_size,
    pop_25plus = population,
    pct_hs_complete = graduation_rate,
    num_some_college = number_some_college,
    pop_25_44 = population,
    pct_some_college = percent_some_college,
    
    pct_child_poverty = percent_children_in_poverty
  ) %>%
  mutate(
    pct_lbw_aian = NA, pct_lbw_asian = NA, pct_lbw_biracial = NA, pct_lbw_black = NA,
    pct_lbw_hispanic = NA, pct_lbw_nhopi = NA, pct_lbw_white = NA,
    tbr_aian = NA, tbr_asian = NA, tbr_biracial = NA, tbr_black = NA,
    tbr_hispanic = NA, tbr_nhopi = NA, tbr_white = NA,
    pct_child_poverty_aian = NA, pct_child_poverty_asian = NA, pct_child_poverty_black = NA,
    pct_child_poverty_hispanic = NA, pct_child_poverty_white = NA
  ) %>%
  select(
    year, county,
    pct_lbw, pct_lbw_aian, pct_lbw_asian, pct_lbw_biracial, pct_lbw_black, 
    pct_lbw_hispanic, pct_lbw_nhopi, pct_lbw_white,
    tbr, tbr_aian, tbr_asian, tbr_biracial, tbr_black, 
    tbr_hispanic, tbr_nhopi, tbr_white,
    num_hs_complete, pop_25plus, pct_hs_complete,
    num_some_college, pop_25_44, pct_some_college,
    pct_child_poverty, pct_child_poverty_aian, pct_child_poverty_asian,
    pct_child_poverty_black, pct_child_poverty_hispanic, pct_child_poverty_white
  )

###2014
AL14SM2_ <- AL14SM2 %>% 
  select(
    year, county, 
    pct_lbw = percent_lbw,
    tbr = teen_birth_rate,
    
    num_hs_complete = cohort_size,
    pop_25plus = population,
    pct_hs_complete = graduation_rate,
    num_some_college = number_some_college,
    pop_25_44 = population,
    pct_some_college = percent_some_college,
    
    pct_child_poverty = percent_children_in_poverty
  ) %>%
  mutate(
    pct_lbw_aian = NA, pct_lbw_asian = NA, pct_lbw_biracial = NA, pct_lbw_black = NA,
    pct_lbw_hispanic = NA, pct_lbw_nhopi = NA, pct_lbw_white = NA,
    tbr_aian = NA, tbr_asian = NA, tbr_biracial = NA, tbr_black = NA,
    tbr_hispanic = NA, tbr_nhopi = NA, tbr_white = NA,
    pct_child_poverty_aian = NA, pct_child_poverty_asian = NA, pct_child_poverty_black = NA,
    pct_child_poverty_hispanic = NA, pct_child_poverty_white = NA
  ) %>%
  select(
    year, county,
    pct_lbw, pct_lbw_aian, pct_lbw_asian, pct_lbw_biracial, pct_lbw_black, 
    pct_lbw_hispanic, pct_lbw_nhopi, pct_lbw_white,
    tbr, tbr_aian, tbr_asian, tbr_biracial, tbr_black, 
    tbr_hispanic, tbr_nhopi, tbr_white,
    num_hs_complete, pop_25plus, pct_hs_complete,
    num_some_college, pop_25_44, pct_some_college,
    pct_child_poverty, pct_child_poverty_aian, pct_child_poverty_asian,
    pct_child_poverty_black, pct_child_poverty_hispanic, pct_child_poverty_white
  )

##Creating Main table and then onto analysis

# Convert year column issue
AL14SM2_ <- AL14SM2_ %>% mutate(year = as.character(year))
AL15SM2_ <- AL15SM2_ %>% mutate(year = as.character(year))
AL16SM2_ <- AL16SM2_ %>% mutate(year = as.character(year))
AL17SM2_ <- AL17SM2_ %>% mutate(year = as.character(year))
AL18SM2_ <- AL18SM2_ %>% mutate(year = as.character(year))
AL19SM2_ <- AL19SM2_ %>% mutate(year = as.character(year))
AL20SM2_ <- AL20SM2_ %>% mutate(year = as.character(year))
AL21SM2_ <- AL21SM2_ %>% mutate(year = as.character(year))
AL22SM2_ <- AL22SM2_ %>% mutate(year = as.character(year))
AL23SM2_ <- AL23SM2_ %>% mutate(year = as.character(year))
AL24SM2_ <- AL24SM2_ %>% mutate(year = as.character(year))

AL14_24SM2 <- bind_rows(
  AL14SM2_, AL15SM2_, AL16SM2_, AL17SM2_, AL18SM2_, AL19SM2_,
  AL20SM2_, AL21SM2_, AL22SM2_, AL23SM2_, AL24SM2_
)

# Convert data types for proper analysis
AL14_24SM2 <- AL14_24SM2 %>%
  mutate(
    # Convert year to numeric (for trend analysis, plotting)
    year = as.numeric(year),
    county = as.character(county),
    
    # Convert all percentage and rate variables to numeric
    # Low Birth Weight variables
    pct_lbw = as.numeric(pct_lbw),
    pct_lbw_aian = as.numeric(pct_lbw_aian),
    pct_lbw_asian = as.numeric(pct_lbw_asian),
    pct_lbw_biracial = as.numeric(pct_lbw_biracial),
    pct_lbw_black = as.numeric(pct_lbw_black),
    pct_lbw_hispanic = as.numeric(pct_lbw_hispanic),
    pct_lbw_nhopi = as.numeric(pct_lbw_nhopi),
    pct_lbw_white = as.numeric(pct_lbw_white),
    
    # Teen Birth Rate variables
    tbr = as.numeric(tbr),
    tbr_aian = as.numeric(tbr_aian),
    tbr_asian = as.numeric(tbr_asian),
    tbr_biracial = as.numeric(tbr_biracial),
    tbr_black = as.numeric(tbr_black),
    tbr_hispanic = as.numeric(tbr_hispanic),
    tbr_nhopi = as.numeric(tbr_nhopi),
    tbr_white = as.numeric(tbr_white),
    
    # Education count variables
    num_hs_complete = as.numeric(num_hs_complete),
    pop_25plus = as.numeric(pop_25plus),
    num_some_college = as.numeric(num_some_college),
    pop_25_44 = as.numeric(pop_25_44),
    
    # Education percentage variables
    pct_hs_complete = as.numeric(pct_hs_complete),
    pct_some_college = as.numeric(pct_some_college),
    
    # Children in Poverty variables
    pct_child_poverty = as.numeric(pct_child_poverty),
    pct_child_poverty_aian = as.numeric(pct_child_poverty_aian),
    pct_child_poverty_asian = as.numeric(pct_child_poverty_asian),
    pct_child_poverty_black = as.numeric(pct_child_poverty_black),
    pct_child_poverty_hispanic = as.numeric(pct_child_poverty_hispanic),
    pct_child_poverty_white = as.numeric(pct_child_poverty_white)
  )

# Combine all standardized datasets
AL14_24SM2 <- bind_rows(
  AL14SM2_, AL15SM2_, AL16SM2_, AL17SM2_, AL18SM2_, AL19SM2_,
  AL20SM2_, AL21SM2_, AL22SM2_, AL23SM2_, AL24SM2_
)

# Convert data types for proper analysis
AL14_24SM2 <- AL14_24SM2 %>%
  mutate(
    # Convert year to numeric (for trend analysis, plotting)
    year = as.numeric(year),
    county = as.character(county),
    
    # [ALL YOUR TYPE CONVERSION CODE]
  )

# Save the unified dataset
write_csv(AL14_24SM2, "data/processed/AL14_24SM2_unified.csv")

cat("✅ Data standardization complete!\n")
