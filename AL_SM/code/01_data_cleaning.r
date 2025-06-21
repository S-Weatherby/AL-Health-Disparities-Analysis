# Alabama Health Disparities Analysis
# Script: 01_data_cleaning.R
# Author: Shelita Smith
# June 20, 2025 
# Purpose: Clean and standardize AL County Health Rankings data (2014-2024) 
# Variables: County, Year, Race, Teen Birth Rate (TBR), TBR_race, Low Birth Weight (LBW), LBW_race, Children in Poverty (CIP), CIP_race, Education (Highschool/Ged and Some College)

# =============================================================================
# PACKAGE LOADING
# =============================================================================

# Core tidyverse packages
library(tidyverse)     
library(janitor)       
library(readxl)        

# =============================================================================
# DATA IMPORT AND INITIAL CLEANING
# =============================================================================

file_info <- list(
  AL14SM2 = "data/raw/2014 County Health Rankings Alabama Data - v6.xls - Ranked Measure Data.csv",
  AL15SM2 = "data/raw/2015 County Health Rankings Alabama Data - v3.xls - Ranked Measure Data.csv",
  AL16SM2 = "data/raw/2016 County Health Rankings Alabama Data - v3.xlsx - Ranked Measure Data.csv",
  AL17SM2 = "data/raw/2017 County Health Rankings Alabama Data - v2.xls - Ranked Measure Data.csv",
  AL18SM2 = "data/raw/2018 County Health Rankings Alabama Data - v3.xlsx - Ranked Measure Data.csv",
  AL19SM2 = "data/raw/2019 County Health Rankings Alabama Data - v1_0.xls - Ranked Measure Data.csv",
  AL20SM2 = "data/raw/2020 County Health Rankings Alabama Data - v1_1.xlsx - Ranked Measure Data.csv",
  AL21SM2 = "data/raw/2021 County Health Rankings Alabama Data - v1_0.xlsx - Ranked Measure Data.csv",
  AL22SM2 = "data/raw/2022 County Health Rankings Alabama Data - v2.xlsx - Ranked Measure Data.csv",
  AL23SM2 = "data/raw/2023 County Health Rankings Alabama Data - v3.xlsx - Ranked Measure Data.csv",
  AL24SM2 = "data/raw/2024 County Health Rankings Alabama Data - v2.xlsx - Select Measure Data.csv"
)

# Function to read and clean each file
clean_al_data <- function(file_path, data_name) {
  cat(paste("Processing", data_name, "...\n"))
  
  
  data <- read.csv(file_path)
  
  # Extract row 1 as column names and clean them
  new_col_names <- data[1, ] %>% 
    as.character() %>% 
    make_clean_names()
  
  # Set the cleaned names as column names
  colnames(data) <- new_col_names
  
  # Remove the first row since it's now the header
  data <- data[-1, ]
  
  cat(paste("  Cleaned", data_name, "- Dimensions:", nrow(data), "x", ncol(data), "\n"))
  
  return(data)
}

# Create individual cleaned datasets
cat("\n=== CREATING INDIVIDUAL OBJECTS ===\n")
for (name in names(file_info)) {
  assign(name, clean_al_data(file_info[[name]], name), envir = .GlobalEnv)
  cat(paste("Created object:", name, "\n"))
}

# Add State Total labels
AL14SM2[1, 3] <- "State Total"
AL15SM2[1, 3] <- "State Total"
AL16SM2[1, 4] <- "State Total"
AL17SM2[1, 3] <- "State Total"
AL18SM2[1, 4] <- "State Total"
AL19SM2[1, 3] <- "State Total"
AL20SM2[1, 4] <- "State Total"
AL21SM2[1, 3] <- "State Total"
AL22SM2[1, 4] <- "State Total"
AL23SM2[1, 3] <- "State Total"

# Add year columns
dataset_years <- list(
  AL14SM2 = 2014, AL15SM2 = 2015, AL16SM2 = 2016, AL17SM2 = 2017, AL18SM2 = 2018,
  AL19SM2 = 2019, AL20SM2 = 2020, AL21SM2 = 2021, AL22SM2 = 2022, AL23SM2 = 2023, AL24SM2 = 2024
)

# Function to add year column if it doesn't exist
add_year_column <- function(data, dataset_name, year) {
  cat(paste("Checking", dataset_name, "for year column...\n"))
  
  if ("year" %in% names(data)) {
    cat(paste("  ✅", dataset_name, "already has year column\n"))
    return(data)
  } else {
    cat(paste("  📅 Adding year", year, "to", dataset_name, "\n"))
    
    data <- data %>%
      mutate(year = year) %>%
      select(year, everything())
    
    cat(paste("  ✅ Year column added to", dataset_name, "\n"))
    return(data)
  }
}

# Apply to all datasets
cat("=== ADDING YEAR COLUMNS ===\n\n")

for (dataset_name in names(dataset_years)) {
  if (exists(dataset_name)) {
    current_data <- get(dataset_name)
    year <- dataset_years[[dataset_name]]
    updated_data <- add_year_column(current_data, dataset_name, year)
    assign(dataset_name, updated_data, envir = .GlobalEnv)
  } else {
    cat(paste("⚠️ ", dataset_name, "not found - skipping\n"))
  }
}

# Save cleaned individual datasets
cat("=== SAVING CLEANED DATASETS ===\n")
for (name in names(file_info)) {
  if (exists(name)) {
    write_csv(get(name), paste0("data/processed/", name, "_cleaned.csv"))
    cat(paste("Saved:", name, "_cleaned.csv\n"))
  }
}

cat("✅ Data cleaning complete!\n")
