#=================#
# i. Importing and Clealing Tables for later table setup
#=================#


AL21SM2 <- read.csv("C:\\Users\\\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2021 County Health Rankings Alabama Data - v1_0.xlsx - Ranked Measure Data.csv")
AL20SM2 <- read.csv("C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2020 County Health Rankings Alabama Data - v1_1.xlsx - Ranked Measure Data.csv")
AL19SM2 <- read.csv("C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2019 County Health Rankings Alabama Data - v1_0.xls - Ranked Measure Data.csv")
AL18SM2 <- read.csv("C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2018 County Health Rankings Alabama Data - v3.xlsx - Ranked Measure Data.csv")
AL17SM2 <- read.csv("C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2017 County Health Rankings Alabama Data - v2.xls - Ranked Measure Data.csv")
AL16SM2 <- read.csv("C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2016 County Health Rankings Alabama Data - v3.xlsx - Ranked Measure Data.csv")
AL15SM2 <- read.csv("C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2015 County Health Rankings Alabama Data - v3.xls - Ranked Measure Data.csv")
AL14SM2 <- read.csv("C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2014 County Health Rankings Alabama Data - v6.xls - Ranked Measure Data.csv")
AL24SM2 <- read.csv("C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2024 County Health Rankings Alabama Data - v2.xlsx - Select Measure Data.csv")
AL23SM2 <- read.csv("C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2023 County Health Rankings Alabama Data - v3.xlsx - Ranked Measure Data.csv")
AL22SM2 <- read.csv("C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2022 County Health Rankings Alabama Data - v2.xlsx - Ranked Measure Data.csv")


# Define file paths and corresponding object names
file_info <- list(
  AL14SM2 = "C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2014 County Health Rankings Alabama Data - v6.xls - Ranked Measure Data.csv",
  AL15SM2 = "C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2015 County Health Rankings Alabama Data - v3.xls - Ranked Measure Data.csv",
  AL16SM2 = "C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2016 County Health Rankings Alabama Data - v3.xlsx - Ranked Measure Data.csv",
  AL17SM2 = "C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2017 County Health Rankings Alabama Data - v2.xls - Ranked Measure Data.csv",
  AL18SM2 = "C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2018 County Health Rankings Alabama Data - v3.xlsx - Ranked Measure Data.csv",
  AL19SM2 = "C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2019 County Health Rankings Alabama Data - v1_0.xls - Ranked Measure Data.csv",
  AL20SM2 = "C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2020 County Health Rankings Alabama Data - v1_1.xlsx - Ranked Measure Data.csv",
  AL21SM2 = "C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2021 County Health Rankings Alabama Data - v1_0.xlsx - Ranked Measure Data.csv",
  AL22SM2 = "C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2022 County Health Rankings Alabama Data - v2.xlsx - Ranked Measure Data.csv",
  AL23SM2 = "C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2023 County Health Rankings Alabama Data - v3.xlsx - Ranked Measure Data.csv",
  AL24SM2 = "C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\R projects\\AL SM\\AL14_24SM2\\ALCountyHealthDataSets\\2024 County Health Rankings Alabama Data - v2.xlsx - Select Measure Data.csv"
)

# Function to read and clean each file
clean_al_data <- function(file_path, data_name) {
  cat(paste("Processing", data_name, "...\n"))
  
  # Read the CSV file
  data <- read.csv(file_path)
  
  # Extract row 1 as column names and clean them
  new_col_names <- data[1, ] %>% 
    as.character() %>% 
    make_clean_names()  # This removes spaces, special chars, makes lowercase
  
  # Set the cleaned names as column names
  colnames(data) <- new_col_names
  
  # Remove the first row since it's now the header
  data <- data[-1, ]
  
  cat(paste("  Cleaned", data_name, "- Dimensions:", nrow(data), "x", ncol(data), "\n"))
  
  return(data)
}

cat("\n=== CREATING INDIVIDUAL OBJECTS ===\n")
for (name in names(file_info)) {
  assign(name, clean_al_data(file_info[[name]], name), envir = .GlobalEnv)
  cat(paste("Created object:", name, "\n"))
}

# Simple way to add "State Total" to row 2, column 4 for all tables except AL24SM2

# Add State Total to each dataset
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
# AL24SM2 is skipped 

# Define dataset names and their corresponding years
dataset_years <- list(
  AL14SM2 = 2014,
  AL15SM2 = 2015,
  AL16SM2 = 2016,
  AL17SM2 = 2017,
  AL18SM2 = 2018,
  AL19SM2 = 2019,
  AL20SM2 = 2020,
  AL21SM2 = 2021,
  AL22SM2 = 2022,
  AL23SM2 = 2023,
  AL24SM2 = 2024
)

# Function to add year column if it doesn't exist
# Define dataset names and their corresponding years
dataset_years <- list(
  AL14SM2 = 2014,
  AL15SM2 = 2015,
  AL16SM2 = 2016,
  AL17SM2 = 2017,
  AL18SM2 = 2018,
  AL19SM2 = 2019,
  AL20SM2 = 2020,
  AL21SM2 = 2021,
  AL22SM2 = 2022,
  AL23SM2 = 2023,
  AL24SM2 = 2024
)

# Function to add year column if it doesn't exist
add_year_column <- function(data, dataset_name, year) {
  cat(paste("Checking", dataset_name, "for year column...\n"))
  
  # Check if year column already exists
  if ("year" %in% names(data)) {
    cat(paste("  ✅", dataset_name, "already has year column\n"))
    return(data)
  } else {
    cat(paste("  📅 Adding year", year, "to", dataset_name, "\n"))
    
    # Add year column as the first column
    data <- data %>%
      mutate(year = year) %>%
      select(year, everything())  # Move year to first position
    
    cat(paste("  ✅ Year column added to", dataset_name, "\n"))
    return(data)
  }
}

# Apply to all datasets
cat("=== ADDING YEAR COLUMNS ===\n\n")

for (dataset_name in names(dataset_years)) {
  if (exists(dataset_name)) {
    # Get the dataset and corresponding year
    current_data <- get(dataset_name)
    year <- dataset_years[[dataset_name]]
    
    # Add year column if needed
    updated_data <- add_year_column(current_data, dataset_name, year)
    
    # Update the dataset in global environment
    assign(dataset_name, updated_data, envir = .GlobalEnv)
    
  } else {
    cat(paste("⚠️ ", dataset_name, "not found - skipping\n"))
  }
}

cat("\n=== VERIFICATION ===\n")

# Verify that all datasets now have year columns
verify_year_columns <- function() {
  for (dataset_name in names(dataset_years)) {
    if (exists(dataset_name)) {
      data <- get(dataset_name)
      expected_year <- dataset_years[[dataset_name]]
      
      if ("year" %in% names(data)) {
        actual_year <- unique(data$year)[1]  # Get first unique value
        if (actual_year == expected_year) {
          cat(paste("✅", dataset_name, "- Year column present and correct:", actual_year, "\n"))
        } else {
          cat(paste("⚠️ ", dataset_name, "- Year column present but incorrect. Expected:", expected_year, "Found:", actual_year, "\n"))
        }
      } else {
        cat(paste("❌", dataset_name, "- Year column missing\n"))
      }
    }
  }
}

verify_year_columns()

print(names(AL14SM2)) ## oldest and sparsest tables
print(names(AL24SM2)) ##most recent and comprehensive table

## creating Tables

###2024 
AL24SM2_ <- AL24SM2 %>% 
  select(
    # Basic identifiers
    year, 
    county, 
    
    # Low Birth Weight (*racial data available)
    pct_lbw = percent_low_birthweight,                         # Overall rate
    pct_lbw_aian = percent_lbw_non_hispanic_aian,              # American Indian/Alaska Native  
    pct_lbw_asian = percent_lbw_non_hispanic_asian,            # Asian
    pct_lbw_biracial = percent_lbw_non_hispanic_2_races,       # Two or more races
    pct_lbw_black = percent_lbw_non_hispanic_black,            # Black
    pct_lbw_hispanic = percent_lbw_hispanic_all_races,         # Hispanic
    pct_lbw_nhopi = percent_lbw_non_hispanic_native_hawaiian_and_other_pacific_islander, # Native Hawaiian/Pacific Islander
    pct_lbw_white = percent_lbw_non_hispanic_white,            # White
    
    # Teen Birth Rate (*racial data available)
    tbr = teen_birth_rate,                                     # Overall rate
    tbr_aian = teen_birth_rate_non_hispanic_aian,             # American Indian/Alaska Native
    tbr_asian = teen_birth_rate_non_hispanic_asian,           # Asian
    tbr_biracial = teen_birth_rate_non_hispanic_2_races,      # Two or more races
    tbr_black = teen_birth_rate_non_hispanic_black,           # Black
    tbr_hispanic = teen_birth_rate_hispanic_all_races,        # Hispanic
    tbr_nhopi = teen_birth_rate_non_hispanic_native_hawaiian_and_other_pacific_islander, # Native Hawaiian/Pacific Islander
    tbr_white = teen_birth_rate_non_hispanic_white,           # White
    
    # Education (no racial breakdown available)
    num_hs_complete = number_completed_high_school,            # Adults age 25+ with high school diploma or equivalent
    pop_25plus = population,                                   # Adults age 25 and over (denominator for high school %)
    pct_hs_complete = percent_completed_high_school,           # % of adults age 25+ with high school diploma or equivalent
    num_some_college = number_some_college,                    # Adults age 25-44 with some post-secondary education
    pop_25_44 = population_2,                                  # Adults age 25-44 (denominator for some college %)
    pct_some_college = percent_some_college,                   # % of adults age 25-44 with some post-secondary education
    
    # Children in Poverty (*racial data available)
    pct_child_poverty = percent_children_in_poverty,           # Overall rate
    pct_child_poverty_aian = percent_children_in_poverty_aian, # American Indian/Alaska Native
    pct_child_poverty_asian = percent_children_in_poverty_asian, # Asian
    pct_child_poverty_black = percent_children_in_poverty_black, # Black
    pct_child_poverty_hispanic = percent_children_in_poverty_hispanic, # Hispanic
    pct_child_poverty_white = percent_children_in_poverty_white # White
  )

###2023
print(names(AL23SM2))
AL23SM2_ <- AL23SM2 %>% 
  select(
    # Basic identifiers
    year, 
    county, 
    
    # Low Birth Weight (*racial data available in 2023)
    pct_lbw = percent_low_birthweight,                         # Overall rate
    pct_lbw_aian = percent_lbw_aian,                           # American Indian/Alaska Native  
    pct_lbw_asian = percent_lbw_asian,                         # Asian
    pct_lbw_black = percent_lbw_black,                         # Black
    pct_lbw_hispanic = percent_lbw_hispanic,                   # Hispanic
    pct_lbw_white = percent_lbw_white,                         # White
    
    # Teen Birth Rate (*racial data available in 2023)
    tbr = teen_birth_rate,                                     # Overall rate
    tbr_aian = teen_birth_rate_aian,                           # American Indian/Alaska Native
    tbr_asian = teen_birth_rate_asian,                         # Asian
    tbr_black = teen_birth_rate_black,                         # Black
    tbr_hispanic = teen_birth_rate_hispanic,                   # Hispanic
    tbr_white = teen_birth_rate_white,                         # White
    
    # Education (no racial breakdown available)
    num_hs_complete = number_completed_high_school,            # Adults age 25+ with high school diploma or equivalent
    pop_25plus = population,                                   # Adults age 25 and over (denominator for high school %)
    pct_hs_complete = percent_completed_high_school,           # % of adults age 25+ with high school diploma or equivalent
    num_some_college = number_some_college,                    # Adults age 25-44 with some post-secondary education
    pop_25_44 = population_2,                                  # Adults age 25-44 (denominator for some college %)
    pct_some_college = percent_some_college,                   # % of adults age 25-44 with some post-secondary education
    
    # Children in Poverty (*racial data available in 2023)
    pct_child_poverty = percent_children_in_poverty,           # Overall rate
    pct_child_poverty_aian = percent_children_in_poverty_aian, # American Indian/Alaska Native
    pct_child_poverty_asian = percent_children_in_poverty_asian, # Asian
    pct_child_poverty_black = percent_children_in_poverty_black, # Black
    pct_child_poverty_hispanic = percent_children_in_poverty_hispanic, # Hispanic
    pct_child_poverty_white = percent_children_in_poverty_white # White
  ) %>%
  # Add missing demographic columns that exist in AL24SM2_ but not in AL23SM2
  mutate(
    # Missing LBW categories (not available in 2023)
    pct_lbw_biracial = NA,                                     # Two or more races
    pct_lbw_nhopi = NA,                                        # Native Hawaiian/Pacific Islander
    
    # Missing Teen Birth categories (not available in 2023)
    tbr_biracial = NA,                                         # Two or more races
    tbr_nhopi = NA                                             # Native Hawaiian/Pacific Islander
  ) %>%
  # Reorder columns to match AL24SM2_ structure exactly
  select(
    # Identifiers
    year, county,
    
    # Low Birth Weight (alphabetical order)
    pct_lbw, pct_lbw_aian, pct_lbw_asian, pct_lbw_biracial, pct_lbw_black, 
    pct_lbw_hispanic, pct_lbw_nhopi, pct_lbw_white,
    
    # Teen Birth Rate (alphabetical order)
    tbr, tbr_aian, tbr_asian, tbr_biracial, tbr_black, 
    tbr_hispanic, tbr_nhopi, tbr_white,
    
    # Education
    num_hs_complete, pop_25plus, pct_hs_complete,
    num_some_college, pop_25_44, pct_some_college,
    
    # Children in Poverty
    pct_child_poverty, pct_child_poverty_aian, pct_child_poverty_asian,
    pct_child_poverty_black, pct_child_poverty_hispanic, pct_child_poverty_white
  )

###2022
print(names(AL22SM2))
AL22SM2_ <- AL22SM2 %>% 
  select(
    # Basic identifiers
    year, 
    county, 
    
    # Low Birth Weight (*racial data available in 2022)
    pct_lbw = percent_low_birthweight,                         # Overall rate
    pct_lbw_aian = percent_lbw_aian,                           # American Indian/Alaska Native  
    pct_lbw_asian = percent_lbw_asian,                         # Asian
    pct_lbw_black = percent_lbw_black,                         # Black
    pct_lbw_hispanic = percent_lbw_hispanic,                   # Hispanic
    pct_lbw_white = percent_lbw_white,                         # White
    
    # Teen Birth Rate (*racial data available in 2022)
    tbr = teen_birth_rate,                                     # Overall rate
    tbr_aian = teen_birth_rate_aian,                           # American Indian/Alaska Native
    tbr_asian = teen_birth_rate_asian,                         # Asian
    tbr_black = teen_birth_rate_black,                         # Black
    tbr_hispanic = teen_birth_rate_hispanic,                   # Hispanic
    tbr_white = teen_birth_rate_white,                         # White
    
    # Education (no racial breakdown available)
    num_hs_complete = number_completed_high_school,            # Adults age 25+ with high school diploma or equivalent
    pop_25plus = population,                                   # Adults age 25 and over (denominator for high school %)
    pct_hs_complete = percent_completed_high_school,           # % of adults age 25+ with high school diploma or equivalent
    num_some_college = number_some_college,                    # Adults age 25-44 with some post-secondary education
    pop_25_44 = population_2,                                  # Adults age 25-44 (denominator for some college %)
    pct_some_college = percent_some_college,                   # % of adults age 25-44 with some post-secondary education
    
    # Children in Poverty (*racial data available in 2022)
    pct_child_poverty = percent_children_in_poverty,           # Overall rate
    pct_child_poverty_aian = percent_children_in_poverty_aian, # American Indian/Alaska Native
    pct_child_poverty_asian = percent_children_in_poverty_asian, # Asian
    pct_child_poverty_black = percent_children_in_poverty_black, # Black
    pct_child_poverty_hispanic = percent_children_in_poverty_hispanic, # Hispanic
    pct_child_poverty_white = percent_children_in_poverty_white # White
  ) %>%
  # Add missing demographic columns that exist in AL24SM2_ but not in AL22SM2
  mutate(
    # Missing LBW categories (not available in 2022)
    pct_lbw_biracial = NA,                                     # Two or more races
    pct_lbw_nhopi = NA,                                        # Native Hawaiian/Pacific Islander
    
    # Missing Teen Birth categories (not available in 2022)
    tbr_biracial = NA,                                         # Two or more races
    tbr_nhopi = NA                                             # Native Hawaiian/Pacific Islander
  ) %>%
  # Reorder columns to match AL24SM2_ structure exactly
  select(
    # Identifiers
    year, county,
    
    # Low Birth Weight (alphabetical order)
    pct_lbw, pct_lbw_aian, pct_lbw_asian, pct_lbw_biracial, pct_lbw_black, 
    pct_lbw_hispanic, pct_lbw_nhopi, pct_lbw_white,
    
    # Teen Birth Rate (alphabetical order)
    tbr, tbr_aian, tbr_asian, tbr_biracial, tbr_black, 
    tbr_hispanic, tbr_nhopi, tbr_white,
    
    # Education
    num_hs_complete, pop_25plus, pct_hs_complete,
    num_some_college, pop_25_44, pct_some_college,
    
    # Children in Poverty
    pct_child_poverty, pct_child_poverty_aian, pct_child_poverty_asian,
    pct_child_poverty_black, pct_child_poverty_hispanic, pct_child_poverty_white
  )

###2021
print(names(AL21SM2))
AL21SM2_ <- AL21SM2 %>% 
  select(
    # Basic identifiers
    year, 
    county, 
    
    # Low Birth Weight (*racial data available in 2021)
    pct_lbw = percent_low_birthweight,                         # Overall rate
    pct_lbw_aian = percent_lbw_aian,                           # American Indian/Alaska Native  
    pct_lbw_asian = percent_lbw_asian,                         # Asian
    pct_lbw_black = percent_lbw_black,                         # Black
    pct_lbw_hispanic = percent_lbw_hispanic,                   # Hispanic
    pct_lbw_white = percent_lbw_white,                         # White
    
    # Teen Birth Rate (*racial data available in 2021)
    tbr = teen_birth_rate,                                     # Overall rate
    tbr_aian = teen_birth_rate_aian,                           # American Indian/Alaska Native
    tbr_asian = teen_birth_rate_asian,                         # Asian
    tbr_black = teen_birth_rate_black,                         # Black
    tbr_hispanic = teen_birth_rate_hispanic,                   # Hispanic
    tbr_white = teen_birth_rate_white,                         # White
    
    # Education (no racial breakdown available)
    num_hs_complete = number_completed_high_school,            # Adults age 25+ with high school diploma or equivalent
    pop_25plus = population,                                   # Adults age 25 and over (denominator for high school %)
    pct_hs_complete = percent_completed_high_school,           # % of adults age 25+ with high school diploma or equivalent
    num_some_college = number_some_college,                    # Adults age 25-44 with some post-secondary education
    pop_25_44 = population_2,                                  # Adults age 25-44 (denominator for some college %)
    pct_some_college = percent_some_college,                   # % of adults age 25-44 with some post-secondary education
    
    # Children in Poverty (*racial data available in 2021)
    pct_child_poverty = percent_children_in_poverty,           # Overall rate
    pct_child_poverty_aian = percent_children_in_poverty_aian, # American Indian/Alaska Native
    pct_child_poverty_asian = percent_children_in_poverty_asian, # Asian
    pct_child_poverty_black = percent_children_in_poverty_black, # Black
    pct_child_poverty_hispanic = percent_children_in_poverty_hispanic, # Hispanic
    pct_child_poverty_white = percent_children_in_poverty_white # White
  ) %>%
  # Add missing demographic columns that exist in AL24SM2_ but not in AL21SM2
  mutate(
    # Missing LBW categories (not available in 2021)
    pct_lbw_biracial = NA,                                     # Two or more races
    pct_lbw_nhopi = NA,                                        # Native Hawaiian/Pacific Islander
    
    # Missing Teen Birth categories (not available in 2021)
    tbr_biracial = NA,                                         # Two or more races
    tbr_nhopi = NA                                             # Native Hawaiian/Pacific Islander
  ) %>%
  # Reorder columns to match AL24SM2_ structure exactly
  select(
    # Identifiers
    year, county,
    
    # Low Birth Weight (alphabetical order)
    pct_lbw, pct_lbw_aian, pct_lbw_asian, pct_lbw_biracial, pct_lbw_black, 
    pct_lbw_hispanic, pct_lbw_nhopi, pct_lbw_white,
    
    # Teen Birth Rate (alphabetical order)
    tbr, tbr_aian, tbr_asian, tbr_biracial, tbr_black, 
    tbr_hispanic, tbr_nhopi, tbr_white,
    
    # Education
    num_hs_complete, pop_25plus, pct_hs_complete,
    num_some_college, pop_25_44, pct_some_college,
    
    # Children in Poverty
    pct_child_poverty, pct_child_poverty_aian, pct_child_poverty_asian,
    pct_child_poverty_black, pct_child_poverty_hispanic, pct_child_poverty_white
  )

###2020
print(names(AL20SM2))
AL20SM2_ <- AL20SM2 %>% 
  select(
    # Basic identifiers
    year, 
    county, 
    
    # Low Birth Weight (*racial data available in 2020)
    pct_lbw = percent_low_birthweight,                         # Overall rate
    pct_lbw_aian = percent_lbw_aian,                           # American Indian/Alaska Native  
    pct_lbw_asian = percent_lbw_asian,                         # Asian
    pct_lbw_black = percent_lbw_black,                         # Black
    pct_lbw_hispanic = percent_lbw_hispanic,                   # Hispanic
    pct_lbw_white = percent_lbw_white,                         # White
    
    # Teen Birth Rate (*racial data available in 2020)
    tbr = teen_birth_rate,                                     # Overall rate
    tbr_aian = teen_birth_rate_aian,                           # American Indian/Alaska Native
    tbr_asian = teen_birth_rate_asian,                         # Asian
    tbr_black = teen_birth_rate_black,                         # Black
    tbr_hispanic = teen_birth_rate_hispanic,                   # Hispanic
    tbr_white = teen_birth_rate_white,                         # White
    
    # Education (different structure in 2020 - cohort-based)
    num_hs_complete = cohort_size,                             # Cohort expected to graduate
    pop_25plus = population,                                   # Population for some college calculation
    pct_hs_complete = high_school_graduation_rate,             # High school graduation rate
    num_some_college = number_some_college,                    # Adults age 25-44 with some post-secondary education
    pop_25_44 = population,                                    # Population (same as pop_25plus in 2020)
    pct_some_college = percent_some_college,                   # % of adults age 25-44 with some post-secondary education
    
    # Children in Poverty (*racial data available in 2020)
    pct_child_poverty = percent_children_in_poverty,           # Overall rate
    pct_child_poverty_aian = percent_children_in_poverty_aian, # American Indian/Alaska Native
    pct_child_poverty_asian = percent_children_in_poverty_asian, # Asian
    pct_child_poverty_black = percent_children_in_poverty_black, # Black
    pct_child_poverty_hispanic = percent_children_in_poverty_hispanic, # Hispanic
    pct_child_poverty_white = percent_children_in_poverty_white # White
  ) %>%
  # Add missing demographic columns
  mutate(
    pct_lbw_biracial = NA,                                     # Two or more races
    pct_lbw_nhopi = NA,                                        # Native Hawaiian/Pacific Islander
    tbr_biracial = NA,                                         # Two or more races
    tbr_nhopi = NA                                             # Native Hawaiian/Pacific Islander
  ) %>%
  # Reorder columns to match standard structure
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
print(names(AL19SM2))
AL19SM2_ <- AL19SM2 %>% 
  select(
    # Basic identifiers
    year, 
    county, 
    
    # Low Birth Weight (*limited racial data in 2019: Black, Hispanic, White only)
    pct_lbw = percent_lbw,                                     # Overall rate
    pct_lbw_black = percent_lbw_black,                         # Black
    pct_lbw_hispanic = percent_lbw_hispanic,                   # Hispanic
    pct_lbw_white = percent_lbw_white,                         # White
    
    # Teen Birth Rate (*limited racial data in 2019: Black, Hispanic, White only)
    tbr = teen_birth_rate,                                     # Overall rate
    tbr_black = teen_birth_rate_black,                         # Black
    tbr_hispanic = teen_birth_rate_hispanic,                   # Hispanic
    tbr_white = teen_birth_rate_white,                         # White
    
    # Education (cohort-based in 2019)
    num_hs_complete = cohort_size,                             # Cohort expected to graduate
    pop_25plus = population,                                   # Population for some college calculation
    pct_hs_complete = graduation_rate,                         # High school graduation rate
    num_some_college = number_some_college,                    # Adults with some post-secondary education
    pop_25_44 = population,                                    # Population (same as pop_25plus in 2019)
    pct_some_college = percent_some_college,                   # % with some post-secondary education
    
    # Children in Poverty (*limited racial data in 2019: Black, Hispanic, White only)
    pct_child_poverty = percent_children_in_poverty,           # Overall rate
    pct_child_poverty_black = percent_children_in_poverty_black, # Black
    pct_child_poverty_hispanic = percent_children_in_poverty_hispanic, # Hispanic
    pct_child_poverty_white = percent_children_in_poverty_white # White
  ) %>%
  # Add missing demographic columns (not available in 2019)
  mutate(
    pct_lbw_aian = NA,                                         # American Indian/Alaska Native
    pct_lbw_asian = NA,                                        # Asian
    pct_lbw_biracial = NA,                                     # Two or more races
    pct_lbw_nhopi = NA,                                        # Native Hawaiian/Pacific Islander
    tbr_aian = NA,                                             # American Indian/Alaska Native
    tbr_asian = NA,                                            # Asian
    tbr_biracial = NA,                                         # Two or more races
    tbr_nhopi = NA,                                            # Native Hawaiian/Pacific Islander
    pct_child_poverty_aian = NA,                               # American Indian/Alaska Native
    pct_child_poverty_asian = NA                               # Asian
  ) %>%
  # Reorder columns to match standard structure
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
print(names(AL18SM2))
AL18SM2_ <- AL18SM2 %>% 
  select(
    # Basic identifiers
    year, 
    county, 
    
    # Low Birth Weight (*limited racial data in 2018: Black, Hispanic, White only)
    pct_lbw = percent_lbw,                                     # Overall rate
    pct_lbw_black = percent_lbw_black,                         # Black
    pct_lbw_hispanic = percent_lbw_hispanic,                   # Hispanic
    pct_lbw_white = percent_lbw_white,                         # White
    
    # Teen Birth Rate (*limited racial data in 2018: Black, Hispanic, White only)
    tbr = teen_birth_rate,                                     # Overall rate
    tbr_black = teen_birth_rate_black,                         # Black
    tbr_hispanic = teen_birth_rate_hispanic,                   # Hispanic
    tbr_white = teen_birth_rate_white,                         # White
    
    # Education (cohort-based in 2018)
    num_hs_complete = cohort_size,                             # Cohort expected to graduate
    pop_25plus = population,                                   # Population for some college calculation
    pct_hs_complete = graduation_rate,                         # High school graduation rate
    num_some_college = number_some_college,                    # Adults with some post-secondary education
    pop_25_44 = population,                                    # Population (same as pop_25plus in 2018)
    pct_some_college = percent_some_college,                   # % with some post-secondary education
    
    # Children in Poverty (*limited racial data in 2018: Black, Hispanic, White only)
    pct_child_poverty = percent_children_in_poverty,           # Overall rate
    pct_child_poverty_black = percent_children_in_poverty_black, # Black
    pct_child_poverty_hispanic = percent_children_in_poverty_hispanic, # Hispanic
    pct_child_poverty_white = percent_children_in_poverty_white # White
  ) %>%
  # Add missing demographic columns (not available in 2018)
  mutate(
    pct_lbw_aian = NA,                                         # American Indian/Alaska Native
    pct_lbw_asian = NA,                                        # Asian
    pct_lbw_biracial = NA,                                     # Two or more races
    pct_lbw_nhopi = NA,                                        # Native Hawaiian/Pacific Islander
    tbr_aian = NA,                                             # American Indian/Alaska Native
    tbr_asian = NA,                                            # Asian
    tbr_biracial = NA,                                         # Two or more races
    tbr_nhopi = NA,                                            # Native Hawaiian/Pacific Islander
    pct_child_poverty_aian = NA,                               # American Indian/Alaska Native
    pct_child_poverty_asian = NA                               # Asian
  ) %>%
  # Reorder columns to match standard structure
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
print(names(AL17SM2))
AL17SM2_ <- AL17SM2 %>% 
  select(
    # Basic identifiers
    year, 
    county, 
    
    # Low Birth Weight (*no racial data in 2017)
    pct_lbw = percent_lbw,                                     # Overall rate only
    
    # Teen Birth Rate (*no racial data in 2017)
    tbr = teen_birth_rate,                                     # Overall rate only
    
    # Education (cohort-based in 2017)
    num_hs_complete = cohort_size,                             # Cohort expected to graduate
    pop_25plus = population,                                   # Population for some college calculation
    pct_hs_complete = graduation_rate,                         # High school graduation rate
    num_some_college = number_some_college,                    # Adults with some post-secondary education
    pop_25_44 = population,                                    # Population (same as pop_25plus in 2017)
    pct_some_college = percent_some_college,                   # % with some post-secondary education
    
    # Children in Poverty (*limited racial data in 2017: Black, Hispanic, White only)
    pct_child_poverty = percent_children_in_poverty,           # Overall rate
    pct_child_poverty_black = percent_children_in_poverty_black, # Black
    pct_child_poverty_hispanic = percent_children_in_poverty_hispanic, # Hispanic
    pct_child_poverty_white = percent_children_in_poverty_white # White
  ) %>%
  # Add missing demographic columns (not available in 2017)
  mutate(
    pct_lbw_aian = NA,                                         # All LBW racial categories missing
    pct_lbw_asian = NA,
    pct_lbw_biracial = NA,
    pct_lbw_black = NA,
    pct_lbw_hispanic = NA,
    pct_lbw_nhopi = NA,
    pct_lbw_white = NA,
    tbr_aian = NA,                                             # All TBR racial categories missing
    tbr_asian = NA,
    tbr_biracial = NA,
    tbr_black = NA,
    tbr_hispanic = NA,
    tbr_nhopi = NA,
    tbr_white = NA,
    pct_child_poverty_aian = NA,                               # Missing child poverty categories
    pct_child_poverty_asian = NA
  ) %>%
  # Reorder columns to match standard structure
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
print(names(AL16SM2))
AL16SM2_ <- AL16SM2 %>% 
  select(
    # Basic identifiers
    year, 
    county, 
    
    # Low Birth Weight (*no racial data in 2016)
    pct_lbw = percent_lbw,                                     # Overall rate only
    
    # Teen Birth Rate (*no racial data in 2016)
    tbr = teen_birth_rate,                                     # Overall rate only
    
    # Education (cohort-based in 2016)
    num_hs_complete = cohort_size,                             # Cohort expected to graduate
    pop_25plus = population,                                   # Population for some college calculation
    pct_hs_complete = graduation_rate,                         # High school graduation rate
    num_some_college = number_some_college,                    # Adults with some post-secondary education
    pop_25_44 = population,                                    # Population (same as pop_25plus in 2016)
    pct_some_college = percent_some_college,                   # % with some post-secondary education
    
    # Children in Poverty (*no racial data in 2016)
    pct_child_poverty = percent_children_in_poverty            # Overall rate only
  ) %>%
  # Add missing demographic columns (not available in 2016)
  mutate(
    pct_lbw_aian = NA,                                         # All LBW racial categories missing
    pct_lbw_asian = NA,
    pct_lbw_biracial = NA,
    pct_lbw_black = NA,
    pct_lbw_hispanic = NA,
    pct_lbw_nhopi = NA,
    pct_lbw_white = NA,
    tbr_aian = NA,                                             # All TBR racial categories missing
    tbr_asian = NA,
    tbr_biracial = NA,
    tbr_black = NA,
    tbr_hispanic = NA,
    tbr_nhopi = NA,
    tbr_white = NA,
    pct_child_poverty_aian = NA,                               # All child poverty racial categories missing
    pct_child_poverty_asian = NA,
    pct_child_poverty_black = NA,
    pct_child_poverty_hispanic = NA,
    pct_child_poverty_white = NA
  ) %>%
  # Reorder columns to match standard structure
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
print(names(AL15SM2))
AL15SM2_ <- AL15SM2 %>% 
  select(
    # Basic identifiers
    year, 
    county, 
    
    # Low Birth Weight (*no racial data in 2015)
    pct_lbw = percent_lbw,                                     # Overall rate only
    
    # Teen Birth Rate (*no racial data in 2015)
    tbr = teen_birth_rate,                                     # Overall rate only
    
    # Education (cohort-based in 2015)
    num_hs_complete = cohort_size,                             # Cohort expected to graduate
    pop_25plus = population,                                   # Population for some college calculation
    pct_hs_complete = graduation_rate,                         # High school graduation rate
    num_some_college = number_some_college,                    # Adults with some post-secondary education
    pop_25_44 = population,                                    # Population (same as pop_25plus in 2015)
    pct_some_college = percent_some_college,                   # % with some post-secondary education
    
    # Children in Poverty (*no racial data in 2015)
    pct_child_poverty = percent_children_in_poverty            # Overall rate only
  ) %>%
  # Add missing demographic columns (not available in 2015)
  mutate(
    pct_lbw_aian = NA,                                         # All LBW racial categories missing
    pct_lbw_asian = NA,
    pct_lbw_biracial = NA,
    pct_lbw_black = NA,
    pct_lbw_hispanic = NA,
    pct_lbw_nhopi = NA,
    pct_lbw_white = NA,
    tbr_aian = NA,                                             # All TBR racial categories missing
    tbr_asian = NA,
    tbr_biracial = NA,
    tbr_black = NA,
    tbr_hispanic = NA,
    tbr_nhopi = NA,
    tbr_white = NA,
    pct_child_poverty_aian = NA,                               # All child poverty racial categories missing
    pct_child_poverty_asian = NA,
    pct_child_poverty_black = NA,
    pct_child_poverty_hispanic = NA,
    pct_child_poverty_white = NA
  ) %>%
  # Reorder columns to match standard structure
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
print(names(AL14SM2))
AL14SM2_ <- AL14SM2 %>% 
  select(
    # Basic identifiers
    year, 
    county, 
    
    # Low Birth Weight (*no racial data in 2014)
    pct_lbw = percent_lbw,                                     # Overall rate only
    
    # Teen Birth Rate (*no racial data in 2014)
    tbr = teen_birth_rate,                                     # Overall rate only
    
    # Education (cohort-based in 2014)
    num_hs_complete = cohort_size,                             # Cohort expected to graduate
    pop_25plus = population,                                   # Population for some college calculation
    pct_hs_complete = graduation_rate,                         # High school graduation rate
    num_some_college = number_some_college,                    # Adults with some post-secondary education
    pop_25_44 = population,                                    # Population (same as pop_25plus in 2014)
    pct_some_college = percent_some_college,                   # % with some post-secondary education
    
    # Children in Poverty (*no racial data in 2014)
    pct_child_poverty = percent_children_in_poverty            # Overall rate only
  ) %>%
  # Add missing demographic columns (not available in 2014)
  mutate(
    pct_lbw_aian = NA,                                         # All LBW racial categories missing
    pct_lbw_asian = NA,
    pct_lbw_biracial = NA,
    pct_lbw_black = NA,
    pct_lbw_hispanic = NA,
    pct_lbw_nhopi = NA,
    pct_lbw_white = NA,
    tbr_aian = NA,                                             # All TBR racial categories missing
    tbr_asian = NA,
    tbr_biracial = NA,
    tbr_black = NA,
    tbr_hispanic = NA,
    tbr_nhopi = NA,
    tbr_white = NA,
    pct_child_poverty_aian = NA,                               # All child poverty racial categories missing
    pct_child_poverty_asian = NA,
    pct_child_poverty_black = NA,
    pct_child_poverty_hispanic = NA,
    pct_child_poverty_white = NA
  ) %>%
  # Reorder columns to match standard structure
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

##year column issue
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
  AL14SM2_,
  AL15SM2_,
  AL16SM2_,
  AL17SM2_,
  AL18SM2_,
  AL19SM2_,
  AL20SM2_,
  AL21SM2_,
  AL22SM2_,
  AL23SM2_,
  AL24SM2_
)

# Convert data types for proper analysis
AL14_24SM2 <- AL14_24SM2 %>%
  mutate(
    # Convert year to numeric (for trend analysis, plotting)
    year = as.numeric(year),
    
    # Keep county as character (for grouping, filtering)
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
    
    # Education count variables (keep as numeric for calculations)
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

cat("✅ Type conversions completed!\n\n")

# Verify the conversions
cat("=== UPDATED DATA TYPES ===\n")
updated_types <- sapply(AL14_24SM2, class)
print(data.frame(Column = names(updated_types), Type = updated_types, row.names = NULL))

cat("1. MISSING DATA SUMMARY:\n")
missing_summary <- AL14_24SM2 %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  mutate(Missing_Percent = round(Missing_Count / nrow(AL14_24SM2) * 100, 1)) %>%
  arrange(desc(Missing_Count))

print(head(missing_summary, 15))

cat("\n2. YEAR DISTRIBUTION:\n")
year_counts <- table(AL14_24SM2$year)
print(year_counts)

cat("\n3. COUNTY COUNT BY YEAR:\n")
county_by_year <- AL14_24SM2 %>%
  group_by(year) %>%
  summarise(Counties = n_distinct(county), .groups = 'drop')
print(county_by_year)

cat("\n4. RANGE CHECKS FOR KEY VARIABLES:\n")
# Check ranges for percentage variables (should be 0-100)
range_check <- AL14_24SM2 %>%
  summarise(
    LBW_min = min(pct_lbw, na.rm = TRUE),
    LBW_max = max(pct_lbw, na.rm = TRUE),
    TBR_min = min(tbr, na.rm = TRUE),
    TBR_max = max(tbr, na.rm = TRUE),
    HS_min = min(pct_hs_complete, na.rm = TRUE),
    HS_max = max(pct_hs_complete, na.rm = TRUE),
    Poverty_min = min(pct_child_poverty, na.rm = TRUE),
    Poverty_max = max(pct_child_poverty, na.rm = TRUE)
  )
print(range_check)

AL14_24SM2 <- AL14_24SM2 %>%
  mutate(
    # Create factor versions for statistical analysis
    year_factor = factor(year),
    county_factor = factor(county),
    
    # Create racial data availability indicator
    racial_data_available = case_when(
      year %in% 2014:2017 ~ "None/Limited",
      year %in% 2018:2019 ~ "Basic (3 groups)",
      year %in% 2020:2023 ~ "Full (5 groups)",
      year == 2024 ~ "Extended (7 groups)",
      TRUE ~ "Unknown"
    ))

write.csv(AL14_24SM2, "AL14_24SM2.csv")

#============#
#Analysis
#============#

# Overall Trends (2014-2024): 11-year analysis using only overall rates
# Equity Analysis (2020-2024): 5-year analysis with full racial breakdowns
# Limited Equity (2018-2024): 7-year analysis with Black, White, Hispanic only
# 
# Timeline of Data Availability:
#   
# 2014-2017: No racial data (overall only)
# 2018-2019: Black, White, Hispanic available
# 2020-2024: Full racial breakdown (5+ groups)
# Education: Never has racial breakdowns

###State Trends
state_trends <- AL14_24SM2 %>% 
  filter(county == "State Total")

county_trends <- AL14_24SM2 %>% 
  filter(county != "State Total")

# Check the state trends data
glimpse(state_trends)

# Look at a few key variables
state_trends %>% 
  select(year, pct_lbw, tbr, pct_hs_complete, pct_child_poverty) %>%
  head()

# See all 11 years
# See all 11 years - simpler method
state_trends %>% 
  select(year, pct_lbw, tbr, pct_hs_complete, pct_some_college, pct_child_poverty)

# Check data types
sapply(state_trends[c("year", "pct_lbw", "tbr", "pct_hs_complete", "pct_child_poverty")], class)

##ensuring percetnage values carry over
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

# Quick verification of your Tableau-ready data
AL14_24SM2_tableau %>%
  filter(county == "State Total") %>%
  select(year, county, pct_lbw, tbr, pct_hs_complete, pct_child_poverty) %>%
  head(3)

write_csv(AL14_24SM2_tableau, "Al14_24SM2_Tableau.csv")

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

# Export the new indexed data
write_csv(AL14_24SM2_indexed, "Al14_24SM2_Indexed.csv")

# Filter to complete data and select variables for correlation
correlation_data <- AL14_24SM2 %>%
  # Remove State Total since we want county-level correlations
  filter(county != "State Total") %>%
  # Select your key variables
  select(year, county, pct_lbw, tbr, pct_hs_complete, pct_some_college, pct_child_poverty) %>%
  # Remove any rows with missing data
  drop_na()

##correlations
# Check what years have the most complete data
correlation_data %>%
  group_by(year) %>%
  summarise(
    counties = n(),
    complete_cases = sum(complete.cases(.))
  )

# Calculate correlations using all years and counties
# Calculate correlations using all years and counties
correlation_matrix <- correlation_data %>%
  select(pct_lbw, tbr, pct_hs_complete, pct_some_college, pct_child_poverty) %>%
  cor(use = "complete.obs")

# View the correlation matrix
print(round(correlation_matrix, 3))

# Calculate correlations for each year separately
yearly_correlations <- correlation_data %>%
  group_by(year) %>%
  summarise(
    education_poverty = cor(pct_hs_complete, pct_child_poverty, use = "complete.obs"),
    education_lbw = cor(pct_hs_complete, pct_lbw, use = "complete.obs"),
    lbw_poverty = cor(pct_lbw, pct_child_poverty, use = "complete.obs"),
    .groups = 'drop'
  )

#view the yearly correlations
print(round(yearly_correlations, 3))


# Create correlation trends data for Tableau export
correlation_trends_tableau <- correlation_data %>%
  group_by(year) %>%
  summarise(
    # Education correlations
    `Education-Poverty` = cor(pct_hs_complete, pct_child_poverty, use = "complete.obs"),
    `Education-LBW` = cor(pct_hs_complete, pct_lbw, use = "complete.obs"),
    `College-Poverty` = cor(pct_some_college, pct_child_poverty, use = "complete.obs"),
    `College-LBW` = cor(pct_some_college, pct_lbw, use = "complete.obs"),
    `College-TBR` = cor(pct_some_college, tbr, use = "complete.obs"),
    
    # Health outcome correlations
    `LBW-Poverty` = cor(pct_lbw, pct_child_poverty, use = "complete.obs"),
    `LBW-TBR` = cor(pct_lbw, tbr, use = "complete.obs"),
    `TBR-Poverty` = cor(tbr, pct_child_poverty, use = "complete.obs"),
    
    .groups = 'drop'
  )

# View the data
print(correlation_trends_tableau)

# Export for Tableau
write_csv(correlation_trends_tableau, "AL_Corr_Trends_14_24SM2.csv")

# Also create the overall correlation matrix for export
correlation_matrix_tableau <- correlation_data %>%
  select(pct_lbw, tbr, pct_hs_complete, pct_some_college, pct_child_poverty) %>%
  cor(use = "complete.obs") %>%
  round(3) %>%
  as.data.frame() %>%
  rownames_to_column("Variable1") %>%
  pivot_longer(-Variable1, names_to = "Variable2", values_to = "Correlation")

# Export correlation matrix
write_csv(correlation_matrix_tableau, "AL_Corr_Matrix_SM2.csv")


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

#### 3 years (2021-2023) of complete racial data for 5 groups

# year-by-year data availability for state level
AL14_24SM2 %>%
  filter(county == "State Total") %>%
  select(year, contains("_aian"), contains("_black")) %>%
  group_by(year) %>%
  summarise(
    has_aian_lbw = !is.na(first(pct_lbw_aian)),
    has_black_lbw = !is.na(first(pct_lbw_black)),
    .groups = 'drop'
  )

# raw demographic data by year
AL14_24SM2 %>%
  filter(county == "State Total") %>%
  select(year, pct_lbw, pct_lbw_black, pct_lbw_white, pct_child_poverty, pct_child_poverty_black) %>%
  arrange(year)

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
# County-level racial disparities for 2021-2023
county_demo <- AL14_24SM2 %>%
  filter(
    year %in% c(2021, 2022, 2023),
    county != "State Total"
  ) %>%
  select(year, county, pct_lbw_black, pct_lbw_white, 
         pct_child_poverty_black, pct_child_poverty_white)

# Check which counties have complete data
county_demo %>%
  group_by(county) %>%
  summarise(complete_years = sum(!is.na(pct_lbw_black) & !is.na(pct_child_poverty_black))) %>%
  arrange(desc(complete_years))

# organize county data 
county_disparities <- AL14_24SM2 %>%
  filter(
    year %in% c(2021, 2022, 2023),
    county != "State Total"
  ) %>%
  select(year, county, pct_lbw_black, pct_lbw_white, 
         pct_child_poverty_black, pct_child_poverty_white) %>%
  # Calculate racial gaps
  mutate(
    lbw_gap = pct_lbw_black - pct_lbw_white,
    poverty_gap = pct_child_poverty_black - pct_child_poverty_white
  ) %>%
  # Remove counties with insufficient data
  filter(!is.na(lbw_gap) | !is.na(poverty_gap))

# Look at the largest disparities
county_disparities %>%
  group_by(county) %>%
  summarise(
    avg_lbw_gap = mean(lbw_gap, na.rm = TRUE),
    avg_poverty_gap = mean(poverty_gap, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_poverty_gap)) %>%
  head(10)

# actual values for the top disparity counties
county_disparities %>%
  filter(county %in% c("Lamar", "Wilcox", "Tallapoosa")) %>%
  select(year, county, pct_child_poverty_black, pct_child_poverty_white, poverty_gap) %>%
  arrange(county, year)

# Compare to state-level 
state_gaps <- AL14_24SM2 %>%
  filter(year %in% c(2021, 2022, 2023), county == "State Total") %>%
  summarise(
    state_lbw_gap = mean(pct_lbw_black - pct_lbw_white, na.rm = TRUE),
    state_poverty_gap = mean(pct_child_poverty_black - pct_child_poverty_white, na.rm = TRUE)
  )

print(state_gaps)

# Which counties have the SMALLEST gaps (most equitable)?
county_disparities %>%
  group_by(county) %>%
  summarise(
    avg_lbw_gap = mean(lbw_gap, na.rm = TRUE),
    avg_poverty_gap = mean(poverty_gap, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(avg_poverty_gap) %>%
  head(10)

# Create year-filterable disparity data for Tableau
tableau_disparities_flexible <- county_disparities %>%
  # Individual year data
  select(year, county, lbw_gap, poverty_gap) %>%
  mutate(
    year_filter = as.character(year),
    lbw_gap_category = case_when(
      lbw_gap >= 10 ~ "High Disparity (10+ points)",
      lbw_gap >= 5 ~ "Moderate Disparity (5-10 points)", 
      lbw_gap >= 0 ~ "Low Disparity (0-5 points)",
      lbw_gap < 0 ~ "Reverse Disparity",
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
  # Add "All Years" aggregate data
  bind_rows(
    county_disparities %>%
      group_by(county) %>%
      summarise(
        year = 9999,  # Use 9999 as code for "All Years"
        year_filter = "All Years",
        lbw_gap = mean(lbw_gap, na.rm = TRUE),
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

# Export for Tableau
write_csv(tableau_disparities_flexible, "Alabama_County_Disparities_Filterable.csv")

# Check the output
tableau_disparities_flexible %>%
  filter(county == "Jefferson") %>%
  select(year_filter, county, lbw_gap, poverty_gap)


###2018 - 2024 Demo analysis

# Prepare 2018-2024 data with Black, White, Hispanic
demo_2018_2024 <- AL14_24SM2 %>%
  filter(
    year >= 2018,
    county != "State Total"  # county-level for more robust analysis
  ) %>%
  select(
    year, county,
    pct_lbw, pct_lbw_black, pct_lbw_white, pct_lbw_hispanic,
    pct_child_poverty, pct_child_poverty_black, pct_child_poverty_white, pct_child_poverty_hispanic
  ) %>%
  # Calculate gaps
  mutate(
    lbw_black_white_gap = pct_lbw_black - pct_lbw_white,
    lbw_hispanic_white_gap = pct_lbw_hispanic - pct_lbw_white,
    poverty_black_white_gap = pct_child_poverty_black - pct_child_poverty_white,
    poverty_hispanic_white_gap = pct_child_poverty_hispanic - pct_child_poverty_white
  )

# Check data availability by year
demo_2018_2024 %>%
  group_by(year) %>%
  summarise(
    counties_with_lbw_data = sum(!is.na(lbw_black_white_gap)),
    counties_with_poverty_data = sum(!is.na(poverty_black_white_gap)),
    .groups = 'drop'
  )

# state-level trends from county data
state_trends_2018_2024 <- demo_2018_2024 %>%
  group_by(year) %>%
  summarise(
    # Average gaps across counties
    avg_lbw_black_white_gap = mean(lbw_black_white_gap, na.rm = TRUE),
    avg_lbw_hispanic_white_gap = mean(lbw_hispanic_white_gap, na.rm = TRUE),
    avg_poverty_black_white_gap = mean(poverty_black_white_gap, na.rm = TRUE),
    avg_poverty_hispanic_white_gap = mean(poverty_hispanic_white_gap, na.rm = TRUE),
    
    # Number of counties with data
    counties_lbw = sum(!is.na(lbw_black_white_gap)),
    counties_poverty = sum(!is.na(poverty_black_white_gap)),
    .groups = 'drop'
  )

print(state_trends_2018_2024)

# Export Table 4: Demographic Trends (2018-2024)
write_csv(state_trends_2018_2024, "Alabama_Demo_Trends_18_24.csv")

# create a more comprehensive demographic trends table for Tableau
demographic_trends_detailed <- demo_2018_2024 %>%
  # Calculate state-level averages by year
  group_by(year) %>%
  summarise(
    # Overall rates
    avg_lbw = mean(pct_lbw, na.rm = TRUE),
    avg_child_poverty = mean(pct_child_poverty, na.rm = TRUE),
    
    # Black-White gaps
    lbw_black_white_gap = mean(lbw_black_white_gap, na.rm = TRUE),
    poverty_black_white_gap = mean(poverty_black_white_gap, na.rm = TRUE),
    
    # Hispanic-White gaps  
    lbw_hispanic_white_gap = mean(lbw_hispanic_white_gap, na.rm = TRUE),
    poverty_hispanic_white_gap = mean(poverty_hispanic_white_gap, na.rm = TRUE),
    
    # Data availability counts
    counties_with_data = sum(!is.na(lbw_black_white_gap)),
    
    .groups = 'drop'
  ) %>%
  # Convert to percentage decimals for Tableau
  mutate(
    lbw_black_white_gap = lbw_black_white_gap / 100,
    poverty_black_white_gap = poverty_black_white_gap / 100,
    lbw_hispanic_white_gap = lbw_hispanic_white_gap / 100,
    poverty_hispanic_white_gap = poverty_hispanic_white_gap / 100,
    avg_lbw = avg_lbw / 100,
    avg_child_poverty = avg_child_poverty / 100
  )

# Export the detailed version
write_csv(demographic_trends_detailed, "AL_Demo_Trends_18_24_Tableau.csv")

#trends for major counties
major_counties <- c("Jefferson", "Mobile", "Madison", "Montgomery", "Tuscaloosa")

demo_2018_2024 %>%
  filter(county %in% major_counties) %>%
  select(year, county, poverty_black_white_gap, lbw_black_white_gap) %>%
  arrange(county, year)

###ANOVA testing
# Reshape data for ANOVA (need long format)
anova_data <- demo_2018_2024 %>%
  filter(!is.na(pct_lbw_black), !is.na(pct_lbw_white), !is.na(pct_lbw_hispanic)) %>%
  select(year, county, pct_lbw_black, pct_lbw_white, pct_lbw_hispanic,
         pct_child_poverty_black, pct_child_poverty_white, pct_child_poverty_hispanic) %>%
  # Convert to long format
  pivot_longer(
    cols = c(pct_lbw_black, pct_lbw_white, pct_lbw_hispanic),
    names_to = "race_lbw", 
    values_to = "lbw_rate",
    names_prefix = "pct_lbw_"
  ) %>%
  pivot_longer(
    cols = c(pct_child_poverty_black, pct_child_poverty_white, pct_child_poverty_hispanic),
    names_to = "race_poverty",
    values_to = "poverty_rate", 
    names_prefix = "pct_child_poverty_"
  ) %>%
  filter(
    str_remove(race_lbw, "pct_lbw_") == str_remove(race_poverty, "pct_child_poverty_")
  ) %>%
  mutate(
    race = str_remove(race_lbw, "pct_lbw_"),
    race = str_replace(race, "_", " ") %>% str_to_title()
  )

# Test 1: Race differences in LBW
lbw_anova <- aov(lbw_rate ~ race, data = anova_data)
summary(lbw_anova)

# Test 2: Race differences in Child Poverty  
poverty_anova <- aov(poverty_rate ~ race, data = anova_data)
summary(poverty_anova)

##Tukeytest

# Pairwise comparisons for LBW
TukeyHSD(lbw_anova)

# Pairwise comparisons for Child Poverty
TukeyHSD(poverty_anova)

###ANOVA time trend analysis
# Test if disparities are significantly worsening over time
trend_data <- state_trends_2018_2024 %>%
  select(year, avg_lbw_black_white_gap, avg_poverty_black_white_gap)

# Linear regression to test trend significance
lbw_trend <- lm(avg_lbw_black_white_gap ~ year, data = trend_data)
poverty_trend <- lm(avg_poverty_black_white_gap ~ year, data = trend_data)

summary(lbw_trend)
summary(poverty_trend)

##expanding on racial trends
# Create streamlined comprehensive race trends table (2017-2024)
comprehensive_race_trends <- AL14_24SM2 %>%
  filter(year >= 2017) %>%
  select(year, county, 
         # All racial breakdowns for LBW
         pct_lbw_aian, pct_lbw_asian, pct_lbw_black, pct_lbw_white, 
         pct_lbw_hispanic, pct_lbw_biracial, pct_lbw_nhopi,
         # All racial breakdowns for Child Poverty
         pct_child_poverty_aian, pct_child_poverty_asian, pct_child_poverty_black, 
         pct_child_poverty_white, pct_child_poverty_hispanic,
         # All racial breakdowns for TBR
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
    
    # CONVERT TO DECIMALS - This is what Tableau expects for percentage formatting
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

# Export for Tableau
write_csv(comprehensive_race_trends, "AL14_24SM2_Comprehensive_Race_Trends.csv", na = "")

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



