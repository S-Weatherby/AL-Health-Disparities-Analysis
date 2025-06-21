# Alabama Health Disparities Analysis
# Script: 03_statistical_analysis.R
# Author: Shelita Smith
# Date: June 20, 2025
# Purpose: Perform statistical tests and correlation analysis

library(tidyverse)
library(corrplot)


AL14_24SM2 <- read_csv("data/processed/AL14_24SM2_unified.csv")

# Filter to complete data and select variables for correlation
correlation_data <- AL14_24SM2 %>%
  filter(county != "State Total") %>%
  select(year, county, pct_lbw, tbr, pct_hs_complete, pct_some_college, pct_child_poverty) %>%
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

# Create the overall correlation matrix for export - Fixed version
correlation_matrix_tableau <- correlation_data %>%
  select(pct_lbw, tbr, pct_hs_complete, pct_some_college, pct_child_poverty) %>%
  cor(use = "complete.obs") %>%
  round(3)

# Convert to long format using base R approach
correlation_matrix_tableau <- as.data.frame(correlation_matrix_tableau)
correlation_matrix_tableau$Variable1 <- rownames(correlation_matrix_tableau)

# Reshape to long format
correlation_matrix_tableau <- correlation_matrix_tableau %>%
  pivot_longer(-Variable1, names_to = "Variable2", values_to = "Correlation")

# =============================================================================
# ENHANCED CORRELATION ANALYSIS WITH TBR
# =============================================================================

# Create a more detailed correlation matrix with significance tests
create_enhanced_correlation_matrix <- function(data) {
  # Select numeric variables INCLUDING TBR
  numeric_vars <- data %>%
    select(pct_lbw, tbr, pct_hs_complete, pct_some_college, pct_child_poverty) %>%
    select(where(is.numeric))
  
  # Calculate correlation matrix
  cor_matrix <- cor(numeric_vars, use = "complete.obs")
  
  # Calculate p-values for correlations
  cor_test_results <- combn(names(numeric_vars), 2, function(x) {
    test_result <- cor.test(numeric_vars[[x[1]]], numeric_vars[[x[2]]])
    data.frame(
      Variable1 = x[1],
      Variable2 = x[2], 
      Correlation = round(test_result$estimate, 3),
      P_Value = round(test_result$p.value, 4),
      Significant = test_result$p.value < 0.05
    )
  }, simplify = FALSE)
  
  # Combine results
  cor_results <- do.call(rbind, cor_test_results)
  
  return(list(
    correlation_matrix = cor_matrix,
    detailed_results = cor_results
  ))
}

# Apply enhanced correlation analysis
enhanced_correlations <- create_enhanced_correlation_matrix(correlation_data)

# ENHANCED: Reshape data for ANOVA INCLUDING TBR
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

anova_data <- demo_2018_2024 %>%
  filter(!is.na(pct_lbw_black), !is.na(pct_lbw_white), !is.na(pct_lbw_hispanic),
         !is.na(tbr_black), !is.na(tbr_white), !is.na(tbr_hispanic)) %>%  # ENHANCED: ADD TBR filters
  select(year, county, 
         pct_lbw_black, pct_lbw_white, pct_lbw_hispanic,
         tbr_black, tbr_white, tbr_hispanic,  # ENHANCED: ADD TBR variables
         pct_child_poverty_black, pct_child_poverty_white, pct_child_poverty_hispanic) %>%
  # Convert to long format
  pivot_longer(
    cols = c(pct_lbw_black, pct_lbw_white, pct_lbw_hispanic),
    names_to = "race_lbw", 
    values_to = "lbw_rate",
    names_prefix = "pct_lbw_"
  ) %>%
  # ENHANCED: ADD TBR pivot
  pivot_longer(
    cols = c(tbr_black, tbr_white, tbr_hispanic),
    names_to = "race_tbr",
    values_to = "tbr_rate",
    names_prefix = "tbr_"
  ) %>%
  pivot_longer(
    cols = c(pct_child_poverty_black, pct_child_poverty_white, pct_child_poverty_hispanic),
    names_to = "race_poverty",
    values_to = "poverty_rate", 
    names_prefix = "pct_child_poverty_"
  ) %>%
  filter(
    str_remove(race_lbw, "pct_lbw_") == str_remove(race_poverty, "pct_child_poverty_") &
      str_remove(race_lbw, "pct_lbw_") == str_remove(race_tbr, "tbr_")  # ENHANCED: ADD TBR matching
  ) %>%
  mutate(
    race = str_remove(race_lbw, "pct_lbw_"),
    race = str_replace(race, "_", " ") %>% str_to_title()
  )

# ENHANCED: ANOVA Tests
print("=== ENHANCED ANOVA RESULTS WITH TBR ===")

# Test 1: Race differences in LBW
lbw_anova <- aov(lbw_rate ~ race, data = anova_data)
print("LBW by Race:")
summary(lbw_anova)

# ENHANCED: Test 2: Race differences in TBR
tbr_anova <- aov(tbr_rate ~ race, data = anova_data)
print("Teen Birth Rate by Race:")
summary(tbr_anova)

# Test 3: Race differences in Child Poverty  
poverty_anova <- aov(poverty_rate ~ race, data = anova_data)
print("Child Poverty by Race:")
summary(poverty_anova)

##ENHANCED Tukey tests WITH TBR

print("=== ENHANCED TUKEY HSD RESULTS ===")

# Pairwise comparisons for LBW
print("LBW Pairwise Comparisons:")
TukeyHSD(lbw_anova)

# ENHANCED: Pairwise comparisons for TBR
print("Teen Birth Rate Pairwise Comparisons:")
TukeyHSD(tbr_anova)

# Pairwise comparisons for Child Poverty
print("Child Poverty Pairwise Comparisons:")
TukeyHSD(poverty_anova)

###ENHANCED ANOVA time trend analysis WITH TBR
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

# ENHANCED: Test if disparities are significantly worsening over time
trend_data <- state_trends_2018_2024 %>%
  select(year, avg_lbw_black_white_gap, avg_tbr_black_white_gap, avg_poverty_black_white_gap)  # ENHANCED: ADD TBR

# ENHANCED: Linear regression tests
lbw_trend <- lm(avg_lbw_black_white_gap ~ year, data = trend_data)
tbr_trend <- lm(avg_tbr_black_white_gap ~ year, data = trend_data)  # ENHANCED: ADD TBR trend
poverty_trend <- lm(avg_poverty_black_white_gap ~ year, data = trend_data)

print("=== ENHANCED TREND ANALYSIS RESULTS ===")
print("LBW Gap Trend:")
summary(lbw_trend)

print("Teen Birth Rate Gap Trend:")  # ENHANCED: ADD TBR results
summary(tbr_trend)

print("Child Poverty Gap Trend:")
summary(poverty_trend)

# ENHANCED: Correlation between disparity trends
print("Correlations between disparity trends:")
cor(trend_data[2:4], use = "complete.obs")

# Save analysis results
write_csv(correlation_trends_tableau, "outputs/tables/correlation_trends.csv")
write_csv(correlation_matrix_tableau, "outputs/tables/correlation_matrix.csv")
write_csv(enhanced_correlations$detailed_results, "outputs/tables/enhanced_correlations.csv")
