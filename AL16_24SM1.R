AL2016CountyHealthSM <- read.csv("C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\2016 County Health Rankings Alabama Data - v3.xlsx - Ranked Measure Data.csv")
AL2018CountyHealthSM <- read.csv("C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\2018 County Health Rankings Alabama Data - v3.xlsx - Ranked Measure Data.csv")
AL2020CountyHealthSM <- read.csv("C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\2020 County Health Rankings Alabama Data - v1_1.xlsx - Ranked Measure Data.csv")
AL2022CountyHealthSM <- read.csv("C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\2022 County Health Rankings Alabama Data - v2.xlsx - Ranked Measure Data.csv")
AL2024CountyHealthSM <- read.csv("C:\\Users\\sheli\\OneDrive\\Desktop\\Data Analysis\\Portfolio Projects\\2024 County Health Rankings Alabama Data - v2.xlsx - Select Measure Data.csv")

##New column names
new_colnames <- as.character(AL2016CountyHealthSM[1, ])
colnames(AL2016CountyHealthSM) <- new_colnames
AL2016CountyHealthSM <- AL2016CountyHealthSM[-1, ]  

new_colnames <- as.character(AL2018CountyHealthSM[1, ])
colnames(AL2018CountyHealthSM) <- new_colnames
AL2018CountyHealthSM <- AL2018CountyHealthSM[-1, ]  

new_colnames <- as.character(AL2020CountyHealthSM[1, ])
colnames(AL2020CountyHealthSM) <- new_colnames
AL2020CountyHealthSM <- AL2020CountyHealthSM[-1, ]  

new_colnames <- as.character(AL2022CountyHealthSM[1, ])
colnames(AL2022CountyHealthSM) <- new_colnames
AL2022CountyHealthSM <- AL2022CountyHealthSM[-1, ] 

new_colnames <- as.character(AL2024CountyHealthSM[1, ])
colnames(AL2024CountyHealthSM) <- new_colnames
AL2024CountyHealthSM <- AL2024CountyHealthSM[-1, ] 

AL2016CountyHealthSM <- clean_names(AL2016CountyHealthSM)
AL2018CountyHealthSM <- clean_names(AL2018CountyHealthSM)
AL2020CountyHealthSM <- clean_names(AL2020CountyHealthSM)
AL2022CountyHealthSM <- clean_names(AL2022CountyHealthSM)
AL2024CountyHealthSM <- clean_names(AL2024CountyHealthSM)

summary(AL2016CountyHealthSM)
summary(AL2018CountyHealthSM)
summary(AL2020CountyHealthSM)
summary(AL2022CountyHealthSM)
summary(AL2024CountyHealthSM)


##Making SM1 tables

AL16SM1 <- AL2016CountyHealthSM %>% 
  select(year, county, premature_deaths = number_deaths, years_of_potential_life_lost_rate, number_uninsured, percent_uninsured, number_pcp = number_primary_care_physicians, pcp_ratio, pcp_rate, preventable_hosp_rate)

AL18SM1 <- AL2018CountyHealthSM %>% 
  select(year, county, premature_deaths = deaths, years_of_potential_life_lost_rate, number_uninsured, percent_uninsured, number_pcp = number_primary_care_physicians, pcp_ratio, pcp_rate, preventable_hosp_rate)

AL20SM1 <- AL2020CountyHealthSM %>% 
  select(year, county, premature_deaths = deaths, years_of_potential_life_lost_rate, number_uninsured, percent_uninsured, number_pcp = number_primary_care_physicians, pcp_ratio = primary_care_physicians_ratio, pcp_rate = primary_care_physicians_rate, preventable_hosp_rate = preventable_hospitalization_rate)

AL22SM1 <- AL2022CountyHealthSM %>% 
  select(year, county, premature_deaths = deaths, years_of_potential_life_lost_rate, number_uninsured, percent_uninsured, number_pcp = number_primary_care_physicians, pcp_ratio = primary_care_physicians_ratio, pcp_rate = primary_care_physicians_rate, preventable_hosp_rate = preventable_hospitalization_rate)

AL24SM1 <- AL2024CountyHealthSM %>% 
  select(year, county, premature_deaths = deaths, years_of_potential_life_lost_rate, number_uninsured, percent_uninsured, number_pcp = number_primary_care_physicians, pcp_ratio = primary_care_physicians_ratio, pcp_rate = primary_care_physicians_rate, preventable_hosp_rate = preventable_hospitalization_rate)

summary(AL16SM1)
summary(AL18SM1)
summary(AL20SM1)
summary(AL22SM1)
summary(AL24SM1)

## Cleaning

AL16SM1 <- AL16SM1 %>% 
  mutate(across(-c(year, county, pcp_ratio), ~ {
    str_replace_all(.x, ",", "") %>% 
      as.numeric()
  }))

AL18SM1 <- AL18SM1 %>% 
  mutate(across(-c(year, county, pcp_ratio), ~ {
    str_replace_all(.x, ",", "") %>% 
      as.numeric()
  }))

AL20SM1 <- AL20SM1 %>% 
  mutate(across(-c(year, county, pcp_ratio), ~ {
    str_replace_all(.x, ",", "") %>% 
      as.numeric()
  }))

AL22SM1 <- AL22SM1 %>% 
  mutate(across(-c(year, county, pcp_ratio), ~ {
    str_replace_all(.x, ",", "") %>% 
      as.numeric()
  }))

AL24SM1 <- AL24SM1 %>% 
  mutate(across(-c(year, county, pcp_ratio), ~ {
    str_replace_all(.x, ",", "") %>% 
      as.numeric()
  }))

# # Function to clean and convert columns
# clean_and_convert <- function(data) {
#   data %>%
#     mutate(across(-c(year, county, pcp_ratio), ~ {
#       # Remove commas, extra spaces, and common non-numeric characters
#       cleaned <- str_replace_all(.x, "[,$%]", "")  # Remove commas, dollar signs, percent signs
#       cleaned <- str_trim(cleaned)  # Remove leading/trailing spaces
#       cleaned <- ifelse(cleaned == "" | cleaned == "N/A" | cleaned == "-", NA, cleaned)
#     }))
# }

##Changing column types
AL16SM1 <- AL16SM1 %>% 
  mutate(across(-c(year, county, pcp_ratio),as.numeric))
AL18SM1 <- AL18SM1 %>% 
  mutate(across(-c(year, county, pcp_ratio),as.numeric))
AL20SM1 <- AL20SM1 %>% 
  mutate(across(-c(year, county, pcp_ratio),as.numeric))
AL22SM1 <- AL22SM1 %>% 
  mutate(across(-c(year, county, pcp_ratio),as.numeric))
AL24SM1 <- AL24SM1 %>% 
  mutate(across(-c(year, county, pcp_ratio),as.numeric))


#making one large table to see how it goes
combined_table <- bind_rows(AL16SM1, AL18SM1, AL20SM1, AL22SM1, AL24SM1)

AL16_24SM1 <- combined_table %>%
  select(year, county, premature_deaths, years_of_potential_life_lost_rate, number_uninsured, percent_uninsured, number_pcp, pcp_ratio, pcp_rate, preventable_hosp_rate) %>%
  arrange(county, year)

##removing state total rows
AL16_24SM1 <- AL16_24SM1 %>%
  slice(-(1:7))

write.csv(AL16_24SM1, file = "AL16_24SM1.csv")

# =====================================
# KEEP YOUR EXISTING CODE ABOVE THIS POINT
# (Data loading, cleaning, combining, etc.)
# =====================================

# Load required libraries for analysis section
library(broom)
library(corrplot)

# =====================================
# 1. EXPLORATORY DATA ANALYSIS
# =====================================

## Basic scatter plots for 2024 data
# Insurance vs health outcomes
p1 <- ggplot(AL24SM1, aes(x = number_uninsured, y = premature_deaths)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Insurance Coverage vs Premature Deaths (2024)",
       x = "Number Uninsured", y = "Premature Deaths")

p2 <- ggplot(AL24SM1, aes(x = percent_uninsured, y = preventable_hosp_rate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Insurance Coverage vs Preventable Hospitalizations (2024)",
       x = "Percent Uninsured", y = "Preventable Hospitalization Rate")

# Display plots
print(p1)
print(p2)

## Physician shortage analysis
physician_shortage <- AL24SM1 %>% 
  filter(pcp_ratio > 1500) %>%  # HRSA shortage threshold
  select(county, pcp_ratio, percent_uninsured) %>%
  arrange(desc(pcp_ratio))

cat("Counties with Physician Shortages (>1500:1 ratio):\n")
print(physician_shortage)

# =====================================
# 2. TEMPORAL TREND ANALYSIS
# =====================================

## Health outcomes trends over time
trends_plot <- AL16_24SM1 %>%
  group_by(year) %>%
  summarise(
    avg_premature_deaths = mean(premature_deaths, na.rm = TRUE),
    avg_ypll_rate = mean(years_of_potential_life_lost_rate, na.rm = TRUE),
    avg_preventable_hosp = mean(preventable_hosp_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-year, names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = year, y = value, color = metric)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~metric, scales = "free_y") +
  labs(title = "Health Outcomes Trends (2016-2024)",
       x = "Year", y = "Value") +
  theme_minimal()

print(trends_plot)

## Healthcare access trends
access_trends_plot <- AL16_24SM1 %>%
  group_by(year) %>%
  summarise(
    avg_uninsured_pct = mean(percent_uninsured, na.rm = TRUE),
    avg_pcp_rate = mean(pcp_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = avg_uninsured_pct), color = "red", size = 1.2) +
  geom_line(aes(y = avg_pcp_rate/10), color = "blue", size = 1.2) +
  scale_y_continuous(
    name = "% Uninsured",
    sec.axis = sec_axis(~.*10, name = "PCP Rate per 100k")
  ) +
  labs(title = "Healthcare Access Trends Over Time") +
  theme_minimal()

print(access_trends_plot)

# =====================================
# 3. COUNTY PERFORMANCE ANALYSIS
# =====================================

## Identify high-risk counties
high_risk_counties <- AL16_24SM1 %>%
  group_by(county) %>%
  summarise(
    avg_ypll = mean(years_of_potential_life_lost_rate, na.rm = TRUE),
    avg_uninsured = mean(percent_uninsured, na.rm = TRUE),  # Fixed: use percent not number
    avg_pcp_rate = mean(pcp_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    risk_score = scale(avg_ypll)[,1] + scale(avg_uninsured)[,1]
  ) %>%
  arrange(desc(risk_score))

## Create top/bottom performers dataset
top_bottom_counties <- bind_rows(
  head(high_risk_counties, 5) %>% mutate(category = "Highest Risk"),
  tail(high_risk_counties, 5) %>% mutate(category = "Lowest Risk")
)

cat("Top 5 Highest Risk Counties:\n")
print(head(high_risk_counties, 5))
cat("\nTop 5 Lowest Risk Counties:\n")
print(tail(high_risk_counties, 5))

# =====================================
# 4. CORRELATION ANALYSIS
# =====================================

## Overall correlation matrix
correlation_analysis <- AL16_24SM1 %>%
  select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs")

# Display correlation matrix
print("Correlation Matrix:")
print(round(correlation_analysis, 3))

## Correlations by year
year_correlations <- AL16_24SM1 %>%
  group_by(year) %>%
  nest() %>%
  mutate(
    correlations = map(data, ~ {
      .x %>% 
        select(where(is.numeric)) %>% 
        cor(use = "pairwise.complete.obs")
    })
  )

# Example: View correlation for 2020 (3rd element)
cat("\nCorrelation Matrix for 2020:\n")
print(round(year_correlations$correlations[[3]], 3))

# =====================================
# 5. RELATIONSHIP VISUALIZATIONS
# =====================================

## Insurance vs health outcomes across years
insurance_health_plot <- AL16_24SM1 %>%
  ggplot(aes(x = percent_uninsured, y = years_of_potential_life_lost_rate)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  facet_wrap(~year) +
  labs(title = "Insurance Coverage vs Years of Potential Life Lost",
       x = "Percent Uninsured", y = "YPLL Rate") +
  theme_minimal()

print(insurance_health_plot)

## Physician availability vs preventable hospitalizations
pcp_hospital_plot <- AL16_24SM1 %>%
  ggplot(aes(x = pcp_rate, y = preventable_hosp_rate)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  labs(title = "Primary Care Physician Rate vs Preventable Hospitalizations",
       x = "PCP Rate per 100k", y = "Preventable Hospitalization Rate") +
  theme_minimal()

print(pcp_hospital_plot)

# =====================================
# 6. COVID IMPACT ANALYSIS
# =====================================

## Pre vs post-COVID comparison (2018 vs 2022)
covid_impact <- AL16_24SM1 %>%
  filter(year %in% c(2018, 2022)) %>%
  group_by(county) %>%
  summarise(
    change_premature_deaths = premature_deaths[year == 2022] - premature_deaths[year == 2018],
    change_uninsured_pct = percent_uninsured[year == 2022] - percent_uninsured[year == 2018],
    change_pcp_rate = pcp_rate[year == 2022] - pcp_rate[year == 2018],
    .groups = "drop"
  ) %>%
  filter(!is.na(change_premature_deaths))

cat("COVID Impact Analysis (2018 vs 2022):\n")
cat("Average change in premature deaths:", round(mean(covid_impact$change_premature_deaths, na.rm = TRUE), 1), "\n")
cat("Average change in uninsured %:", round(mean(covid_impact$change_uninsured_pct, na.rm = TRUE), 1), "\n")
cat("Average change in PCP rate:", round(mean(covid_impact$change_pcp_rate, na.rm = TRUE), 1), "\n")

## County improvements over entire period
county_improvements <- AL16_24SM1 %>%
  group_by(county) %>%
  summarise(
    ypll_change = last(years_of_potential_life_lost_rate) - first(years_of_potential_life_lost_rate),
    uninsured_change = last(percent_uninsured) - first(percent_uninsured),
    .groups = "drop"
  ) %>%
  filter(!is.na(ypll_change))

# =====================================
# 7. STATISTICAL ANALYSIS
# =====================================

## ANOVA analysis function
analyze_temporal_trends <- function(data, outcome_var) {
  # ANOVA
  formula_str <- paste(outcome_var, "~ factor(year)")
  aov_result <- aov(as.formula(formula_str), data = data)
  aov_summary <- summary(aov_result)
  
  # Extract key statistics
  f_value <- aov_summary[[1]][["F value"]][1]
  p_value <- aov_summary[[1]][["Pr(>F)"]][1]
  
  # Effect size
  ss_total <- sum(aov_summary[[1]][["Sum Sq"]])
  ss_year <- aov_summary[[1]][["Sum Sq"]][1]
  eta_squared <- ss_year / ss_total
  
  # Interpretation
  significant <- p_value < 0.05
  
  cat("=== ANOVA Results for", outcome_var, "===\n")
  cat("F-statistic:", round(f_value, 2), "\n")
  cat("P-value:", round(p_value, 4), "\n")
  cat("Effect size (η²):", round(eta_squared, 3), "\n")
  cat("Significant?", ifelse(significant, "YES", "NO"), "\n")
  
  if(significant) {
    cat("\nInterpretation: There are significant differences across years.\n")
    cat("Year explains", round(eta_squared * 100, 1), "% of the variance.\n")
  } else {
    cat("\nInterpretation: No significant differences across years.\n")
    cat("Outcomes appear stable over time.\n")
  }
  cat("\n")
  
  return(list(aov = aov_result, summary = aov_summary, 
              significant = significant, eta_squared = eta_squared))
}

## Run ANOVA for multiple indicators
health_indicators <- c("years_of_potential_life_lost_rate", "percent_uninsured", 
                       "pcp_rate", "preventable_hosp_rate", "premature_deaths")

# Individual analyses
ypll_analysis <- analyze_temporal_trends(AL16_24SM1, "years_of_potential_life_lost_rate")
premature_deaths_analysis <- analyze_temporal_trends(AL16_24SM1, "premature_deaths")

# Comprehensive ANOVA for all indicators
aov_results_list <- map(health_indicators, ~ {
  formula_str <- paste(.x, "~ factor(year)")
  aov(as.formula(formula_str), data = AL16_24SM1)
})
names(aov_results_list) <- health_indicators

# ANOVA summary table
anova_summary <- tibble(
  metric = health_indicators,
  f_statistic = map_dbl(aov_results_list, ~ summary(.x)[[1]][["F value"]][1]),
  p_value = map_dbl(aov_results_list, ~ summary(.x)[[1]][["Pr(>F)"]][1]),
  significant = p_value < 0.05,
  interpretation = ifelse(significant, "Years differ significantly", "No significant year differences")
)

print("ANOVA Summary for All Health Indicators:")
print(anova_summary)

## Post-hoc analysis (if significant)
if(ypll_analysis$significant) {
  cat("Performing post-hoc analysis for YPLL...\n")
  tukey_results <- TukeyHSD(ypll_analysis$aov)
  print(tukey_results)
}

## Regression analysis
full_model <- lm(years_of_potential_life_lost_rate ~ percent_uninsured + pcp_rate + 
                   preventable_hosp_rate + factor(year), data = AL16_24SM1)

regression_summary <- tidy(full_model) %>%
  mutate(significant = p.value < 0.05)

cat("Regression Analysis Summary:\n")
print(regression_summary)

## Visualization of ANOVA results
anova_plot <- AL16_24SM1 %>%
  ggplot(aes(x = factor(year), y = years_of_potential_life_lost_rate)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3, width = 0.2) +
  labs(title = "YPLL Rate by Year",
       subtitle = paste("ANOVA F =", round(ypll_analysis$summary[[1]][["F value"]][1], 2),
                        ", p =", round(ypll_analysis$summary[[1]][["Pr(>F)"]][1], 3)),
       x = "Year", y = "YPLL Rate") +
  theme_minimal()

print(anova_plot)

# Line plot with error bars
trend_plot_detailed <- AL16_24SM1 %>%
  group_by(year) %>%
  summarise(
    mean_ypll = mean(years_of_potential_life_lost_rate, na.rm = TRUE),
    se_ypll = sd(years_of_potential_life_lost_rate, na.rm = TRUE)/sqrt(n()),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = year, y = mean_ypll)) +
  geom_line(group = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_ypll - se_ypll, ymax = mean_ypll + se_ypll), 
                width = 0.1) +
  labs(title = "Mean YPLL Rate Over Time with Standard Error",
       x = "Year", y = "Mean YPLL Rate") +
  theme_minimal()

print(trend_plot_detailed)

# =====================================
# 8. CREATE ENHANCED DATASET
# =====================================

## Add derived variables for Tableau
AL16_24SM1_enhanced <- AL16_24SM1 %>%
  group_by(county) %>%
  arrange(year) %>%
  mutate(
    # Time trends from baseline
    ypll_change_from_baseline = years_of_potential_life_lost_rate - first(years_of_potential_life_lost_rate, na_rm = TRUE),
    uninsured_change_from_baseline = percent_uninsured - first(percent_uninsured, na_rm = TRUE),
    
    # Year-over-year changes
    ypll_yoy_change = years_of_potential_life_lost_rate - lag(years_of_potential_life_lost_rate),
    uninsured_yoy_change = percent_uninsured - lag(percent_uninsured),
    
    # Performance quartiles
    ypll_quartile = ntile(years_of_potential_life_lost_rate, 4),
    uninsured_quartile = ntile(percent_uninsured, 4),
    
    # Healthcare access indicators
    pcp_shortage = case_when(
      pcp_ratio > 1500 ~ "Shortage",
      pcp_ratio <= 1500 ~ "Adequate",
      TRUE ~ "Unknown"
    )
  ) %>%
  ungroup() %>%
  mutate(
    # Combined health risk score
    health_risk_score = scale(years_of_potential_life_lost_rate)[,1] + 
      scale(percent_uninsured)[,1] + 
      scale(preventable_hosp_rate)[,1],
    
    # Performance tiers
    performance_tier = case_when(
      health_risk_score <= quantile(health_risk_score, 0.25, na.rm = TRUE) ~ "Top Quartile",
      health_risk_score <= quantile(health_risk_score, 0.5, na.rm = TRUE) ~ "Second Quartile",
      health_risk_score <= quantile(health_risk_score, 0.75, na.rm = TRUE) ~ "Third Quartile",
      TRUE ~ "Bottom Quartile"
    ),
    
    # Access categories
    access_category = case_when(
      pcp_rate > 80 & percent_uninsured < 15 ~ "Good Access",
      pcp_rate < 50 | percent_uninsured > 20 ~ "Poor Access",
      TRUE ~ "Moderate Access"
    )
  )

# =====================================
# 9. CREATE SUMMARY TABLES
# =====================================

## County-level summaries
county_summary <- AL16_24SM1_enhanced %>%
  group_by(county) %>%
  summarise(
    # Averages
    avg_ypll_rate = mean(years_of_potential_life_lost_rate, na.rm = TRUE),
    avg_uninsured_pct = mean(percent_uninsured, na.rm = TRUE),
    avg_pcp_rate = mean(pcp_rate, na.rm = TRUE),
    avg_preventable_hosp_rate = mean(preventable_hosp_rate, na.rm = TRUE),
    
    # Trends (only if enough data points)
    ypll_trend = ifelse(n() >= 3, 
                        coef(lm(years_of_potential_life_lost_rate ~ year, na.action = na.exclude))[2], 
                        NA),
    uninsured_trend = ifelse(n() >= 3,
                             coef(lm(percent_uninsured ~ year, na.action = na.exclude))[2], 
                             NA),
    
    # Data availability
    years_available = n(),
    latest_year = max(year, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Rankings
    ypll_rank = rank(-avg_ypll_rate),
    uninsured_rank = rank(-avg_uninsured_pct),
    pcp_rank = rank(avg_pcp_rate),
    
    # Performance categories
    ypll_performance = case_when(
      ypll_rank <= quantile(ypll_rank, 0.25, na.rm = TRUE) ~ "Top Tier",
      ypll_rank <= quantile(ypll_rank, 0.5, na.rm = TRUE) ~ "Second Tier",
      ypll_rank <= quantile(ypll_rank, 0.75, na.rm = TRUE) ~ "Third Tier",
      TRUE ~ "Bottom Tier"
    ),
    
    # Improvement categories
    improvement_category = case_when(
      ypll_trend < -1 ~ "Improving",
      ypll_trend > 1 ~ "Worsening",
      TRUE ~ "Stable"
    ),
    
    # Access categories
    access_category = case_when(
      avg_pcp_rate > 80 & avg_uninsured_pct < 15 ~ "Good Access",
      avg_pcp_rate < 50 | avg_uninsured_pct > 20 ~ "Poor Access",
      TRUE ~ "Moderate Access"
    )
  )

## Year-level summaries
year_summary <- AL16_24SM1_enhanced %>%
  group_by(year) %>%
  summarise(
    counties_count = n(),
    avg_ypll_rate = mean(years_of_potential_life_lost_rate, na.rm = TRUE),
    median_ypll_rate = median(years_of_potential_life_lost_rate, na.rm = TRUE),
    avg_uninsured_pct = mean(percent_uninsured, na.rm = TRUE),
    avg_pcp_rate = mean(pcp_rate, na.rm = TRUE),
    counties_with_shortage = sum(pcp_ratio > 1500, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Year-over-year changes
    ypll_change = avg_ypll_rate - lag(avg_ypll_rate),
    uninsured_change = avg_uninsured_pct - lag(avg_uninsured_pct),
    pcp_change = avg_pcp_rate - lag(avg_pcp_rate),
    
    # Performance indicators
    shortage_pct = (counties_with_shortage / counties_count) * 100,
    
    # Trend directions
    ypll_direction = case_when(
      ypll_change < 0 ~ "Improving",
      ypll_change > 0 ~ "Worsening",
      TRUE ~ "Stable"
    )
  )

# =====================================
# 10. EXPORT DATASETS FOR TABLEAU
# =====================================

## Main dataset
tableau_main <- AL16_24SM1_enhanced %>%
  left_join(county_summary %>% select(county, avg_ypll_rate, ypll_trend, ypll_performance), 
            by = "county", suffix = c("", "_overall")) %>%
  select(
    # Identifiers
    year, county,
    # Core metrics
    premature_deaths, years_of_potential_life_lost_rate, 
    percent_uninsured, pcp_rate, pcp_ratio, preventable_hosp_rate,
    # Derived variables
    health_risk_score, performance_tier, pcp_shortage, access_category,
    ypll_quartile, uninsured_quartile,
    # Trends
    ypll_change_from_baseline, uninsured_change_from_baseline,
    ypll_yoy_change, uninsured_yoy_change,
    # County characteristics
    ypll_trend, avg_ypll_rate, ypll_performance
  )

## Statistical results
statistical_results <- bind_rows(
  # ANOVA results
  anova_summary %>%
    mutate(analysis_type = "ANOVA") %>%
    select(analysis_type, metric, f_statistic, p_value, significant, interpretation),
  
  # Regression results
  regression_summary %>%
    filter(term != "(Intercept)") %>%
    mutate(
      analysis_type = "Regression",
      metric = "years_of_potential_life_lost_rate"
    ) %>%
    select(analysis_type, metric = term, f_statistic = statistic, 
           p_value = p.value, significant, interpretation = term)
) %>%
  mutate(
    significance_level = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

## KPI table
create_kpi_table <- function(main_data, county_data, year_data) {
  latest_year_data <- main_data %>% filter(year == max(year, na.rm = TRUE))
  
  tibble(
    kpi_category = c(
      rep("State Averages", 5),
      rep("Best Performance", 4),
      rep("Worst Performance", 4),
      rep("Trends", 3)
    ),
    kpi_name = c(
      # State averages
      "Average YPLL Rate", "Average % Uninsured", "Average PCP Rate",
      "Counties with PCP Shortage", "Average Preventable Hosp Rate",
      
      # Best performers
      "Lowest YPLL Rate", "Lowest % Uninsured", "Highest PCP Rate",
      "Best YPLL Improvement",
      
      # Worst performers
      "Highest YPLL Rate", "Highest % Uninsured", "Lowest PCP Rate",
      "Worst YPLL Decline",
      
      # Trends
      "Overall YPLL Trend", "Counties Improving", "Access Improving Counties"
    ),
    kpi_value = c(
      # State averages
      round(mean(latest_year_data$years_of_potential_life_lost_rate, na.rm = TRUE), 1),
      round(mean(latest_year_data$percent_uninsured, na.rm = TRUE), 1),
      round(mean(latest_year_data$pcp_rate, na.rm = TRUE), 1),
      sum(latest_year_data$pcp_ratio > 1500, na.rm = TRUE),
      round(mean(latest_year_data$preventable_hosp_rate, na.rm = TRUE), 1),
      
      # Best/worst
      round(min(county_data$avg_ypll_rate, na.rm = TRUE), 1),
      round(min(county_data$avg_uninsured_pct, na.rm = TRUE), 1),
      round(max(county_data$avg_pcp_rate, na.rm = TRUE), 1),
      round(min(county_data$ypll_trend, na.rm = TRUE), 2),
      
      round(max(county_data$avg_ypll_rate, na.rm = TRUE), 1),
      round(max(county_data$avg_uninsured_pct, na.rm = TRUE), 1),
      round(min(county_data$avg_pcp_rate, na.rm = TRUE), 1),
      round(max(county_data$ypll_trend, na.rm = TRUE), 2),
      
      # Trends
      round(mean(county_data$ypll_trend, na.rm = TRUE), 3),
      sum(county_data$ypll_trend < 0, na.rm = TRUE),
      sum(county_data$improvement_category == "Improving", na.rm = TRUE)
    ),
    kpi_unit = c(
      "per 100k", "%", "per 100k", "counties", "per 1k",
      "per 100k", "%", "per 100k", "per year",
      "per 100k", "%", "per 100k", "per year",
      "per year", "counties", "counties"
    )
  )
}

kpi_table <- create_kpi_table(tableau_main, county_summary, year_summary)

## Export all files
write_csv(tableau_main, "alabama_health_main.csv")
write_csv(county_summary, "alabama_county_summary.csv")
write_csv(year_summary, "alabama_year_summary.csv")
write_csv(statistical_results, "alabama_statistical_results.csv")
write_csv(kpi_table, "alabama_kpis.csv")

# Simple correlation dataset
correlation_data <- tableau_main %>%
  select(year, county, percent_uninsured, pcp_rate, pcp_ratio,
         years_of_potential_life_lost_rate, preventable_hosp_rate,
         premature_deaths, pcp_shortage)

write_csv(correlation_data, "alabama_correlations.csv")

# =====================================
# 11. FINAL SUMMARY
# =====================================

cat("=== ANALYSIS COMPLETE ===\n")
cat("Files created for Tableau:\n")
cat("- alabama_health_main.csv (primary dataset)\n")
cat("- alabama_county_summary.csv\n")
cat("- alabama_year_summary.csv\n")
cat("- alabama_statistical_results.csv\n")
cat("- alabama_kpis.csv\n")
cat("- alabama_correlations.csv\n\n")

cat("Key findings:\n")
cat("- Total counties analyzed:", n_distinct(tableau_main$county), "\n")
cat("- Years covered:", paste(range(tableau_main$year, na.rm = TRUE), collapse = " - "), "\n")
cat("- Counties with physician shortages:", sum(tableau_main$pcp_shortage == "Shortage" & tableau_main$year == 2024, na.rm = TRUE), "\n")
cat("- Average YPLL rate (2024):", round(mean(tableau_main$years_of_potential_life_lost_rate[tableau_main$year == 2024], na.rm = TRUE), 1), "per 100k\n")

##testing for connectivity to GitHub DA Practice
