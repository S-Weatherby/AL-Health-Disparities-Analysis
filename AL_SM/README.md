# Alabama Health Disparities Analysis (2014-2024)

## Overview
Comprehensive longitudinal analysis examining racial health disparities across all 67 Alabama counties using 11 years of County Health Rankings data. This project demonstrates advanced statistical analysis, data visualization, and reproducible research practices.

## Key Findings
- **Statistically significant worsening** of racial disparities over the decade (p < 0.01)
- **Strengthening correlation** between poverty and health outcomes over time

## Technical Highlights
- **Data Processing:** Automated pipeline handling 11 separate datasets with validation protocols
- **Statistical Analysis:** ANOVA testing, correlation analysis, time trend regression
- **Visualization:** Interactive Tableau dashboards with county-level heat maps
- **Reproducibility:** Full documentation and standardized methodology

## Repository Structure
- `code/` - R scripts for data processing, analysis, and visualization
- `data/` - Raw and processed datasets (anonymized)
- `outputs/` - Generated figures, tables, and reports
- `documentation/` - Detailed methodology and findings
- `tableau/` - Dashboard screenshots and documentation

## Technologies Used
- **R:** tidyverse, ggplot2, statistical packages
- **Tableau:** Advanced dashboards and interactive visualizations
- **Statistical Methods:** ANOVA, regression analysis, correlation matrices

## How to Reproduce This Analysis
1. Clone this repository
2. Install required R packages: `install.packages(c("tidyverse", "ggplot2", "..."))`
3. Run scripts in order: `01_data_cleaning.R` → `02_statistical_analysis.R` → `03_visualizations.R`
4. View outputs in the `outputs/` folder

## Impact & Applications
This analysis provides actionable insights for:
- Public health policy development
- Healthcare resource allocation
- Health equity initiatives
- Rural health program planning

## Contact
Shelita Smith - shelita17smith@gmail.com 
MPH Health Policy & Organization | Healthcare Analytics Professional
