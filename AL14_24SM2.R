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
AL14SM2[1, 4] <- "State Total"
AL15SM2[1, 4] <- "State Total"
AL16SM2[1, 4] <- "State Total"
AL17SM2[1, 4] <- "State Total"
AL18SM2[1, 4] <- "State Total"
AL19SM2[1, 4] <- "State Total"
AL20SM2[1, 4] <- "State Total"
AL21SM2[1, 4] <- "State Total"
AL22SM2[1, 4] <- "State Total"
AL23SM2[1, 4] <- "State Total"
# AL24SM2 is skipped 

# Verify it worked
cat("Verification:\n")
cat("AL14SM2[1,4]:", AL14SM2[1, 4], "\n")
cat("AL15SM2[1,4]:", AL15SM2[1, 4], "\n")
cat("AL16SM2[1,4]:", AL16SM2[1, 4], "\n")
cat("AL17SM2[1,4]:", AL17SM2[1, 4], "\n")
cat("AL18SM2[1,4]:", AL18SM2[1, 4], "\n")
cat("AL19SM2[1,4]:", AL19SM2[1, 4], "\n")
cat("AL20SM2[1,4]:", AL20SM2[1, 4], "\n")
cat("AL21SM2[1,4]:", AL21SM2[1, 4], "\n")
cat("AL22SM2[1,4]:", AL22SM2[1, 4], "\n")
cat("AL23SM2[1,4]:", AL23SM2[1, 4], "\n")
cat("AL24SM2 was skipped (no changes made)\n")




  
