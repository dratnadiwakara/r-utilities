rm(list=ls())
library(data.table)
library(stringr)
library(tidycensus)
library(dplyr)

# Variables to retrieve
acs_vars <- c(
  median_income = "B19013_001",          # Median household income
  pct_college_educated = "B23006_023",   # Bachelor's degree (age 25+)
  total_education = "B23006_001",        # Total population 25+
  median_age = "B01002_001",             # Median age
  white_population = "B02001_002",       # White alone population
  total_population = "B01003_001"        # Total population
)

# Initialize an empty list to hold the data
all_years_data <- list()

# Loop through years
# Note: ACS 5-year data is usually released the year after. 
# 2024 data might not be available yet depending on the current date.
years <- seq(2010, 2024)

for(yr in years) {
  message("Getting county-level data for year: ", yr)
  
  # Use tryCatch to skip years that are not yet available or have API errors
  tryCatch({
    # Fetch all US counties at once (no state loop needed for county geo)
    year_data <- get_acs(
      geography = "county",
      variables = acs_vars,
      year = yr,
      survey = "acs5",
      output = "wide",
      show_call = TRUE
    )
    
    year_data <- data.table(year_data)
    year_data[, yr := yr]
    
    # Store the data
    all_years_data[[as.character(yr)]] <- year_data
    
  }, error = function(e) {
    message("Could not retrieve data for year: ", yr, ". Error: ", e$message)
  })
}

acs_data_combined <- bind_rows(all_years_data)

acs_data <- acs_data_combined %>%
  mutate(
    pct_college_educated = 100 * (pct_college_educatedE / total_educationE),  # Bachelor's degree holders as %
    white_population_pct = 100 * (white_populationE / total_populationE),     # White population as %
    median_income = median_incomeE,
    median_age = median_ageE,
    total_population = total_populationE
  ) %>%
  select(yr, GEOID, NAME, median_income, pct_college_educated, median_age, white_population_pct, total_population) %>%
  rename(
    county_fips = GEOID,
    county_name = NAME
  )

# Save the file
saveRDS(acs_data, "C:/data/acs_data_2010_2024_county.rds")