rm(list=ls())
library(data.table)
library(stringr)
library(tidycensus)
library(dplyr)

states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

# Variables to retrieve
acs_vars <- c(
  median_income = "B19013_001",          # Median household income
  pct_college_educated = "B23006_023",   # Bachelor's degree (age 25+)
  total_education = "B23006_001",# Total population 25+
  median_age = "B01002_001",             # Median age
  white_population = "B02001_002",       # White alone population
  total_population = "B01003_001"        # Total population
)

# Initialize an empty list to hold the data for each state
all_states_data <- list()

# Loop through each state to get ACS data at the tract level
for(yr in seq(2010,2024,by=1)) {
  for (state in states) {
    state_yr <-paste(state,yr)
    message("Getting data for state: ", state_yr)

    # Get the ACS data for the state at the tract level
    state_data <- get_acs(
      geography = "tract",
      variables = acs_vars,
      state = state,
      year = yr,
      survey = "acs5",
      output = "wide"
    )

    state_data <- data.table(state_data)
    state_data[,yr:=yr]

    # Store the data for the state
    all_states_data[[state_yr]] <- state_data
  }
}

acs_data_combined <- bind_rows(all_states_data)

acs_data <- acs_data_combined %>%
  mutate(
    pct_college_educated = 100 * (pct_college_educatedE / total_educationE),  # Bachelor's degree holders as a percentage of 25+ population
    white_population_pct = 100 * (white_populationE / total_populationE),   # White population as a percentage of total population
    median_income = median_incomeE,
    median_age = median_ageE
  ) %>%
  select(yr,GEOID, median_income, pct_college_educated, median_age, white_population_pct) %>%
  rename(
    tract = GEOID
  )

saveRDS(acs_data,"C:/data/acs_data_2010_2024_tract.rds")