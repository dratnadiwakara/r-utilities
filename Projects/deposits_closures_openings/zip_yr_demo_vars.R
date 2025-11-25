rm(list=ls())
library(data.table)
library(stringr)
library(tidycensus)
library(dplyr)
library(purrr)

# 1. SETUP
# ---------------------------------------------------------
# census_api_key("YOUR_KEY_HERE", install = TRUE)

years_to_process <- seq(2013, 2022) 

# Define the specific ACS variables you requested
acs_vars <- c(
  median_income        = "B19013_001",  # Median household income
  pct_college_educated = "B23006_023",  # Bachelor's degree or higher (numerator)
  total_education      = "B23006_001",  # Total population 25-64 (denominator)
  median_age           = "B01002_001",  # Median age
  total_population     = "B01003_001"   # Total population
)

# 2. PROCESSING FUNCTION
# ---------------------------------------------------------
process_year <- function(yr) {
  
  message(paste("Processing Year:", yr))
  
  # --- PART A: CENSUS DATA ---
  tryCatch({
    # We use output = "wide" to get variables in columns (ending in 'E' for Estimate)
    pop_data <- get_acs(
      geography = "zcta",
      variables = acs_vars,
      year = yr,
      survey = "acs5", 
      output = "wide",
      show_call = FALSE,
      quiet = TRUE
    )
    
    setDT(pop_data)
    
    # Calculate the education percentage
    # Note: B23006 universe is usually 25-64 years old
    pop_data[, college_grad_frac := pct_college_educatedE / total_educationE]
    
    # Select and rename the columns we want (dropping the 'E' suffix for cleanliness)
    # We keep GEOID as 'zipcode' to match IRS data
    census_clean <- pop_data[, .(
      zipcode = GEOID,
      population = total_populationE,
      median_income = median_incomeE,
      median_age = median_ageE,
      college_grad_frac = college_grad_frac
    )]
    
  }, error = function(e) {
    message(paste("Warning: Census fetch failed for", yr, "-", e$message))
    return(NULL)
  })
  
  # --- PART B: IRS DATA ---
  file_path <- paste0("C:/data/irs_zip/", substr(yr, 3, 4), "zpallagi.csv")
  
  if (!file.exists(file_path)) {
    message(paste("Skipping IRS: File not found for", yr))
    return(NULL) 
  }
  
  irs <- fread(file_path)
  names(irs) <- tolower(names(irs))
  
  # Standardize Zipcode (ensure 5 digits)
  irs[, zipcode := sprintf("%05d", as.numeric(zipcode))]
  
  # Columns: N1(count), N00600(div count), N01000(cap gain count), A00100(AGI amount)
  names(irs) <- tolower(names(irs))
  target_cols <- c("n1", "n00600", "n01000", "a00100")
  
  if (!all(target_cols %in% names(irs))) {
    message(paste("Skipping", yr, "- IRS column mismatch (likely pre-2010)."))
    return(NULL)
  }
  
  # Aggregate IRS data by Zipcode
  irs_agg <- irs[, lapply(.SD, function(x) sum(as.numeric(x), na.rm = TRUE)), 
                 by = zipcode, 
                 .SDcols = target_cols]
  
  setnames(irs_agg, target_cols, c("total_returns", "returns_dividend", "returns_cap_gain", "total_agi_thousands"))
  
  # IRS Calculations
  irs_agg[, `:=`(
    dividend_frac = returns_dividend / total_returns,
    capital_gain_frac = returns_cap_gain / total_returns,
    avg_agi_per_return = (total_agi_thousands * 1000) / total_returns,
    year = yr
  )]
  
  irs_final <- irs_agg[, .(zipcode, year, dividend_frac, capital_gain_frac, avg_agi_per_return)]
  
  # --- PART C: MERGE ---
  # Left Join: Keep IRS data, add Census data where available
  combined <- merge(census_clean,irs_final, by = "zipcode", all.x = TRUE,all.y=T)
  
  return(combined)
}

# 3. EXECUTE
# ---------------------------------------------------------
final_panel <- map_dfr(years_to_process, process_year)

# Filter out IRS "00000" aggregates
final_panel <- final_panel[zipcode != "00000"]

print(head(final_panel))

# Save
saveRDS(final_panel, "C:/data/zip_irs_acs.rds")
