rm(list=ls())
library(dplyr)
library(readxl)

# 1. Read the data
# We use read_excel for .xlsx files. 
# col_types must be a vector of types corresponding to the columns in order:
# ZIP (text), COUNTY (text), RES_RATIO (numeric), BUS_RATIO (numeric), OTH_RATIO (numeric), TOT_RATIO (numeric)
df <- read_excel("C:\\data\\ZIP_COUNTY_122019.xlsx", 
                 col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric"))

# 2. Filter for the primary county
# We group by ZIP and keep only the row with the highest Total Ratio.
primary_county <- df %>%
  group_by(ZIP) %>%
  # slice_max selects the row with the highest value in TOT_RATIO.
  # with_ties = FALSE ensures that if a ZIP is split exactly 50/50, 
  # it forces a choice so you strictly return 1 county per ZIP.
  slice_max(TOT_RATIO, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ZIP, COUNTY)

# 3. View or Save results
print(head(primary_county))

# Optional: Write the clean version to a new CSV
# We use write.csv or readr::write_csv (if readr is loaded) to save the output
write.csv(primary_county, "C:/data/zip_to_county_mapping.csv", row.names = FALSE)







# 1. Read the data
# We use read_excel for .xlsx files. 
# col_types must be a vector of types corresponding to the columns in order:
# ZIP (text), CBSA (text), RES_RATIO (numeric), BUS_RATIO (numeric), OTH_RATIO (numeric), TOT_RATIO (numeric)
# Note: We read CBSA as text to preserve any potential leading zeros or specific formatting.
df <- read_excel("C:\\data\\zip_cbsa_122019.xlsx", 
                 col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric"))

# 2. Filter for the primary CBSA
# We group by ZIP and keep only the row with the highest Total Ratio.
primary_cbsa <- df %>%
  group_by(ZIP) %>%
  # slice_max selects the row with the highest value in TOT_RATIO.
  # with_ties = FALSE ensures strictly one CBSA per ZIP.
  slice_max(TOT_RATIO, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ZIP, CBSA)

# 3. View or Save results
print(head(primary_cbsa))

# Optional: Write the clean version to a new CSV
write.csv(primary_cbsa, "C:/data/zip_to_cbsa_mapping.csv", row.names = FALSE)