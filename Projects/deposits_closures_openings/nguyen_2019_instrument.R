rm(list=ls())
library(data.table)
library(lubridate)

# ---- 1. Load Data ----
mergers_dt <- readRDS("C:/data/m_and_a_data.rds")
sod_dt     <- readRDS("C:/data/fdic_sod_2000_2025.rds")

# Convert to data.tables if they aren't already
setDT(mergers_dt)
setDT(sod_dt)

sod_dt <- sod_dt[,.(UNINUMBR,ASSET,CERT,ZIPBR,YEAR)]
mergers_dt <- mergers_dt[!is.na(OUT_CERT)]

gc()

# ---- 2. Data Cleaning & Standardization ----

# A. Extract Year and Assets (SOD)
# Create a lookup table for Bank Assets per Year.
bank_assets <- sod_dt[, .(Total_Assets = max(ASSET, na.rm = TRUE)), 
                      by = .(CERT, YEAR)]

# B. Clean Merger Dates (Mergers)
# Format is "YYYY-MM-DDTHH:MM:SS". We just need the date part.
mergers_dt[, EFFDATE := as.Date(substr(EFFDATE, 1, 10))]
mergers_dt[, Merger_Year := year(EFFDATE)]

# Define the "Reference Year" (t-1)
# We verify overlap and asset size in the year PRIOR to the merger.
mergers_dt[, Ref_Year := Merger_Year - 1]

# ---- 3. Apply the "Large Bank" Filter (Nguyen >$10B) ----

# Join Buyer Assets
mergers_dt <- merge(mergers_dt, bank_assets, 
                    by.x = c("ACQ_CERT", "Ref_Year"), 
                    by.y = c("CERT", "YEAR"), 
                    all.x = TRUE)
setnames(mergers_dt, "Total_Assets", "Buyer_Assets")

# Join Target Assets
mergers_dt <- merge(mergers_dt, bank_assets, 
                    by.x = c("OUT_CERT", "Ref_Year"), 
                    by.y = c("CERT", "YEAR"), 
                    all.x = TRUE)
setnames(mergers_dt, "Total_Assets", "Target_Assets")

# FILTER: Both banks must have > $10 Billion in assets.
# Since SOD assets are in Thousands, $10B = 10,000,000.
large_mergers <- mergers_dt[Buyer_Assets >= 10000000 & Target_Assets >= 10000000]

# Keep only unique Merger IDs to avoid duplication
# (Using TRANSNUM or creating a unique ID based on certs/year)
large_mergers <- unique(large_mergers, by = c("ACQ_CERT", "OUT_CERT", "Ref_Year"))

# ---- 4. Construct the Geographic Instrument (The Overlap) ----

# Strategy:
# 1. Get all Zip codes where Buyers had branches in Ref_Year.
# 2. Get all Zip codes where Targets had branches in Ref_Year.
# 3. The Instrument = The intersection of these lists per merger.

# Subset SOD to only relevant years and banks to speed up processing
relevant_years <- unique(large_mergers$Ref_Year)
relevant_certs <- unique(c(large_mergers$ACQ_CERT, large_mergers$OUT_CERT))

sod_subset <- sod_dt[YEAR %in% relevant_years & CERT %in% relevant_certs, 
                     .(YEAR, CERT, ZIPBR)]

# Create a table of Buyer Locations
buyer_locs <- merge(large_mergers[, .(ACQ_CERT, OUT_CERT, Ref_Year, TRANSNUM)],
                    sod_subset,
                    by.x = c("ACQ_CERT", "Ref_Year"),
                    by.y = c("CERT", "YEAR"),
                    all.x = TRUE, allow.cartesian = TRUE)
setnames(buyer_locs, "ZIPBR", "Buyer_Zip")

# Create a table of Target Locations
target_locs <- merge(large_mergers[, .(ACQ_CERT, OUT_CERT, Ref_Year, TRANSNUM)],
                     sod_subset,
                     by.x = c("OUT_CERT", "Ref_Year"),
                     by.y = c("CERT", "YEAR"),
                     all.x = TRUE, allow.cartesian = TRUE)
setnames(target_locs, "ZIPBR", "Target_Zip")

# ---- 5. Find the Intersection (Treated Zips) ----

# We join Buyer Locations and Target Locations on the Merger ID (TRANSNUM) 
# AND the Zip Code. If they match, it's an overlap.
treated_zips <- merge(buyer_locs, target_locs, 
                      by.x = c("TRANSNUM", "Buyer_Zip"), 
                      by.y = c("TRANSNUM", "Target_Zip"))

# Clean up the result
setnames(treated_zips, "Buyer_Zip", "ZIPBR")
final_instrument <- treated_zips[, .(ZIPBR, 
                                     Event_Year = Ref_Year.x + 1, # The year the treatment "starts"
                                     Merger_TransNum = TRANSNUM,
                                     Buyer_Cert = ACQ_CERT.x,
                                     Target_Cert = OUT_CERT.x)]

# Deduplicate: A Zip might be treated by multiple mergers in history, 
# but usually, we care if it is treated in a specific year.
final_instrument <- unique(final_instrument)

# ---- 6. Output ----
# This table contains the Zip Codes that are "Treated" and the year the treatment occurs.
saveRDS(final_instrument, "C:/data/nguyen_instrument_zips.rds")

# View result
print(head(final_instrument))





# Test first stage --------------------------------------------------------

library(data.table)
library(fixest)   # The gold standard for FE models
library(lubridate)

# ---- 1. Load Data ----
# Load your specific files
branch_panel <- readRDS("C:/data/closure_opening_data_simple.rds")
instrument_dt <- readRDS("C:/data/nguyen_instrument_zips.rds")

setDT(branch_panel)
setDT(instrument_dt)

# ---- 2. Data Prep: Define the Event Panel ----

# A. Identify the first "Treatment Year" per Zip
# (If a Zip is treated multiple times, we typically take the first instance)
zip_event_years <- instrument_dt[, .(Event_Year = min(Event_Year)), by = ZIPBR]

# B. Standardize Join Keys
branch_panel[, ZIPBR := sprintf("%05s", as.character(ZIPBR))]
zip_event_years[, ZIPBR := sprintf("%05s", as.character(ZIPBR))]

# C. Merge to create the Analysis Dataset
# 'treated_panel' will contain both Treated branches and Control branches
treated_panel <- merge(branch_panel, zip_event_years, by = "ZIPBR", all.x = TRUE)

# D. Calculate Relative Time
# rel_year = The year relative to the merger approval
treated_panel[, rel_year := YEAR - Event_Year]

# ---- 3. Handling the Control Group (Crucial for fixest) ----
# In fixest, rows with NA in the variable of interest are dropped. 
# To keep Control branches (who have NA rel_year) in the regression 
# (so they contribute to the Year and Zip Fixed Effects), we assign them 
# a "dummy" relative year that is far outside our plotting window.
treated_panel[is.na(rel_year), rel_year := -1000]

# ---- 4. Estimation (Replicating Eq 3) ----
# We use i() to create the event study dummies.
# ref = -1:  Sets t = -1 as the baseline (coefficient = 0).
# keep = -5:8 : Only estimates coefficients for this window (bins others or drops).
# Note: The value -1000 (Controls) is excluded from 'keep', so they serve 
# purely as part of the reference group for the Fixed Effects.

est_event <- feols(closed ~ i(rel_year, ref = -1, keep = -5:5) | 
                     ZIPBR + YEAR,               # Fixed Effects (Tract + Year)
                   cluster = ~STCNTYBR,        # Clustered Errors (County level)
                   data = treated_panel)

# Print results to console to check significance
print(est_event)

# ---- 5. Visualization (Replicating Figure 2) ----
# iplot automatically extracts the coefficients from i() and plots them.

iplot(est_event, 
      # Visual Settings
      main = "Impact of Merger Exposure on Branch Closings",
      xlab = "Years since merger",
      ylab = "Probability of Branch Closing",
      
      # Confidence Intervals
      ci_level = 0.95,       # 95% CI bars
      ci.width = 0,          # Width of the end caps (0 = simple lines)
      pt.join = TRUE,        # Join points with a line (like Figure 2)
      grid = TRUE,           # Add background grid
      
      # Colors (mimicking the blue style)
      col = "navy",
      pt.pch = 20,           # Solid circle points
      pt.cex = 1.5,
      
      xlim = c(-6, 6))          # Point size

# Add the vertical dashed line at t=0 manually if desired
abline(v = 0, col = "gray", lty = 2)
