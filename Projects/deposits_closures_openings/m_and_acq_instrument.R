rm(list=ls())
library(data.table)

#------------------------------------------------
# 1. Load data
#------------------------------------------------
mna  <- readRDS("C:/data/m_and_a_data.rds")          # history-based M&A events
sod  <- readRDS("C:/data/fdic_sod_2000_2025.rds")    # SOD branch data

setDT(mna)
setDT(sod)

# Make sure CERT and DEPSUMBR are numeric
sod[, CERT     := as.integer(CERT)]
sod[, DEPSUMBR := as.numeric(DEPSUMBR)]

mna[, ACQ_CERT := as.integer(ACQ_CERT)]
mna[, OUT_CERT := as.integer(OUT_CERT)]
mna[, SUR_CERT := as.integer(SUR_CERT)]


#------------------------------------------------
# 2. Event year (from EFFDATE)
#------------------------------------------------
# EFFDATE is an ISO-like string: "YYYY-MM-DD..." – year is first 4 chars
mna[, eff_year := as.integer(substr(EFFDATE, 1, 4))]

# Restrict to years for which we have SOD in the *previous* year
years_sod <- unique(as.integer(sod$YEAR))
years_evt <- sort(unique(mna$eff_year))
years_use <- years_evt[(years_evt - 1) %in% years_sod]

#------------------------------------------------
# 3. Loop over years and build ZIP–year panel
#------------------------------------------------
zip_year_list <- vector("list", length(years_use))

for (i in seq_along(years_use)) {
  yr <- years_use[i]
  
  # Events in current year
  mna_yr <- mna[eff_year == yr]
  
  # Set of acquiring and target banks in this year
  acq_certs    <- unique(na.omit(mna_yr$ACQ_CERT))
  target_certs <- unique(na.omit(mna_yr$OUT_CERT))
  
  # Closed banks in this year: OUT_CERT is NA
  closed_certs <- integer(0)
  closed_certs <- unique(na.omit(mna_yr[is.na(OUT_CERT), CERT]))

  # Branch universe in previous year
  sod_prev <- sod[as.integer(YEAR) == (yr - 1)]
  
  if (nrow(sod_prev) == 0L) {
    zip_year_list[[i]] <- NULL
    next
  }
  
  # Flags by bank type
  sod_prev[, is_acq    := CERT %in% acq_certs]
  sod_prev[, is_target := CERT %in% target_certs]
  sod_prev[, is_closed := CERT %in% closed_certs]
  
  # Aggregate to ZIP–(current)year level using previous-year branches
  # Only keep ZIPs that have at least one relevant branch
  zip_panel_yr <- sod_prev[
    is_acq | is_target | is_closed,
    .(
      year                      = yr,
      n_br_target_prev          = sum(is_target),
      n_br_acq_prev             = sum(is_acq),
      dep_target_prev           = sum(DEPSUMBR[is_target], na.rm = TRUE),
      dep_acq_prev              = sum(DEPSUMBR[is_acq],    na.rm = TRUE),
      n_br_closed_prev          = sum(is_closed),
      dep_closed_prev           = sum(DEPSUMBR[is_closed], na.rm = TRUE)
    ),
    by = .(zip = ZIPBR)
  ]
  
  zip_year_list[[i]] <- zip_panel_yr
}

zip_year_panel <- rbindlist(zip_year_list, fill = TRUE)

#------------------------------------------------
# 4. Inspect / save
#------------------------------------------------
# print(zip_year_panel[])
saveRDS(zip_year_panel, "C:/data/zip_year_mna_panel.rds")


# Quick sanity check
# print(dim(all_sod))
# print(head(all_sod))
