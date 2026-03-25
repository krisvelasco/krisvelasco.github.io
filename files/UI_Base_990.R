## ============================================================
## UI_Base_990.R
##
## GOAL
##   Download and merge key sections of the IRS Form 990 from
##   the Urban Institute's NCCS e-file data (v2.1) for 2009-2024.
##   Produces ONE combined CSV with one row per filing (URL),
##   joining across six NCCS tables:
##     - F9-P01-T00-SUMMARY      (financial summary)
##     - F9-P09-T00-EXPENSES     (functional expenses)
##     - F9-P08-T00-REVENUE      (revenue detail)
##     - F9-P03-T00-MISSION      (mission statement / program service)
##     - F9-P00-T00-HEADER       (filer metadata: EIN, tax year, etc.)
##     - SF-P01-T00-FRGN-ACTS    (Schedule F foreign activities flag)
##
##   Only full Form 990 filings are retained (990-EZ excluded).
##   All tables are joined by the `url` column, which is NCCS's
##   unique identifier for each electronic filing.
##
## REQUIRED PACKAGES
##   dplyr, readr, purrr
##
## DATA SOURCE
##   NCCS e-file v2.1: https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_1
##
## OUTPUT
##   nccs_summary_990_2009_2024.csv (saved to working directory)
##
## AUTHOR
##   Kristopher Velasco (Princeton University)
## ============================================================

library(dplyr)
library(readr)
library(purrr)

## ----------------------------
## Configuration
## ----------------------------
base  <- "https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_1"
years <- 2009:2024

## ----------------------------
## Helper Functions
## ----------------------------

# Construct the S3 URL for a given NCCS table prefix and year.
build_url <- function(prefix, year) {
  sprintf("%s/%s-%d.CSV", base, prefix, year)
}

# Download and read a single NCCS CSV file. All columns are read as
# character to avoid type-coercion issues across years; column names
# are lowercased and trimmed for consistency.
read_nccs_csv <- function(prefix, year) {
  u <- build_url(prefix, year)

  df <- readr::read_csv(
    file = u,
    col_types = cols(.default = col_character()),
    progress = FALSE,
    show_col_types = FALSE
  )
  
  # Make names lower-case (and trim whitespace)
  names(df) <- tolower(trimws(names(df)))
  
  # Basic checks
  if (!"url" %in% names(df)) {
    stop(sprintf("Missing `url` column in %s (year %d). Columns: %s",
                 u, year, paste(names(df), collapse = ", ")),
         call. = FALSE)
  }
  
  df
}

# Parse the return_time_stamp field into POSIXct. Handles multiple
# date formats found across different filing years.
parse_return_ts <- function(x) {
  x2 <- trimws(x)
  x2 <- gsub("T", " ", x2, fixed = TRUE)
  x2 <- sub("Z$", "", x2)
  suppressWarnings(as.POSIXct(
    x2,
    tz = "UTC",
    tryFormats = c(
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d %H:%M:%OS",
      "%m/%d/%Y %H:%M:%S",
      "%m/%d/%Y %H:%M"
    )
  ))
}

## ----------------------------
## Main Processing Function
## ----------------------------

# Download all six NCCS tables for a given year, filter to full 990
# filings only, resolve column overlaps, and join into a single
# data frame keyed by `url`.
read_join_one_year <- function(year) {
  message("Processing year: ", year)

  summary    <- read_nccs_csv("F9-P01-T00-SUMMARY",     year)
  expenses   <- read_nccs_csv("F9-P09-T00-EXPENSES",    year)
  revenue    <- read_nccs_csv("F9-P08-T00-REVENUE",     year)  
  mission    <- read_nccs_csv("F9-P03-T00-MISSION",     year)
  header     <- read_nccs_csv("F9-P00-T00-HEADER",      year)
  sf_frgn    <- read_nccs_csv("SF-P01-T00-FRGN-ACTS",   year)
  
  # Ensure required columns exist in SUMMARY
  if (!"return_type" %in% names(summary)) {
    stop(sprintf("Missing `return_type` in SUMMARY (year %d). Columns: %s",
                 year, paste(names(summary), collapse = ", ")),
         call. = FALSE)
  }
  
  # Keep only 990 rows from SUMMARY
  summary <- summary %>% filter(return_type == "990")
  
  # Drop overlapping columns from EXPENSES (keep SUMMARY versions), except url
  overlap <- intersect(names(expenses), names(summary))
  overlap <- setdiff(overlap, "url")
  if (length(overlap) > 0) expenses <- expenses %>% select(-any_of(overlap))
  
  # Drop overlapping columns from REVENUE (keep SUMMARY/EXPENSES versions), except url
  overlap_r <- intersect(names(revenue), c(names(summary), names(expenses)))
  overlap_r <- setdiff(overlap_r, "url")
  if (length(overlap_r) > 0) revenue <- revenue %>% select(-any_of(overlap_r))
  
  # Drop overlapping columns from MISSION (keep prior versions), except url
  overlap2 <- intersect(names(mission), c(names(summary), names(expenses), names(revenue)))
  overlap2 <- setdiff(overlap2, "url")
  if (length(overlap2) > 0) mission <- mission %>% select(-any_of(overlap2))
  
  # Drop overlapping columns from HEADER (keep prior versions), except url
  overlap3 <- intersect(names(header), c(names(summary), names(expenses), names(revenue), names(mission)))
  overlap3 <- setdiff(overlap3, "url")
  if (length(overlap3) > 0) header <- header %>% select(-any_of(overlap3))
  
  # Drop overlapping columns from SF foreign activities (keep prior versions), except url
  overlap4 <- intersect(names(sf_frgn), c(names(summary), names(expenses), names(revenue), names(mission), names(header)))
  overlap4 <- setdiff(overlap4, "url")
  if (length(overlap4) > 0) sf_frgn <- sf_frgn %>% select(-any_of(overlap4))
  
  # Join on url (no suffixes needed because we removed overlaps)
  out <- summary %>%
    left_join(expenses, by = "url") %>%
    left_join(revenue,  by = "url") %>%
    left_join(mission,  by = "url") %>%
    left_join(header,   by = "url") %>%
    left_join(sf_frgn,  by = "url")
  
  # Validate columns needed for later dedupe exist
  req <- c("org_ein", "tax_year", "return_time_stamp")
  missing_req <- setdiff(req, names(out))
  if (length(missing_req) > 0) {
    stop(sprintf("After join (year %d), missing required columns: %s",
                 year, paste(missing_req, collapse = ", ")),
         call. = FALSE)
  }
  
  out
}

## ----------------------------
## Execute: Download, join, and save
## ----------------------------

# Download and join all six tables for each year, then row-bind into one data frame.
all_years <- purrr::map_dfr(years, read_join_one_year)

# Save to working directory. Change the path below to your preferred output location.
write_csv(all_years, "nccs_summary_990_2009_2024.csv")