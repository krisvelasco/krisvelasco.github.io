## ---- NCCS efile v2_1 (2009–2024): join selected GRANTS tables by `url` (keep long) ----
## Rules:
##  - For each year, download the listed CSVs (skip missing)
##  - Lowercase all column names
##  - Join within-year by `url` WITHOUT forcing 1 row per url (urls can repeat)
##  - Stack (row-bind) all years into one master dataset

# If needed:
# install.packages(c("dplyr", "readr", "purrr"))

library(dplyr)
library(readr)
library(purrr)

base  <- "https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_1"
years <- 2009:2024

prefixes <- c(
  "SI-P01-T00-GRANTS-INFO",
  "SI-P02-T00-GRANTS-US-ORGS-GOVTS",
  "SI-P02-T01-GRANTS-US-ORGS-GOVTS",
  "SI-P03-T01-GRANTS-US-INDIV",
  "SI-P99-T00-GRANTS-US-ORGS-GOVTS"
)

build_url <- function(prefix, year) sprintf("%s/%s-%d.CSV", base, prefix, year)

read_nccs_safe <- function(prefix, year) {
  u <- build_url(prefix, year)
  
  df <- tryCatch(
    read_csv(
      file = u,
      col_types = cols(.default = col_character()),
      progress = FALSE,
      show_col_types = FALSE
    ),
    error = function(e) {
      message(sprintf("Skipping (missing/read error): %s", u))
      return(NULL)
    }
  )
  if (is.null(df)) return(NULL)
  
  names(df) <- tolower(trimws(names(df)))
  
  if (!"url" %in% names(df)) {
    message(sprintf("Skipping (no `url` column): %s", u))
    return(NULL)
  }
  
  df %>% mutate(source_file = prefix, source_url = u, file_year = year)
}

# full_join that won’t error on newer dplyr’s relationship arg (and won’t require it on older dplyr)
safe_full_join <- function(x, y, by = "url") {
  # Drop overlapping columns from y (except url) so we don't get suffixes.
  # (Assumes same-named columns are redundant across files.)
  overlap <- setdiff(intersect(names(x), names(y)), by)
  if (length(overlap) > 0) y <- y %>% select(-any_of(overlap))
  
  fj_formals <- names(formals(dplyr::full_join))
  if ("relationship" %in% fj_formals) {
    dplyr::full_join(x, y, by = by, relationship = "many-to-many")
  } else {
    dplyr::full_join(x, y, by = by)
  }
}

join_one_year_long <- function(year) {
  message("Processing year: ", year)
  
  dfs <- map(prefixes, ~ read_nccs_safe(.x, year))
  dfs <- dfs[!vapply(dfs, is.null, logical(1))]
  
  if (length(dfs) == 0) {
    message("No files found for year: ", year)
    return(NULL)
  }
  
  # Prefer GRANTS-INFO as the base if it exists; otherwise use first available
  base_idx <- which(vapply(dfs, function(d) any(d$source_file == "SI-P01-T00-GRANTS-INFO"), logical(1)))
  if (length(base_idx) > 0) {
    base_df <- dfs[[base_idx[1]]]
    rest   <- dfs[-base_idx[1]]
  } else {
    base_df <- dfs[[1]]
    rest   <- dfs[-1]
  }
  
  reduce(rest, ~ safe_full_join(.x, .y, by = "url"), .init = base_df)
}

master_2009_2024_long <- map_dfr(years, join_one_year_long)

# Result:
write_csv(master_2009_2024_long, "/Users/kv7379/Library/CloudStorage/OneDrive-PrincetonUniversity/Foreign 990/Datasets/nccs_schedule_i_2009_2024.csv")
