## ============================================================
## UI_Schedule_F_990.R  (Rebuild: URL-wide destination totals)
##
## GOAL
##   Create ONE ROW PER URL (NCCS unique filing identifier) for 2009–2024
##   containing WIDE columns for:
##     - Region totals and Country totals (parsed from the REGION field text)
##     - For each destination, include four components:
##         (1) total_spending
##         (2) own_ops_spending (Schedule F Part I)
##         (3) cash_grants      (Schedule F Parts II + III cash)
##         (4) noncash_grants   (Schedule F Parts II + III noncash)
##
## CRITICAL CONSTRAINTS (per your instructions)
##   - The ONLY destination field is the NCCS “region” field for each SF table.
##   - Filers often put countries/cities inside that “region” string.
##   - If region string contains multiple countries/cities, split amounts evenly.
##   - If a city/province appears, infer country; misspellings must be tolerated.
##   - Country totals should reconcile to region totals by allocating any
##     “no-country-parsed” amounts to country="UNSPECIFIED" (ISO3="UNSPEC").
##
##
## OUTPUT
##   - nccs_schedule_f_url_wide_2009_2024.csv
##
## DATA SOURCES (NCCS efile v2_1)
##   - F9-P00-T00-HEADER
##   - SF-P01-T01-FRGN-ACTS-BY-REGION  (Part I)
##   - SF-P02-T01-FRGN-ORG-GRANTS      (Part II)
##   - SF-P03-T01-FRGN-INDIV-GRANTS    (Part III)
## ============================================================

## ----------------------------
## Packages
## ----------------------------
pkgs <- c(
  "dplyr", "readr", "tidyr", "stringr",
  "data.table", "jsonlite", "countrycode", "stringdist"
)

to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(data.table)
  library(jsonlite)
  library(countrycode)
  library(stringdist)
})

options(stringsAsFactors = FALSE)

## ----------------------------
## Configuration
## ----------------------------
base_url <- "https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_1"
years <- 2009:2024

out_dir  <- getwd()
out_file <- file.path(out_dir, "nccs_schedule_f_url_wide_2009_2024.csv")

## Cache directory for the city/state/country DB (downloaded once, reused)
cache_dir <- file.path(tempdir(), "schedf_geo_cache")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

## ----------------------------
## I/O Helpers
## ----------------------------

# Read an NCCS CSV (all columns as character, so we can parse safely ourselves).
read_nccs_safe <- function(prefix, year) {
  url <- sprintf("%s/%s-%d.CSV", base_url, prefix, year)
  df <- tryCatch(
    {
      readr::read_csv(
        url,
        col_types = readr::cols(.default = readr::col_character()),
        show_col_types = FALSE,
        progress = FALSE
      )
    },
    error = function(e) {
      message("READ FAIL (skipping): ", url)
      NULL
    }
  )
  if (is.null(df)) return(NULL)
  names(df) <- tolower(trimws(names(df)))
  df
}

# Convert numeric-ish strings to numbers; treat missing/blank as 0.
to_num0 <- function(x) {
  readr::parse_number(ifelse(is.na(x) | trimws(as.character(x)) == "", "0", as.character(x)))
}

## ----------------------------
## Header standardization (minimal but strict)
## ----------------------------
# NCCS header field names are usually stable, but this protects against small changes.
standardize_header <- function(h) {
  if (is.null(h)) return(NULL)
  if (!"url" %in% names(h)) stop("HEADER is missing `url` column.")
  
  # EIN
  if (!"org_ein" %in% names(h) && "ein" %in% names(h)) h <- dplyr::rename(h, org_ein = ein)
  
  # Tax year
  if (!"tax_year" %in% names(h)) {
    if ("taxyr" %in% names(h)) h <- dplyr::rename(h, tax_year = taxyr)
    if (!"tax_year" %in% names(h) && "taxperiod" %in% names(h)) h <- dplyr::rename(h, tax_year = taxperiod)
  }
  
  # Return type
  if (!"return_type" %in% names(h)) {
    cand <- grep("return_type|formtype|form_type|form", names(h), value = TRUE)
    if (length(cand) > 0) h <- dplyr::rename(h, return_type = !!cand[1])
  }
  
  # Timestamp/version (not strictly required for the foreign spending build, but useful metadata)
  if (!"return_time_stamp" %in% names(h)) {
    cand <- grep("return.*(time|timestamp|ts)", names(h), value = TRUE)
    if (length(cand) > 0) h <- dplyr::rename(h, return_time_stamp = !!cand[1])
  }
  if (!"version" %in% names(h)) {
    cand <- grep("^version$|efile_version|schema_version", names(h), value = TRUE)
    if (length(cand) > 0) h <- dplyr::rename(h, version = !!cand[1])
  }
  
  h
}

## ----------------------------
## Canonical IRS region buckets (codes for stable column names)
## ----------------------------
# We store a short region_code because columns must be valid and stable in wide format.
REGIONS <- tibble::tibble(
  region_code = c("ANT", "CAC", "EAP", "EUR", "MENA", "NA", "RUSN", "SAM", "SAS", "SSA", "UNK"),
  region_label = c(
    "Antarctica",
    "Central America and the Caribbean",
    "East Asia and the Pacific",
    "Europe (Including Iceland and Greenland)",
    "Middle East and North Africa",
    "North America",
    "Russia and Neighboring States",
    "South America",
    "South Asia",
    "Sub-Saharan Africa",
    "UNKNOWN"
  )
)

## ----------------------------
## Text normalization (for fuzzy matching)
## ----------------------------
# Vectorized text normalizer (works on character vectors)
normalize_text <- function(x) {
  x <- as.character(x)
  
  # Preserve NA positions
  na_idx <- is.na(x)
  x0 <- x
  x0[na_idx] <- ""
  
  # Accent-fold; if iconv fails for some entries, fall back to original
  x1 <- iconv(x0, from = "", to = "ASCII//TRANSLIT", sub = "")
  fail_idx <- is.na(x1) & !na_idx
  x1[fail_idx] <- x0[fail_idx]
  
  # Lowercase + whitespace normalization
  x1 <- tolower(x1)
  x1 <- stringr::str_replace_all(x1, "[\\r\\n\\t]+", " ")
  x1 <- stringr::str_replace_all(x1, "\\s+", " ")
  x1 <- trimws(x1)
  
  # Restore NAs
  x1[na_idx] <- NA_character_
  x1
}

## ----------------------------
## Region detection from the REGION FIELD (handles misspellings)
## ----------------------------
# This function:
#   1) Detects the IRS region bucket from the raw region string
#   2) Produces a "remainder" string that (hopefully) contains country/city tokens
detect_region <- function(region_field_raw) {
  s <- normalize_text(region_field_raw)
  if (is.na(s) || s == "") return(list(region_code = "UNK", region_label = "UNKNOWN", remainder = ""))
  
  # Common pattern: "Sub-Saharan Africa: Kenya, Uganda"
  # We treat left of ":" as "region-ish", right as "locations-ish".
  parts <- str_split_fixed(s, ":", n = 2)
  left  <- trimws(parts[1])
  right <- ifelse(ncol(parts) == 2, trimws(parts[2]), "")
  
  # Keyword map (fast path)
  kw <- list(
    ANT  = c("antarctica"),
    CAC  = c("central america", "caribbean", "cent america"),
    EAP  = c("east asia", "pacific", "asia pacific", "e asia"),
    EUR  = c("europe", "iceland", "greenland"),
    MENA = c("middle east", "north africa", "mena"),
    NAM   = c("north america"),
    RUSN = c("russia", "neighboring states", "neighbouring states", "former soviet"),
    SAM  = c("south america"),
    SAS  = c("south asia", "s asia"),
    SSA  = c("sub-saharan", "sub saharan", "subsaharan", "ssa")
  )
  
  find_by_kw <- function(txt) {
    for (rc in names(kw)) {
      for (p in kw[[rc]]) {
        if (str_detect(txt, fixed(p))) return(rc)
      }
    }
    NA_character_
  }
  
  rc <- find_by_kw(left)
  if (is.na(rc)) rc <- find_by_kw(s)
  
  # Fuzzy fallback against canonical region labels (catches misspellings/variants)
  if (is.na(rc)) {
    cand <- normalize_text(REGIONS$region_label)
    d_left <- stringdist::stringdist(left, cand, method = "jw", p = 0.1)
    j_left <- which.min(d_left)
    
    if (length(j_left) == 1 && is.finite(d_left[j_left]) && d_left[j_left] <= 0.18) {
      rc <- REGIONS$region_code[j_left]
    } else {
      d_all <- stringdist::stringdist(s, cand, method = "jw", p = 0.1)
      j_all <- which.min(d_all)
      if (length(j_all) == 1 && is.finite(d_all[j_all]) && d_all[j_all] <= 0.18) {
        rc <- REGIONS$region_code[j_all]
      } else {
        rc <- "UNK"
      }
    }
  }
  
  lab <- REGIONS$region_label[match(rc, REGIONS$region_code)]
  if (length(lab) != 1 || is.na(lab)) lab <- "UNKNOWN"
  
  # Remainder:
  #   - If colon exists and right side is non-empty, parse right side for locations
  #   - Else attempt to strip region phrases and parse what remains
  remainder <- ""
  if (!is.na(right) && right != "") {
    remainder <- right
  } else {
    remainder <- s
    remainder <- str_replace_all(remainder, "sub[- ]?saharan africa", " ")
    remainder <- str_replace_all(remainder, "central america( and)? the caribbean", " ")
    remainder <- str_replace_all(remainder, "east asia( and)? the pacific", " ")
    remainder <- str_replace_all(remainder, "europe( \\(including iceland and greenland\\))?", " ")
    remainder <- str_replace_all(remainder, "middle east( and)? north africa", " ")
    remainder <- str_replace_all(remainder, "north america", " ")
    remainder <- str_replace_all(remainder, "russia( and)? neighboring states", " ")
    remainder <- str_replace_all(remainder, "south america", " ")
    remainder <- str_replace_all(remainder, "south asia", " ")
    remainder <- str_replace_all(remainder, "antarctica", " ")
    remainder <- str_replace_all(remainder, "\\s+", " ")
    remainder <- trimws(remainder)
  }
  
  list(region_code = rc, region_label = lab, remainder = remainder)
}

## ----------------------------
## Country lookup (handles misspellings via fuzzy matching)
## ----------------------------
# Build a normalized dictionary of country names, plus common aliases.
build_country_lut <- function() {
  cl <- countrycode::codelist %>%
    transmute(country_name = as.character(country.name.en),
              iso3 = as.character(iso3c)) %>%
    filter(!is.na(country_name), country_name != "", !is.na(iso3), iso3 != "")
  
  aliases <- tibble::tibble(
    alias = c(
      "burma", "myanmar",
      "cote divoire", "cote d ivoire", "cote d'ivoire",
      "drc", "democratic republic of the congo", "congo kinshasa",
      "republic of the congo", "congo brazzaville",
      "uae", "united arab emirates",
      "uk", "u k", "u.k", "united kingdom",
      "russia", "russian federation",
      "iran", "iran islamic republic of",
      "laos", "lao peoples democratic republic",
      "vietnam", "viet nam",
      "syria", "syrian arab republic",
      "tanzania", "tanzania united republic of",
      "bolivia", "bolivia plurinational state of",
      "venezuela", "venezuela bolivarian republic of",
      "palestine", "west bank and gaza"
    ),
    canonical = c(
      "Myanmar", "Myanmar",
      "Côte d’Ivoire", "Côte d’Ivoire", "Côte d’Ivoire",
      "Congo, The Democratic Republic of the", "Congo, The Democratic Republic of the", "Congo, The Democratic Republic of the",
      "Congo", "Congo",
      "United Arab Emirates", "United Arab Emirates",
      "United Kingdom", "United Kingdom", "United Kingdom", "United Kingdom",
      "Russian Federation", "Russian Federation",
      "Iran, Islamic Republic of", "Iran, Islamic Republic of",
      "Lao People's Democratic Republic", "Lao People's Democratic Republic",
      "Viet Nam", "Viet Nam",
      "Syrian Arab Republic", "Syrian Arab Republic",
      "Tanzania, United Republic of", "Tanzania, United Republic of",
      "Bolivia, Plurinational State of", "Bolivia, Plurinational State of",
      "Venezuela, Bolivarian Republic of", "Venezuela, Bolivarian Republic of",
      "West Bank and Gaza", "West Bank and Gaza"
    )
  )
  
  df <- cl %>%
    mutate(key = normalize_text(country_name)) %>%
    distinct(key, .keep_all = TRUE)
  
  lut_name <- setNames(df$country_name, df$key)
  lut_iso3 <- setNames(df$iso3, df$key)
  
  # Add aliases into the lookup
  for (i in seq_len(nrow(aliases))) {
    a  <- normalize_text(aliases$alias[i])
    c0 <- aliases$canonical[i]
    lut_name[a] <- c0
    iso3c <- suppressWarnings(countrycode::countrycode(c0, "country.name", "iso3c", warn = FALSE))
    if (!is.na(iso3c) && iso3c != "") lut_iso3[a] <- iso3c
  }
  
  list(lut_name = lut_name, lut_iso3 = lut_iso3, country_keys = names(lut_name))
}

COUNTRY_LUT <- build_country_lut()

# Resolve a token to a country name using:
#   (1) exact normalized match
#   (2) fuzzy JW distance to any known country key
resolve_country_token <- function(tok_raw) {
  t0 <- normalize_text(tok_raw)
  if (is.na(t0) || t0 == "") return(NA_character_)
  
  # Ignore obvious "not a place" fragments
  if (t0 %in% c("n a", "na", "none", "various", "multiple", "international", "global")) return(NA_character_)
  if (str_detect(t0, "united states|u s a|usa|u s")) return(NA_character_)
  
  # Exact
  val <- COUNTRY_LUT$lut_name[t0]
  if (length(val) == 1 && !is.na(val) && val != "") return(unname(val))
  
  
  # Fuzzy match (small universe ~250, so manageable)
  keys <- COUNTRY_LUT$country_keys
  d <- stringdist::stringdist(t0, keys, method = "jw", p = 0.1)
  j <- which.min(d)
  if (length(j) == 1 && is.finite(d[j]) && d[j] <= 0.14) {
    return(COUNTRY_LUT$lut_name[[keys[j]]])
  }
  
  NA_character_
}

# Convert a country name to ISO3; return:
#   - "UNSPEC" for UNSPECIFIED bucket
#   - "ZZZ" for unknown/unmatched country string
country_to_iso3 <- function(country_name) {
  if (is.na(country_name) || country_name == "" || country_name == "UNSPECIFIED") return("UNSPEC")
  
  iso3 <- suppressWarnings(countrycode::countrycode(country_name, "country.name", "iso3c", warn = FALSE))
  if (!is.na(iso3) && iso3 != "") return(iso3)
  
  k <- normalize_text(country_name)
  iso3b <- COUNTRY_LUT$lut_iso3[k]
  if (length(iso3b) == 1 && !is.na(iso3b) && iso3b != "") return(unname(iso3b))
  
  
  "ZZZ"
}

## ----------------------------
## City/Province -> Country inference (dr5hn DB, with misspelling tolerance)
## ----------------------------
# We use the dr5hn countries-states-cities DB:
#   - Build maps for places that uniquely map to a single country
#   - Allow fuzzy matching to those unique places using stringdist JW
download_geodb_if_needed <- function(cache_dir) {
  dest <- file.path(cache_dir, "countries+states+cities.json.gz")
  if (file.exists(dest)) return(dest)
  
  url <- "https://raw.githubusercontent.com/dr5hn/countries-states-cities-database/master/json/countries%2Bstates%2Bcities.json.gz"
  ok <- tryCatch({
    utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE)
  
  if (!ok) return(NULL)
  dest
}

build_unique_place_maps <- function(cache_dir) {
  rds_path <- file.path(cache_dir, "place_unique_maps.rds")
  if (file.exists(rds_path)) return(readRDS(rds_path))
  
  gz_path <- download_geodb_if_needed(cache_dir)
  if (is.null(gz_path) || !file.exists(gz_path)) {
    message("Geo DB download unavailable; city/province inference disabled.")
    maps <- list(city_map = character(0), state_map = character(0),
                 city_dt = data.table(), state_dt = data.table())
    saveRDS(maps, rds_path)
    return(maps)
  }
  
  message("Loading geo DB (first run may be slow)...")
  geo <- jsonlite::fromJSON(gzfile(gz_path), simplifyVector = FALSE)
  
  rows_city  <- list()
  rows_state <- list()
  idx_c <- 1L
  idx_s <- 1L
  
  # Build tables: (place_name, country_name)
  for (ctry in geo) {
    ctry_name <- ctry$name
    if (is.null(ctry_name) || is.na(ctry_name) || ctry_name == "") next
    
    if (!is.null(ctry$states) && length(ctry$states) > 0) {
      for (st in ctry$states) {
        st_name <- st$name
        if (!is.null(st_name) && !is.na(st_name) && st_name != "") {
          rows_state[[idx_s]] <- list(state = st_name, country = ctry_name)
          idx_s <- idx_s + 1L
        }
        if (!is.null(st$cities) && length(st$cities) > 0) {
          for (city in st$cities) {
            city_name <- city$name
            if (is.null(city_name) || is.na(city_name) || city_name == "") next
            rows_city[[idx_c]] <- list(city = city_name, country = ctry_name)
            idx_c <- idx_c + 1L
          }
        }
      }
    }
  }
  
  city_dt  <- if (length(rows_city)  > 0) rbindlist(rows_city)  else data.table()
  state_dt <- if (length(rows_state) > 0) rbindlist(rows_state) else data.table()
  
  # Normalize and keep unique (place_norm, country)
  if (nrow(city_dt) > 0) {
    city_dt[, place_norm := normalize_text(city)]
    city_dt <- unique(city_dt[!is.na(place_norm) & place_norm != "", .(place_norm, country)])
  }
  if (nrow(state_dt) > 0) {
    state_dt[, place_norm := normalize_text(state)]
    state_dt <- unique(state_dt[!is.na(place_norm) & place_norm != "", .(place_norm, country)])
  }
  
  # Keep ONLY places that map uniquely to one country (to reduce false matches)
  if (nrow(city_dt) > 0) {
    city_cnt <- city_dt[, .(n = uniqueN(country), country1 = country[1]), by = place_norm]
    city_unique <- city_cnt[n == 1 & !is.na(country1) & country1 != ""]
    city_map <- setNames(as.character(city_unique$country1), as.character(city_unique$place_norm))
  } else city_map <- character(0)
  
  if (nrow(state_dt) > 0) {
    state_cnt <- state_dt[, .(n = uniqueN(country), country1 = country[1]), by = place_norm]
    state_unique <- state_cnt[n == 1 & !is.na(country1) & country1 != ""]
    state_map <- setNames(as.character(state_unique$country1), as.character(state_unique$place_norm))
  } else state_map <- character(0)
  
  # Add indexing columns to speed fuzzy lookup (first letter + length)
  if (nrow(city_dt) > 0) {
    city_dt[, first := substr(place_norm, 1, 1)]
    city_dt[, nchar := nchar(place_norm)]
  }
  if (nrow(state_dt) > 0) {
    state_dt[, first := substr(place_norm, 1, 1)]
    state_dt[, nchar := nchar(place_norm)]
  }
  
  maps <- list(city_map = city_map, state_map = state_map, city_dt = city_dt, state_dt = state_dt)
  saveRDS(maps, rds_path)
  maps
}

PLACE_MAPS <- build_unique_place_maps(cache_dir)

# Resolve a token as a (unique) city/state -> country:
#   - Exact normalized match first
#   - Then fuzzy JW match, filtered by first letter and similar length
resolve_country_from_place <- function(tok_raw) {
  t0 <- normalize_text(tok_raw)
  if (is.na(t0) || t0 == "") return(NA_character_)
  
  # Exact unique maps
  if (length(PLACE_MAPS$city_map) > 0) {
    v <- PLACE_MAPS$city_map[t0]
    if (length(v) == 1 && !is.na(v) && v != "") return(unname(v))
  }
  if (length(PLACE_MAPS$state_map) > 0) {
    v <- PLACE_MAPS$state_map[t0]
    if (length(v) == 1 && !is.na(v) && v != "") return(unname(v))
  }
  
  first <- substr(t0, 1, 1)
  len   <- nchar(t0)
  
  # Try states first (usually smaller candidate set)
  if (nrow(PLACE_MAPS$state_dt) > 0) {
    cand <- PLACE_MAPS$state_dt[first == first & abs(nchar - len) <= 2]
    if (nrow(cand) > 0) {
      d <- stringdist::stringdist(t0, cand$place_norm, method = "jw", p = 0.1)
      j <- which.min(d)
      if (length(j) == 1 && is.finite(d[j]) && d[j] <= 0.12) {
        chosen_norm <- cand$place_norm[j]
        ctry <- cand[place_norm == chosen_norm, unique(country)]
        if (length(ctry) == 1) return(ctry[1])
      }
    }
  }
  
  # Then cities
  if (nrow(PLACE_MAPS$city_dt) > 0) {
    cand <- PLACE_MAPS$city_dt[first == first & abs(nchar - len) <= 2]
    if (nrow(cand) > 0) {
      d <- stringdist::stringdist(t0, cand$place_norm, method = "jw", p = 0.1)
      j <- which.min(d)
      if (length(j) == 1 && is.finite(d[j]) && d[j] <= 0.12) {
        chosen_norm <- cand$place_norm[j]
        ctry <- cand[place_norm == chosen_norm, unique(country)]
        if (length(ctry) == 1) return(ctry[1])
      }
    }
  }
  
  NA_character_
}

## ----------------------------
## Tokenization: take “remainder” and split into candidate tokens
## ----------------------------
extract_tokens <- function(txt) {
  s <- normalize_text(txt)
  if (is.na(s) || s == "") return(character(0))
  
  # Standardize separators and joiners
  s <- str_replace_all(s, "\\b(and|or|&|including)\\b", ",")
  s <- str_replace_all(s, "[/\\|;]", ",")
  s <- str_replace_all(s, "\\(", ",")
  s <- str_replace_all(s, "\\)", ",")
  s <- str_replace_all(s, ":", ",")
  s <- str_replace_all(s, "\\s+", " ")
  s <- trimws(s)
  
  toks <- unlist(str_split(s, ","), use.names = FALSE)
  toks <- trimws(toks)
  toks <- toks[toks != ""]
  toks
}

## ----------------------------
## Parse a REGION FIELD into: (region_code, list_of_countries)
## ----------------------------
# This is where the core “country parsing from region text” happens.
parse_region_field_to_countries <- function(region_field_raw) {
  det <- detect_region(region_field_raw)
  rc  <- det$region_code
  rem <- det$remainder
  
  toks <- extract_tokens(rem)
  
  countries <- character(0)
  
  # For each token:
  #   - Try to interpret as a country name (with fuzzy matching)
  #   - If not, try city/province -> country (with fuzzy matching)
  if (length(toks) > 0) {
    for (t in toks) {
      c1 <- resolve_country_token(t)
      if (!is.na(c1) && c1 != "") {
        countries <- c(countries, c1)
        next
      }
      
      c2 <- resolve_country_from_place(t)
      if (!is.na(c2) && c2 != "") {
        countries <- c(countries, c2)
        next
      }
    }
  }
  
  countries <- unique(countries[!is.na(countries) & countries != ""])
  
  # If nothing parseable, allocate to UNSPECIFIED so region totals still reconcile.
  if (length(countries) == 0) countries <- "UNSPECIFIED"
  
  list(region_code = rc, countries = countries)
}

## ----------------------------
## Process each SF Part table to a LONG allocation dataset by URL/region/country
## ----------------------------
# Output is long so we can:
#   - do even-splitting across parsed countries
#   - aggregate cleanly by URL x region and URL x country
process_part <- function(df, part, header_urls) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (is.null(header_urls) || nrow(header_urls) == 0) return(NULL)
  
  if (!"url" %in% names(df)) stop("Schedule F table missing `url` column.")
  
  # Keep only URLs that exist in the header table for that year.
  df <- df %>%
    mutate(url = as.character(url)) %>%
    filter(url %in% header_urls$url)
  
  if (nrow(df) == 0) return(NULL)
  
  # Each part has:
  #   - one destination field (REGION field)
  #   - one or two amount fields
  if (part == "P1") {
    dest_col <- "sf_01_frgn_reg_region"
    amt_ops  <- "sf_01_frgn_reg_tot_exp"
    if (!dest_col %in% names(df) || !amt_ops %in% names(df)) {
      stop("Part I missing required columns: ", dest_col, " and/or ", amt_ops)
    }
    
    x <- df %>%
      transmute(
        url        = url,
        dest_raw   = .data[[dest_col]],
        own_ops    = to_num0(.data[[amt_ops]]),
        cash_grants    = 0,
        noncash_grants = 0
      )
    
  } else if (part == "P2") {
    dest_col <- "sf_02_frgn_org_grant_region"
    amt_cash <- "sf_02_frgn_org_grant_amt"
    amt_nc   <- "sf_02_frgn_org_grant_nc_amt"
    if (!dest_col %in% names(df) || !amt_cash %in% names(df) || !amt_nc %in% names(df)) {
      stop("Part II missing required columns: ", dest_col, ", ", amt_cash, ", ", amt_nc)
    }
    
    x <- df %>%
      transmute(
        url        = url,
        dest_raw   = .data[[dest_col]],
        own_ops    = 0,
        cash_grants    = to_num0(.data[[amt_cash]]),
        noncash_grants = to_num0(.data[[amt_nc]])
      )
    
  } else if (part == "P3") {
    dest_col <- "sf_03_frgn_indiv_grant_region"
    amt_cash <- "sf_03_frgn_indiv_grant_amt"
    amt_nc   <- "sf_03_frgn_indiv_grant_nc_amt"
    if (!dest_col %in% names(df) || !amt_cash %in% names(df) || !amt_nc %in% names(df)) {
      stop("Part III missing required columns: ", dest_col, ", ", amt_cash, ", ", amt_nc)
    }
    
    x <- df %>%
      transmute(
        url        = url,
        dest_raw   = .data[[dest_col]],
        own_ops    = 0,
        cash_grants    = to_num0(.data[[amt_cash]]),
        noncash_grants = to_num0(.data[[amt_nc]])
      )
    
  } else {
    stop("Unknown part: ", part)
  }
  
  # Attach header metadata (URL is the join key)
  x <- x %>%
    left_join(
      header_urls %>% select(url, org_ein, tax_year, return_time_stamp, version),
      by = "url"
    ) %>%
    mutate(
      org_ein  = as.character(org_ein),
      tax_year = as.integer(tax_year)
    )
  
  # Parse region + list of countries from dest_raw
  n <- nrow(x)
  region_codes  <- character(n)
  country_lists <- vector("list", n)
  
  for (i in seq_len(n)) {
    pr <- parse_region_field_to_countries(x$dest_raw[i])
    region_codes[i]  <- pr$region_code
    country_lists[[i]] <- pr$countries
  }
  
  x$region_code <- region_codes
  x$countries   <- country_lists
  
  # Expand each original row into one row per parsed country,
  # splitting amounts evenly across the countries in that single cell/row.
  lens <- vapply(x$countries, length, integer(1))
  x_exp <- x[rep(seq_len(n), times = lens), , drop = FALSE]
  x_exp$country <- unlist(x$countries, use.names = FALSE)
  
  w <- 1 / rep(lens, times = lens)
  x_exp$own_ops        <- x_exp$own_ops * w
  x_exp$cash_grants    <- x_exp$cash_grants * w
  x_exp$noncash_grants <- x_exp$noncash_grants * w
  
  # Total spending for that (expanded) piece
  x_exp$total_spending <- x_exp$own_ops + x_exp$cash_grants + x_exp$noncash_grants
  
  x_exp <- x_exp %>%
    mutate(
      part = part,
      country_source = if_else(country == "UNSPECIFIED", "unspecified", "parsed"),
      country_iso3   = vapply(country, country_to_iso3, character(1))
    ) %>%
    select(
      url, org_ein, tax_year, return_time_stamp, version,
      part, region_code,
      country, country_iso3, country_source,
      total_spending, own_ops, cash_grants, noncash_grants
    )
  
  x_exp
}

## ----------------------------
## Wide pivot helpers
## ----------------------------
# Ensure safe suffixes for column names (upper + alnum + underscore).
safe_col <- function(x) {
  s <- toupper(as.character(x))
  s <- str_replace_all(s, "[^A-Z0-9]+", "_")
  s <- str_replace_all(s, "_+", "_")
  s <- str_replace_all(s, "^_|_$", "")
  ifelse(s == "" | is.na(s), "UNK", s)
}

# Build wide block for regions (URL x region_code) for each component.
make_region_wide <- function(df_long) {
  if (is.null(df_long) || nrow(df_long) == 0) return(NULL)
  
  reg <- df_long %>%
    group_by(url, org_ein, tax_year, return_time_stamp, version, region_code) %>%
    summarise(
      total_spending  = sum(total_spending,  na.rm = TRUE),
      own_ops         = sum(own_ops,         na.rm = TRUE),
      cash_grants     = sum(cash_grants,     na.rm = TRUE),
      noncash_grants  = sum(noncash_grants,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(region_code = safe_col(region_code))
  
  # Pivot one component at a time so the names are consistent.
  reg_total <- reg %>%
    select(url, region_code, total_spending) %>%
    pivot_wider(names_from = region_code, values_from = total_spending,
                names_prefix = "tot_region__", values_fill = 0)
  
  reg_ops <- reg %>%
    select(url, region_code, own_ops) %>%
    pivot_wider(names_from = region_code, values_from = own_ops,
                names_prefix = "ops_region__", values_fill = 0)
  
  reg_cash <- reg %>%
    select(url, region_code, cash_grants) %>%
    pivot_wider(names_from = region_code, values_from = cash_grants,
                names_prefix = "cash_region__", values_fill = 0)
  
  reg_nc <- reg %>%
    select(url, region_code, noncash_grants) %>%
    pivot_wider(names_from = region_code, values_from = noncash_grants,
                names_prefix = "noncash_region__", values_fill = 0)
  
  meta <- reg %>%
    distinct(url, org_ein, tax_year, return_time_stamp, version)
  
  meta %>%
    left_join(reg_total, by = "url") %>%
    left_join(reg_ops,   by = "url") %>%
    left_join(reg_cash,  by = "url") %>%
    left_join(reg_nc,    by = "url")
}

# Build wide block for countries using ISO3 codes as stable keys
# (URL x iso3) for each component.
make_country_wide <- function(df_long) {
  if (is.null(df_long) || nrow(df_long) == 0) return(NULL)
  
  cty <- df_long %>%
    group_by(url, country_iso3) %>%
    summarise(
      total_spending  = sum(total_spending,  na.rm = TRUE),
      own_ops         = sum(own_ops,         na.rm = TRUE),
      cash_grants     = sum(cash_grants,     na.rm = TRUE),
      noncash_grants  = sum(noncash_grants,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(country_iso3 = safe_col(country_iso3))
  
  cty_total <- cty %>%
    select(url, country_iso3, total_spending) %>%
    pivot_wider(names_from = country_iso3, values_from = total_spending,
                names_prefix = "tot_country__", values_fill = 0)
  
  cty_ops <- cty %>%
    select(url, country_iso3, own_ops) %>%
    pivot_wider(names_from = country_iso3, values_from = own_ops,
                names_prefix = "ops_country__", values_fill = 0)
  
  cty_cash <- cty %>%
    select(url, country_iso3, cash_grants) %>%
    pivot_wider(names_from = country_iso3, values_from = cash_grants,
                names_prefix = "cash_country__", values_fill = 0)
  
  cty_nc <- cty %>%
    select(url, country_iso3, noncash_grants) %>%
    pivot_wider(names_from = country_iso3, values_from = noncash_grants,
                names_prefix = "noncash_country__", values_fill = 0)
  
  cty_total %>%
    left_join(cty_ops,  by = "url") %>%
    left_join(cty_cash, by = "url") %>%
    left_join(cty_nc,   by = "url")
}

# Counts per URL:
#   - n_regions_reported: distinct region_code excluding UNK
#   - n_countries_reported: distinct ISO3 excluding UNSPEC and ZZZ
#   - n_locations_reported: prefer country count if any countries were parsed
make_counts <- function(df_long) {
  if (is.null(df_long) || nrow(df_long) == 0) return(NULL)
  
  df_long %>%
    group_by(url) %>%
    summarise(
      n_regions_reported = n_distinct(region_code[!is.na(region_code) & region_code != "" & region_code != "UNK"]),
      n_countries_reported = n_distinct(country_iso3[!is.na(country_iso3) & !(country_iso3 %in% c("UNSPEC", "ZZZ"))]),
      n_locations_reported = if_else(n_countries_reported > 0, n_countries_reported, n_regions_reported),
      .groups = "drop"
    )
}

## ----------------------------
## Process a single year
## ----------------------------
process_year <- function(year) {
  message("=== YEAR ", year, " ===")
  
  # 1) Read header and keep only Form 990 filings for that year.
  hdr <- read_nccs_safe("F9-P00-T00-HEADER", year)
  hdr <- standardize_header(hdr)
  if (is.null(hdr) || nrow(hdr) == 0) return(NULL)
  
  if (!"return_type" %in% names(hdr)) stop("HEADER missing return_type after standardization.")
  if (!"org_ein" %in% names(hdr)) stop("HEADER missing org_ein after standardization.")
  if (!"tax_year" %in% names(hdr)) stop("HEADER missing tax_year after standardization.")
  
  header_urls <- hdr %>%
    filter(return_type == "990") %>%
    transmute(
      url = as.character(url),
      org_ein = as.character(org_ein),
      tax_year = as.integer(tax_year),
      return_time_stamp = as.character(return_time_stamp),
      version = as.character(version)
    ) %>%
    filter(!is.na(url), url != "")
  
  if (nrow(header_urls) == 0) return(NULL)
  
  # 2) Read Schedule F part tables (each is a relational table keyed by URL)
  sf01 <- read_nccs_safe("SF-P01-T01-FRGN-ACTS-BY-REGION", year)
  sf02 <- read_nccs_safe("SF-P02-T01-FRGN-ORG-GRANTS", year)
  sf03 <- read_nccs_safe("SF-P03-T01-FRGN-INDIV-GRANTS", year)
  
  # 3) Convert each part table into long allocations by URL/region/country.
  p1 <- process_part(sf01, "P1", header_urls)
  p2 <- process_part(sf02, "P2", header_urls)
  p3 <- process_part(sf03, "P3", header_urls)
  
  long_all <- bind_rows(p1, p2, p3)
  if (nrow(long_all) == 0) return(NULL)
  
  # 4) Compute total foreign spending per URL (THIS supports your Edit #1).
  url_totals <- long_all %>%
    group_by(url) %>%
    summarise(url_total_foreign_spending = sum(total_spending, na.rm = TRUE), .groups = "drop")
  
  # 5) Build wide blocks
  region_wide  <- make_region_wide(long_all)
  country_wide <- make_country_wide(long_all)
  counts       <- make_counts(long_all)
  
  # 6) Assemble one wide row per URL (join by URL)
  out <- region_wide %>%
    left_join(country_wide, by = "url") %>%
    left_join(counts,       by = "url") %>%
    left_join(url_totals,   by = "url") %>%
    mutate(file_year = year)
  
  # 7) Ensure all numeric spend fields are non-NA (set NA -> 0)
  num_cols <- names(out)[vapply(out, is.numeric, logical(1))]
  if (length(num_cols) > 0) {
    out[num_cols] <- lapply(out[num_cols], function(z) ifelse(is.na(z), 0, z))
  }
  
  # 8) EDIT #1: Drop rows where total foreign spending is zero.
  #    This removes URLs that exist in HEADER but effectively have no Schedule F spending.
  out <- out %>%
    filter(url_total_foreign_spending > 0)
  
  out
}

## ----------------------------
## Main loop across years
## ----------------------------
all_years <- list()

for (yr in years) {
  tmp <- process_year(yr)
  if (!is.null(tmp) && nrow(tmp) > 0) {
    all_years[[as.character(yr)]] <- tmp
  }
  gc()
}

final_wide <- bind_rows(all_years)

## ----------------------------
## Final safety checks and final drop
## ----------------------------

# Check strict uniqueness: one row per URL
dups <- final_wide %>%
  count(url) %>%
  filter(n > 1)

if (nrow(dups) > 0) {
  stop(
    "ERROR: URL is not unique in final output. Example duplicated URLs: ",
    paste(head(dups$url, 10), collapse = ", ")
  )
}

# Redundant guard: ensure we did drop all-zero total spend URLs
# (should already have happened inside process_year)
if ("url_total_foreign_spending" %in% names(final_wide)) {
  final_wide <- final_wide %>% filter(url_total_foreign_spending > 0)
}

## ----------------------------
## Write output
## ----------------------------
readr::write_csv(final_wide, out_file)
message("DONE. Wrote: ", out_file)



setwd("/Users/kv7379/Library/CloudStorage/OneDrive-PrincetonUniversity/Foreign 990/Datasets/")

## ============================================================
## FIGURES SCRIPT (FINAL_WIDE READS FROM CSV + 7 BINS)
##
## Reads:
##   - final_wide.csv from:
##     "/Users/kv7379/Library/CloudStorage/OneDrive-PrincetonUniversity/Foreign 990/Datasets/"
##
## Uses:
##   - 7-bin maps with descriptive legends
##   - Diverging 7-bin %change map (3 dec + 1 zero band + 3 inc)
##   - Never plots 2024 (through 2023 only)
##   - USA + Antarctica never mapped (forced to 0 + removed geometry)
## ============================================================


## ----------------------------
## 0) Packages
## ----------------------------
pkgs <- c("dplyr","readr","tidyr","stringr","ggplot2","sf","rnaturalearth","rnaturalearthdata")
to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
})

library(MASS)
library(dplyr)   

## ----------------------------
## 1) Global settings
## ----------------------------
max_plot_year <- 2023L              # never plot 2024
zero_band <- 5                      # +/- percent band for "No / little change"
FORCE_ZERO_ISO3 <- c("USA","ATA")   # never mapped; forced to 0
N_BINS <- 7L                        # requested: 7 bins

out_dir <- getwd()

data_dir <- "/Users/kv7379/Library/CloudStorage/OneDrive-PrincetonUniversity/Foreign 990/Datasets/"

# ---- final_wide file (CSV) ----
final_wide_path <- file.path(data_dir, "nccs_schedule_f_url_wide_2009_2024.csv")
if (!file.exists(final_wide_path)) {
  stop("Could not find final_wide at: ", final_wide_path,
       "\nIf the filename differs, edit `final_wide_path` in this script.")
}

final_wide <- readr::read_csv(
  final_wide_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)


## ----------------------------
## 2) Helpers
## ----------------------------
clean_ein <- function(x) gsub("[^0-9]", "", as.character(x))

label_millions <- function(x) paste0(formatC(x, format = "f", digits = 1), "M")

fmt_millions <- function(x) paste0("$", formatC(x / 1e6, format = "f", digits = 2), "M")
fmt_percent  <- function(x) paste0(formatC(x, format = "f", digits = 1), "%")

# ntile-based bins (N_BINS) with descriptive min–max labels per bin.
# Bins computed only on eligible values; ineligible values -> NA (grey on map).
bin_ntile_range_labels <- function(x, fmt_fun, eligible, n_bins = 7L) {
  out <- rep(NA_character_, length(x))
  if (is.null(eligible)) eligible <- !is.na(x)
  
  idx <- which(eligible & !is.na(x))
  if (length(idx) == 0) return(factor(out))
  
  # If too few observations, fall back to a single range label
  if (length(idx) < n_bins) {
    rng <- range(x[idx], na.rm = TRUE)
    out[idx] <- paste0(fmt_fun(rng[1]), " – ", fmt_fun(rng[2]))
    return(factor(out, ordered = TRUE))
  }
  
  tile <- rep(NA_integer_, length(x))
  tile[idx] <- dplyr::ntile(x[idx], n_bins)
  
  labs <- character(n_bins)
  for (k in seq_len(n_bins)) {
    vals <- x[idx][tile[idx] == k]
    labs[k] <- if (length(vals) == 0) NA_character_ else
      paste0(fmt_fun(min(vals, na.rm = TRUE)), " – ", fmt_fun(max(vals, na.rm = TRUE)))
  }
  
  out[idx] <- labs[tile[idx]]
  factor(out, levels = unique(labs[!is.na(labs)]), ordered = TRUE)
}

# Region labels
REGIONS <- tibble::tibble(
  region_code = c("ANT", "CAC", "EAP", "EUR", "MENA", "NA", "RUSN", "SAM", "SAS", "SSA", "UNK"),
  region_label = c(
    "Antarctica",
    "Central America & Caribbean",
    "East Asia & Pacific",
    "Europe (incl. Iceland/Greenland)",
    "Middle East & North Africa",
    "North America",
    "Russia & Neighboring States",
    "South America",
    "South Asia",
    "Sub-Saharan Africa",
    "Unknown"
  )
)


## ----------------------------
## 3) Anti-LGBTQ indicator (YOUR LOGIC)
## ----------------------------
anti_extended_path <- file.path(data_dir, "anti_lgbtq_schedulei.csv")
anti_known_path    <- file.path(data_dir, "Anti-LGBT EINs.csv")

if (!file.exists(anti_extended_path)) stop("Missing: ", anti_extended_path)
if (!file.exists(anti_known_path)) stop("Missing: ", anti_known_path)

anti_extended <- readr::read_csv(
  anti_extended_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>%
  rename(org_ein = recipient_ein) %>%
  mutate(org_ein = clean_ein(org_ein))

anti_known <- readr::read_csv(
  anti_known_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>%
  rename(org_ein = ein) %>%
  mutate(
    org_ein = clean_ein(org_ein),
    anti_known_org = 1L
  ) %>%
  distinct(org_ein, .keep_all = TRUE)

anti_flags <- anti_extended %>%
  dplyr::select(dplyr::any_of(c("org_ein", "anti_least_likely_candidate"))) %>%
  dplyr::left_join(
    anti_known %>% dplyr::select(org_ein, anti_known_org),
    by = "org_ein"
  ) %>%
  dplyr::mutate(
    anti_least_likely_candidate = suppressWarnings(readr::parse_number(as.character(anti_least_likely_candidate))),
    anti_least_likely_candidate = dplyr::if_else(is.na(anti_least_likely_candidate), 0L, as.integer(anti_least_likely_candidate)),
    anti_known_org             = dplyr::if_else(is.na(anti_known_org), 0L, as.integer(anti_known_org)),
    anti_least_likely_candidate = dplyr::if_else(anti_known_org == 1L, 1L, anti_least_likely_candidate),
    anti_any = as.integer((anti_least_likely_candidate == 1L) | (anti_known_org == 1L))
  ) %>%
  dplyr::select(org_ein, anti_any, anti_known_org, anti_least_likely_candidate) %>%
  dplyr::distinct(org_ein, .keep_all = TRUE)

schedf <- final_wide %>%
  mutate(
    org_ein = clean_ein(org_ein),
    tax_year = suppressWarnings(as.integer(tax_year))
  ) %>%
  left_join(anti_flags, by = "org_ein") %>%
  mutate(anti_any = if_else(is.na(anti_any), 0L, as.integer(anti_any)))


## ----------------------------
## 4) CPI adjustment to 2024 dollars
## ----------------------------
cpi_u_annual <- tibble::tribble(
  ~tax_year, ~cpi_u_avg,
  2008L, 215.303, 2009L, 214.537, 2010L, 218.056, 2011L, 224.939,
  2012L, 229.594, 2013L, 232.957, 2014L, 236.736, 2015L, 237.017,
  2016L, 240.007, 2017L, 245.120, 2018L, 251.107, 2019L, 255.657,
  2020L, 258.811, 2021L, 270.970, 2022L, 292.655, 2023L, 304.702,
  2024L, 313.689
)
cpi_target <- cpi_u_annual %>% filter(tax_year == 2024L) %>% pull(cpi_u_avg)
if (length(cpi_target) != 1) stop("CPI target year 2024 not found.")

spend_cols <- grep("^(tot|ops|cash|noncash)_(region|country)__", names(schedf), value = TRUE)
if (length(spend_cols) == 0) stop("No Schedule F spend columns found in `schedf`.")

schedf_adj <- schedf %>%
  mutate(across(all_of(spend_cols), ~ as.numeric(replace_na(readr::parse_number(as.character(.x)), 0)))) %>%
  left_join(cpi_u_annual, by = "tax_year") %>%
  mutate(
    cpi_u_avg = as.numeric(cpi_u_avg),
    cpi_mult = if_else(!is.na(cpi_u_avg) & cpi_u_avg > 0, cpi_target / cpi_u_avg, NA_real_)
  ) %>%
  mutate(across(all_of(spend_cols), ~ if_else(is.na(cpi_mult), .x, .x * cpi_mult))) %>%
  select(-cpi_u_avg, -cpi_mult)

schedf_plot <- schedf_adj %>%
  filter(!is.na(tax_year), tax_year <= max_plot_year)


## ----------------------------
## 5) Map geometry: remove USA + Antarctica
## ----------------------------
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(iso3 = iso_a3) %>%
  filter(!(iso3 %in% FORCE_ZERO_ISO3))


## ============================================================
## FIGURE 1: Trend lines — anti spending by region (millions)
## ============================================================
df_reg_yr_anti <- schedf_plot %>%
  filter(anti_any == 1L) %>%
  select(tax_year, starts_with("tot_region__")) %>%
  pivot_longer(cols = starts_with("tot_region__"),
               names_to = "region_code",
               values_to = "anti_spend_2024") %>%
  mutate(region_code = toupper(str_replace(region_code, "^tot_region__", ""))) %>%
  group_by(tax_year, region_code) %>%
  summarise(anti_spend_2024 = sum(anti_spend_2024, na.rm = TRUE), .groups = "drop") %>%
  mutate(anti_spend_mil = anti_spend_2024 / 1e6) %>%
  left_join(REGIONS, by = "region_code") %>%
  mutate(region_label = if_else(is.na(region_label), region_code, region_label))

p1 <- ggplot(df_reg_yr_anti, aes(x = tax_year, y = anti_spend_mil, color = region_label)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = label_millions) +
  labs(
    title = "Total Anti-LGBTQ Foreign Spending by Region (2024 dollars; through 2023)",
    x = "Tax year",
    y = "Spending (Millions of 2024 $)",
    color = "Region"
  )

ggsave(file.path(out_dir, "fig1_trend_anti_spending_by_region_millions.png"),
       p1, width = 11, height = 6, dpi = 300)


## ============================================================
## FIGURE 1B: Trend lines — anti share of total by region (%)
## ============================================================
df_reg_yr_all <- schedf_plot %>%
  select(tax_year, starts_with("tot_region__")) %>%
  pivot_longer(cols = starts_with("tot_region__"),
               names_to = "region_code",
               values_to = "total_spend_2024") %>%
  mutate(region_code = toupper(str_replace(region_code, "^tot_region__", ""))) %>%
  group_by(tax_year, region_code) %>%
  summarise(total_spend_2024 = sum(total_spend_2024, na.rm = TRUE), .groups = "drop")

df_reg_share_yr <- df_reg_yr_all %>%
  left_join(df_reg_yr_anti %>% select(tax_year, region_code, anti_spend_2024),
            by = c("tax_year","region_code")) %>%
  mutate(
    anti_spend_2024 = replace_na(anti_spend_2024, 0),
    anti_share_pct = if_else(total_spend_2024 > 0, 100 * anti_spend_2024 / total_spend_2024, NA_real_)
  ) %>%
  left_join(REGIONS, by = "region_code") %>%
  mutate(region_label = if_else(is.na(region_label), region_code, region_label))

p1b <- ggplot(df_reg_share_yr, aes(x = tax_year, y = anti_share_pct, color = region_label)) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Anti-LGBTQ Spending as % of Total Foreign Spending by Region (through 2023)",
    x = "Tax year",
    y = "Anti-LGBTQ share of region spending (%)",
    color = "Region"
  )

ggsave(file.path(out_dir, "fig1b_trend_anti_share_by_region.png"),
       p1b, width = 11, height = 6, dpi = 300)


## ============================================================
## FIGURE 2: Map — YEARLY AVERAGE anti spending by country (7 bins)
##   - bins computed among positive values only
## ============================================================
years_in_period <- length(sort(unique(schedf_plot$tax_year)))

df_cty_anti_total <- schedf_plot %>%
  filter(anti_any == 1L) %>%
  select(starts_with("tot_country__")) %>%
  pivot_longer(cols = starts_with("tot_country__"),
               names_to = "iso3",
               values_to = "anti_spend_2024") %>%
  mutate(iso3 = toupper(str_replace(iso3, "^tot_country__", ""))) %>%
  group_by(iso3) %>%
  summarise(anti_total_2024 = sum(anti_spend_2024, na.rm = TRUE), .groups = "drop") %>%
  filter(!(iso3 %in% c("UNSPEC","ZZZ"))) %>%
  mutate(
    anti_total_2024 = if_else(iso3 %in% FORCE_ZERO_ISO3, 0, anti_total_2024),
    anti_avg_yr_2024 = anti_total_2024 / years_in_period,
    anti_avg_bin = bin_ntile_range_labels(
      x = anti_avg_yr_2024,
      fmt_fun = fmt_millions,
      eligible = anti_avg_yr_2024 > 0,
      n_bins = N_BINS
    )
  )

world_avg_anti <- world %>% left_join(df_cty_anti_total, by = "iso3")

p2 <- ggplot(world_avg_anti) +
  geom_sf(aes(fill = anti_avg_bin), linewidth = 0.1) +
  labs(
    title = "Yearly Average Anti-LGBTQ Foreign Spending by Country (2024 dollars; through 2023)",
    fill  = "Avg annual anti spending\n(2024 $, per year)"
  )

ggsave(file.path(out_dir, "fig2_map_anti_spending_yearly_average_7bins.png"),
       p2, width = 12, height = 7, dpi = 300)


## ============================================================
## FIGURE 3: Map — AVERAGE anti share of total by country (7 bins)
## ============================================================
df_cty_total_all <- schedf_plot %>%
  select(starts_with("tot_country__")) %>%
  pivot_longer(cols = starts_with("tot_country__"),
               names_to = "iso3",
               values_to = "total_spend_2024") %>%
  mutate(iso3 = toupper(str_replace(iso3, "^tot_country__", ""))) %>%
  group_by(iso3) %>%
  summarise(total_total_2024 = sum(total_spend_2024, na.rm = TRUE), .groups = "drop") %>%
  filter(!(iso3 %in% c("UNSPEC","ZZZ"))) %>%
  mutate(total_total_2024 = if_else(iso3 %in% FORCE_ZERO_ISO3, 0, total_total_2024))

df_cty_share <- df_cty_total_all %>%
  left_join(df_cty_anti_total %>% select(iso3, anti_total_2024), by = "iso3") %>%
  mutate(
    anti_total_2024 = replace_na(anti_total_2024, 0),
    anti_share_pct = if_else(total_total_2024 > 0, 100 * anti_total_2024 / total_total_2024, NA_real_),
    anti_share_pct = if_else(iso3 %in% FORCE_ZERO_ISO3, 0, anti_share_pct),
    anti_share_bin = bin_ntile_range_labels(
      x = anti_share_pct,
      fmt_fun = fmt_percent,
      eligible = anti_share_pct > 0,
      n_bins = N_BINS
    )
  )

world_share <- world %>% left_join(df_cty_share, by = "iso3")

p3 <- ggplot(world_share) +
  geom_sf(aes(fill = anti_share_bin), linewidth = 0.1) +
  labs(
    title = "Average Anti-LGBTQ Share of Total Foreign Spending by Country (through 2023)",
    fill  = "Avg anti share\n(of total spending)"
  )

ggsave(file.path(out_dir, "fig3_map_anti_share_average_7bins.png"),
       p3, width = 12, height = 7, dpi = 300)

## ============================================================
## FIGURE 4 (EVENT TIME) — ABSOLUTE CHANGE MAP (STRICT 3-YEAR SUMS)
##
## Computes, by country (ISO3):
##   pre_sum  = SUM of anti-LGBTQ spending at event_time {-3,-2,-1}
##   post_sum = SUM of anti-LGBTQ spending at event_time {+3,+4,+5}
##   abs_change = post_sum - pre_sum
##
## Then bins abs_change into 5 diverging categories (2 dec, 1 near-zero, 2 inc),
## with DATA-DRIVEN legend labels (actual dollar ranges per bin).
##
## Requires already in your environment (from your main figures script):
##   - data_dir, out_dir
##   - max_plot_year, FORCE_ZERO_ISO3
##   - schedf_adj  (URL-level, CPI-adjusted to 2024 $, contains anti_any and tot_country__*)
##   - world       (sf geometry, already excludes USA/ATA)
##
## Creates:
##   - fig4_eventtime_country_abs_change_SUM_pre(-3:-1)_post(3:5).csv
##   - fig4_map_abs_change_anti_funding_eventtime_5bins_diverging.png
## ============================================================

## ----------------------------
## 4.1) Read analytic_data_122025 and derive event_time; join by URL
## ----------------------------
analytic_path <- file.path(data_dir, "analytic_data_122025.csv")
if (!file.exists(analytic_path)) stop("Missing: ", analytic_path)

analytic_data <- readr::read_csv(
  analytic_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

# standardize URL column name to `url`
if ("URL" %in% names(analytic_data) && !"url" %in% names(analytic_data)) {
  analytic_data <- analytic_data %>% rename(url = URL)
}
if (!"url" %in% names(analytic_data)) stop("analytic_data_122025.csv does not have `URL` or `url`.")

# Find existing event time, or compute from tax_year - treat_year
event_time_candidates <- c("event_time","eventtime","rel_year","relative_year","etime","event_t")
treat_year_candidates <- c("treat_year","treatment_year","me_year","marriage_equality_year","policy_year")

evt_col <- intersect(event_time_candidates, names(analytic_data))
trt_col <- intersect(treat_year_candidates, names(analytic_data))

analytic_evt <- analytic_data %>%
  mutate(tax_year = suppressWarnings(as.integer(tax_year)))

if (length(evt_col) >= 1) {
  evt_col <- evt_col[1]
  analytic_evt <- analytic_evt %>%
    mutate(event_time = suppressWarnings(as.integer(readr::parse_number(.data[[evt_col]]))))
} else if (length(trt_col) >= 1) {
  trt_col <- trt_col[1]
  analytic_evt <- analytic_evt %>%
    mutate(
      treat_year = suppressWarnings(as.integer(readr::parse_number(.data[[trt_col]]))),
      event_time = if_else(!is.na(tax_year) & !is.na(treat_year), tax_year - treat_year, NA_integer_)
    )
} else {
  stop(
    "Could not find an event-time column (e.g., event_time) OR a treat-year column (e.g., treat_year)\n",
    "in analytic_data_122025.csv."
  )
}

analytic_evt <- analytic_evt %>%
  select(url, event_time) %>%
  distinct(url, .keep_all = TRUE)

# Attach event_time onto CPI-adjusted URL-level Schedule F spending
schedf_evt <- schedf_adj %>%
  filter(!is.na(tax_year), tax_year <= max_plot_year) %>%
  left_join(analytic_evt, by = "url")

## ----------------------------
## 4.2) Long anti spending by country WITH event_time
## ----------------------------
df_cty_evt_anti <- schedf_evt %>%
  filter(anti_any == 1L) %>%
  select(url, event_time, starts_with("tot_country__")) %>%
  pivot_longer(
    cols = starts_with("tot_country__"),
    names_to = "iso3",
    values_to = "spend_2024"
  ) %>%
  mutate(
    iso3 = toupper(str_replace(iso3, "^tot_country__", "")),
    spend_2024 = as.numeric(replace_na(readr::parse_number(as.character(spend_2024)), 0))
  ) %>%
  filter(!(iso3 %in% c("UNSPEC","ZZZ"))) %>%
  mutate(
    # force USA and Antarctica to 0 (also not mapped)
    spend_2024 = if_else(iso3 %in% FORCE_ZERO_ISO3, 0, spend_2024)
  )

## ----------------------------
## 4.3) STRICT 3-year SUMS and absolute change
## ----------------------------
df_cty_prepost_sum <- df_cty_evt_anti %>%
  group_by(iso3) %>%
  summarise(
    pre_sum  = sum(spend_2024[event_time %in% c(-3L, -2L, -1L)], na.rm = TRUE),
    post_sum = sum(spend_2024[event_time %in% c( 3L,  4L,  5L)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(abs_change = post_sum - pre_sum)

# Quick internal check printed to console
cat("\n[FIG4 ABS CHANGE] Sanity check: abs_change == post_sum - pre_sum\n")
print(
  df_cty_prepost_sum %>%
    mutate(check = abs_change - (post_sum - pre_sum)) %>%
    summarise(
      n = n(),
      max_abs_check = max(abs(check), na.rm = TRUE),
      n_nonzero_pre = sum(pre_sum > 0, na.rm = TRUE),
      n_nonzero_post = sum(post_sum > 0, na.rm = TRUE)
    )
)

## ----------------------------
## 4.4) Diverging 5 bins with DATA-DRIVEN legend labels
## ----------------------------
zero_band_dollars <- 10000  # edit if you want a different near-zero band

fmt_millions <- function(x) paste0("$", formatC(x / 1e6, format = "f", digits = 2), "M")

df_abs <- df_cty_prepost_sum %>%
  mutate(
    change_group = case_when(
      is.na(abs_change) ~ NA_character_,
      abs(abs_change) <= zero_band_dollars ~ "ZERO",
      abs_change < -zero_band_dollars ~ "NEG",
      abs_change >  zero_band_dollars ~ "POS",
      TRUE ~ "ZERO"
    )
  )

# NEG: split into 2 bins, label using actual min/max in each bin
neg_labeled <- df_abs %>%
  filter(change_group == "NEG") %>%
  mutate(bin2 = dplyr::ntile(abs_change, 2)) %>%   # more negative tends to bin2==1
  group_by(bin2) %>%
  mutate(
    bin_min = min(abs_change, na.rm = TRUE),
    bin_max = max(abs_change, na.rm = TRUE),
    abs_change_bin = paste0("Decrease: ", fmt_millions(bin_min), " to ", fmt_millions(bin_max))
  ) %>%
  ungroup() %>%
  select(iso3, abs_change_bin)

# POS: split into 2 bins, label using actual min/max in each bin
pos_labeled <- df_abs %>%
  filter(change_group == "POS") %>%
  mutate(bin2 = dplyr::ntile(abs_change, 2)) %>%
  group_by(bin2) %>%
  mutate(
    bin_min = min(abs_change, na.rm = TRUE),
    bin_max = max(abs_change, na.rm = TRUE),
    abs_change_bin = paste0("Increase: ", fmt_millions(bin_min), " to ", fmt_millions(bin_max))
  ) %>%
  ungroup() %>%
  select(iso3, abs_change_bin)

lab_zero <- paste0("No / little change (±", fmt_millions(zero_band_dollars), ")")

df_abs_binned <- df_abs %>%
  left_join(neg_labeled, by = "iso3") %>%
  left_join(pos_labeled, by = "iso3", suffix = c("", "_pos")) %>%
  mutate(
    abs_change_bin = case_when(
      !is.na(abs_change_bin) ~ abs_change_bin,
      !is.na(abs_change_bin_pos) ~ abs_change_bin_pos,
      change_group == "ZERO" ~ lab_zero,
      TRUE ~ NA_character_
    )
  ) %>%
  select(iso3, pre_sum, post_sum, abs_change, abs_change_bin)

# Order legend: more negative decreases -> zero -> more positive increases
dec_levels <- df_abs_binned %>%
  filter(grepl("^Decrease:", abs_change_bin)) %>%
  group_by(abs_change_bin) %>%
  summarise(v = min(abs_change, na.rm = TRUE), .groups = "drop") %>%
  arrange(v) %>%
  pull(abs_change_bin)

inc_levels <- df_abs_binned %>%
  filter(grepl("^Increase:", abs_change_bin)) %>%
  group_by(abs_change_bin) %>%
  summarise(v = max(abs_change, na.rm = TRUE), .groups = "drop") %>%
  arrange(v) %>%
  pull(abs_change_bin)

levels_all <- c(dec_levels, lab_zero, inc_levels)

df_abs_binned <- df_abs_binned %>%
  mutate(
    abs_change_bin = factor(abs_change_bin, levels = levels_all, ordered = TRUE)
  )

## ----------------------------
## 4.5) Save country table + map
## ----------------------------
write_csv(
  df_abs_binned,
  file.path(out_dir, "fig4_eventtime_country_abs_change_SUM_pre(-3:-1)_post(3:5).csv")
)

world_abs <- world %>% left_join(df_abs_binned, by = "iso3")

p4_abs <- ggplot(world_abs) +
  geom_sf(aes(fill = abs_change_bin), linewidth = 0.1) +
  scale_fill_brewer(palette = "RdBu", direction = -1, na.value = "grey90") +
  labs(
    title = "Absolute Change in Anti-LGBTQ Funding by Country (Event Time: SUM -3:-1 vs SUM +3:+5)",
    fill  = "Absolute change (3-year sums)\nPost − Pre (2024 $)"
  )

ggsave(
  filename = file.path(out_dir, "fig4_map_abs_change_anti_funding_eventtime_5bins_diverging.png"),
  plot = p4_abs,
  width = 12, height = 7, dpi = 300
)
## ============================================================
## TABLE: % of URL observations that list COUNTRIES vs region-only, by year (through 2023)
## ============================================================
country_cols <- grep("^tot_country__", names(schedf_adj), value = TRUE)
unspec_col <- "tot_country__UNSPEC"
zzz_col    <- "tot_country__ZZZ"
country_cols_actual <- setdiff(country_cols, c(unspec_col, zzz_col))

df_country_list_share <- schedf_adj %>%
  filter(!is.na(tax_year), tax_year <= max_plot_year) %>%
  mutate(
    tax_year = suppressWarnings(as.integer(tax_year)),
    across(all_of(country_cols), ~ as.numeric(replace_na(readr::parse_number(as.character(.x)), 0)))
  ) %>%
  mutate(
    lists_countries = if_else(
      rowSums(across(all_of(country_cols_actual), ~ .x), na.rm = TRUE) > 0,
      1L, 0L
    )
  ) %>%
  group_by(tax_year) %>%
  summarise(
    n_urls = n(),
    n_lists_countries = sum(lists_countries, na.rm = TRUE),
    pct_lists_countries = 100 * n_lists_countries / n_urls,
    .groups = "drop"
  ) %>%
  arrange(tax_year)

write_csv(df_country_list_share, file.path(out_dir, "table_pct_urls_listing_countries_by_year.csv"))
print(df_country_list_share)

cat("\nDONE. Outputs saved to:", out_dir, "\n")
cat(" - fig1_trend_anti_spending_by_region_millions.png\n")
cat(" - fig1b_trend_anti_share_by_region.png\n")
cat(" - fig2_map_anti_spending_yearly_average_7bins.png\n")
cat(" - fig3_map_anti_share_average_7bins.png\n")
cat(" - fig4_map_pct_change_anti_funding_7bins_diverging.png\n")
cat(" - fig4_country_pct_change_2010_2013_vs_2017_2020.csv\n")
cat(" - table_pct_urls_listing_countries_by_year.csv\n")
