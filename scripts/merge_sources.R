# ============================================================
# MERGE SOURCES - Chuan hoa raw data tu nhieu nguon
# Chay:
#   Rscript scripts/merge_sources.R
# Input:
#   data/hcmc_bds_raw.csv
#   data/sources/*.csv
# Output:
#   data/hcmc_bds_combined_raw.csv
# ============================================================

if (dir.exists("R_libs")) .libPaths(c(normalizePath("R_libs"), .libPaths()))

required_packages <- c("dplyr", "readr", "purrr", "lubridate")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Thieu package: ", paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(readr)
library(purrr)
library(lubridate)

SOURCE_DIR <- "data/sources"
CHOTOT_RAW <- "data/hcmc_bds_raw.csv"
COMBINED_RAW <- "data/hcmc_bds_combined_raw.csv"

STANDARD_COLS <- c(
  "source", "source_group", "source_id", "ad_id", "title", "price", "price_str",
  "area", "rooms", "address", "ward", "district_id", "district_name",
  "category_id", "category_name", "lat", "lon", "image", "ad_url", "source_url",
  "posted_at", "scraped_at", "page_fetched", "price_m", "price_per_m2",
  "has_coord", "is_rent"
)

CHAR_COLS <- c(
  "source", "source_group", "source_id", "ad_id", "title", "price_str",
  "address", "ward", "district_id", "district_name", "category_id",
  "category_name", "image", "ad_url", "source_url", "posted_at", "scraped_at"
)

NUMERIC_COLS <- c("price", "area", "rooms", "lat", "lon", "price_m", "price_per_m2")
INTEGER_COLS <- c("page_fetched", "has_coord", "is_rent")

add_missing_cols <- function(df) {
  missing <- setdiff(STANDARD_COLS, names(df))
  for (col in missing) df[[col]] <- NA
  df %>%
    select(all_of(STANDARD_COLS)) %>%
    mutate(
      across(all_of(CHAR_COLS), as.character),
      across(all_of(NUMERIC_COLS), as.numeric),
      across(all_of(INTEGER_COLS), as.integer)
    )
}

read_source_file <- function(path, source_name = NULL) {
  df <- read_csv(path, show_col_types = FALSE)
  if (!"source" %in% names(df) || all(is.na(df$source))) {
    df$source <- source_name %||% tools::file_path_sans_ext(basename(path))
  }
  if (!"source_id" %in% names(df) || all(is.na(df$source_id))) {
    df$source_id <- paste(df$source, df$ad_id, sep = "_")
  }
  if (!"source_url" %in% names(df)) df$source_url <- df$ad_url
  add_missing_cols(df)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

read_all_sources <- function() {
  pieces <- list()

  if (file.exists(CHOTOT_RAW)) {
    chotot <- read_csv(CHOTOT_RAW, show_col_types = FALSE) %>%
      mutate(
        source = "chotot",
        source_group = as.character(category_name),
        source_id = paste0("chotot_", ad_id),
        source_url = ad_url,
        is_rent = as.integer(as.character(category_id) %in% c("1030", "1050"))
      )
    pieces <- append(pieces, list(add_missing_cols(chotot)))
  }

  if (dir.exists(SOURCE_DIR)) {
    source_files <- list.files(SOURCE_DIR, pattern = "\\.csv$", full.names = TRUE)
    source_files <- source_files[basename(source_files) != basename(COMBINED_RAW)]
    pieces <- append(pieces, map(source_files, read_source_file))
  }

  if (length(pieces) == 0) {
    stop("Chua co raw data. Hay chay scraper truoc.")
  }

  bind_rows(pieces)
}

clean_combined_sources <- function(df) {
  df %>%
    mutate(
      source = if_else(is.na(source) | source == "", "unknown", as.character(source)),
      source_id = if_else(is.na(source_id) | source_id == "", paste(source, ad_id, sep = "_"), as.character(source_id)),
      ad_id = if_else(is.na(ad_id) | ad_id == "", source_id, as.character(ad_id)),
      price = as.numeric(price),
      area = as.numeric(area),
      rooms = as.numeric(rooms),
      lat = as.numeric(lat),
      lon = as.numeric(lon),
      category_id = as.character(category_id),
      category_name = if_else(is.na(category_name) | category_name == "", "Unknown", as.character(category_name)),
      district_name = if_else(is.na(district_name) | district_name == "", "Unknown", as.character(district_name)),
      ward = if_else(is.na(ward) | ward == "", "Unknown", as.character(ward)),
      price_m = if_else(is.na(price_m), price / 1e6, as.numeric(price_m)),
      price_per_m2 = if_else(is.na(price_per_m2) & !is.na(area) & area > 0, price / area, as.numeric(price_per_m2)),
      has_coord = as.integer(!is.na(lat) & !is.na(lon)),
      is_rent = as.integer(coalesce(as.logical(is_rent), category_id %in% c("1030", "1050")))
    ) %>%
    filter(!is.na(price), price > 0) %>%
    filter(is.na(area) | (area >= 5 & area <= 5000)) %>%
    arrange(desc(scraped_at)) %>%
    distinct(source_id, .keep_all = TRUE)
}

run_merge_sources <- function() {
  dir.create("data", showWarnings = FALSE)
  combined <- read_all_sources() %>% clean_combined_sources()
  write_csv(combined, COMBINED_RAW)

  summary <- combined %>%
    count(source, category_name, is_rent, name = "rows") %>%
    arrange(source, desc(rows))

  message("Da luu ", nrow(combined), " dong vao ", COMBINED_RAW)
  print(summary)
  invisible(combined)
}

if (identical(environment(), globalenv())) {
  run_merge_sources()
}
