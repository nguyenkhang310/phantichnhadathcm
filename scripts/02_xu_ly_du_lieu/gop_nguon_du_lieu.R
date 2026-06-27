source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()
source(PATHS$district_normalization_script)
source(PATHS$data_standardization_script)

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

CHOTOT_RAW <- PATHS$chotot_raw_csv
COMBINED_RAW <- PATHS$combined_raw_csv

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
    ) %>%
    standardize_listing_schema()
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

  source_files <- PATHS$source_raw_csvs[file.exists(PATHS$source_raw_csvs)]
  if (length(source_files) > 0) {
    pieces <- append(pieces, map(source_files, read_source_file))
  }

  if (length(pieces) == 0) {
    stop("Chua co raw data. Hay chay scraper truoc.")
  }

  bind_rows(pieces)
}

normalize_listing_url <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- NA_character_
  x <- sub("/+$", "", x)
  tolower(x)
}

normalize_duplicate_text <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[is.na(x) | x == ""] <- NA_character_
  x <- gsub("[[:space:]]+", " ", x)
  x
}

valid_price_per_m2 <- function(price_per_m2, is_rent) {
  !is.na(price_per_m2) &
    (
      (!is_rent & price_per_m2 >= 5e5 & price_per_m2 <= 2e9) |
        (is_rent & price_per_m2 >= 1e4 & price_per_m2 <= 5e6)
    )
}

clean_combined_sources <- function(df) {
  cleaned <- df %>%
    mutate(
      source = if_else(is.na(source) | source == "", "unknown", as.character(source)),
      source_id = if_else(is.na(source_id) | source_id == "", paste(source, ad_id, sep = "_"), as.character(source_id)),
      ad_id = if_else(is.na(ad_id) | ad_id == "", source_id, as.character(ad_id)),
      title = clean_listing_title(title),
      price = as.numeric(price),
      area = as.numeric(area),
      rooms = as.numeric(rooms),
      lat = as.numeric(lat),
      lon = as.numeric(lon),
      category_id = as.character(category_id),
      category_name = district_clean_label(category_name),
      district_name = canonical_hcmc_district(district_name, address, title, ad_url),
      ward = district_clean_label(ward),
      ad_url_key = normalize_listing_url(ad_url),
      price_m = if_else(is.na(price_m), price / 1e6, as.numeric(price_m)),
      price_per_m2 = if_else(is.na(price_per_m2) & !is.na(area) & area > 0, price / area, as.numeric(price_per_m2)),
      has_coord = as.integer(!is.na(lat) & !is.na(lon)),
      is_rent = as.integer(coalesce(as.logical(is_rent), category_id %in% c("1030", "1050")))
    ) %>%
    filter(!is.na(price), price > 0) %>%
    filter(
      (as.logical(is_rent) & price >= 300000 & price <= 2e9) |
        (!as.logical(is_rent) & price >= 300000000 & price <= 500e9)
    ) %>%
    filter(is.na(area) | (area >= 5 & area <= 5000)) %>%
    filter(is.na(price_per_m2) | valid_price_per_m2(price_per_m2, as.logical(is_rent))) %>%
    arrange(desc(scraped_at)) %>%
    distinct(source_id, .keep_all = TRUE)

  with_url <- cleaned %>%
    filter(!is.na(ad_url_key), ad_url_key != "") %>%
    distinct(ad_url_key, .keep_all = TRUE)
  without_url <- cleaned %>% filter(is.na(ad_url_key) | ad_url_key == "")

  by_url <- bind_rows(with_url, without_url) %>%
    mutate(
      duplicate_key = if_else(
        !is.na(title) & title != "" & !is.na(price) & !is.na(area),
        paste(
          source,
          as.logical(is_rent),
          district_name,
          category_name,
          round(price / 100000) * 100000,
          round(area, 2),
          normalize_duplicate_text(title),
          sep = "|"
        ),
        NA_character_
      )
    )

  with_key <- by_url %>%
    filter(!is.na(duplicate_key), duplicate_key != "") %>%
    distinct(duplicate_key, .keep_all = TRUE)
  without_key <- by_url %>% filter(is.na(duplicate_key) | duplicate_key == "")

  bind_rows(with_key, without_key) %>%
    select(-ad_url_key, -duplicate_key) %>%
    standardize_listing_schema()
}

run_merge_sources <- function() {
  dir.create(dirname(COMBINED_RAW), recursive = TRUE, showWarnings = FALSE)
  combined <- read_all_sources() %>% clean_combined_sources()
  write_csv(combined, COMBINED_RAW)

  summary <- combined %>%
    count(source, category_name, is_rent, name = "rows") %>%
    arrange(source, desc(rows))

  message("Da luu ", nrow(combined), " dong vao ", COMBINED_RAW)
  print(summary)
  invisible(combined)
}

if (sys.nframe() == 0) {
  run_merge_sources()
}
