# ============================================================
# FEATURE ENGINEERING - BDS TP.HCM
# Chay: Rscript feature_engineering.R
# Input uu tien: data/hcmc_bds_combined_raw.csv, fallback: data/hcmc_bds.sqlite, data/hcmc_bds_raw.csv
# Output: data/hcmc_bds_featured.csv
# ============================================================

if (dir.exists("R_libs")) .libPaths(c(normalizePath("R_libs"), .libPaths()))

required_packages <- c("dplyr", "readr", "lubridate")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Thieu package: ", paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(readr)
library(lubridate)

RAW_CSV <- "data/hcmc_bds_raw.csv"
COMBINED_RAW_CSV <- "data/hcmc_bds_combined_raw.csv"
SQLITE_PATH <- "data/hcmc_bds.sqlite"
FEATURED_CSV <- "data/hcmc_bds_featured.csv"

read_project_data <- function() {
  if (file.exists(COMBINED_RAW_CSV)) {
    return(read_csv(COMBINED_RAW_CSV, show_col_types = FALSE))
  }

  if (file.exists(SQLITE_PATH) && requireNamespace("DBI", quietly = TRUE) &&
      requireNamespace("RSQLite", quietly = TRUE)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), SQLITE_PATH)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    return(DBI::dbReadTable(con, "listings"))
  }

  if (file.exists(RAW_CSV)) {
    return(read_csv(RAW_CSV, show_col_types = FALSE))
  }

  stop("Chua co data. Hay chay scraper va scripts/merge_sources.R truoc.")
}

haversine_km <- function(lat1, lon1, lat2 = 10.7758, lon2 = 106.7009) {
  radius <- 6371
  to_rad <- pi / 180
  d_lat <- (lat2 - lat1) * to_rad
  d_lon <- (lon2 - lon1) * to_rad
  a <- sin(d_lat / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(d_lon / 2)^2
  2 * radius * atan2(sqrt(a), sqrt(1 - a))
}

mode_numeric <- function(x, default = 0) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(default)
  as.numeric(names(sort(table(x), decreasing = TRUE))[1])
}

in_hcmc_bbox <- function(lat, lon) {
  !is.na(lat) & !is.na(lon) &
    lat >= 10.30 & lat <= 11.20 &
    lon >= 106.00 & lon <= 107.30
}

build_features <- function(df) {
  required_cols <- c("price", "area", "rooms", "district_name", "category_name",
                     "ward", "lat", "lon", "posted_at", "category_id")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Data thieu cot: ", paste(missing_cols, collapse = ", "))
  }

  if (!"source" %in% names(df)) df$source <- "unknown"
  if (!"source_id" %in% names(df)) {
    df$source_id <- if ("ad_id" %in% names(df)) as.character(df$ad_id) else seq_len(nrow(df))
  }
  if (!"is_rent" %in% names(df)) df$is_rent <- NA

  df <- df %>%
    mutate(
      category_id = as.character(category_id),
      source = as.character(source),
      source_id = as.character(source_id),
      price = as.numeric(price),
      area = as.numeric(area),
      rooms = as.numeric(rooms),
      lat = as.numeric(lat),
      lon = as.numeric(lon),
      district_name = if_else(is.na(district_name) | district_name == "", "Unknown", as.character(district_name)),
      category_name = if_else(is.na(category_name) | category_name == "", "Unknown", as.character(category_name)),
      ward = if_else(is.na(ward) | ward == "", "Unknown", as.character(ward)),
      posted_at = suppressWarnings(as_datetime(posted_at)),
      scraped_at = if ("scraped_at" %in% names(.)) suppressWarnings(as_datetime(scraped_at)) else as_datetime(NA),
      posted_at = coalesce(posted_at, scraped_at),
      lat = if_else(in_hcmc_bbox(lat, lon), lat, NA_real_),
      lon = if_else(in_hcmc_bbox(lat, lon), lon, NA_real_),
      has_area = !is.na(area) & area > 0,
      has_rooms = !is.na(rooms),
      has_coord = !is.na(lat) & !is.na(lon),
      is_rent_raw = suppressWarnings(as.logical(is_rent)),
      is_rent = coalesce(is_rent_raw, category_id %in% c("1030", "1050")),
      transaction_type = if_else(is_rent, "Cho thuê", "Bán")
    ) %>%
    filter(!is.na(price), price > 0)

  area_medians <- df %>%
    filter(has_area) %>%
    group_by(district_name, category_name) %>%
    summarise(area_median_group = median(area, na.rm = TRUE), .groups = "drop")

  room_modes <- df %>%
    group_by(category_name) %>%
    summarise(room_mode_category = mode_numeric(rooms), .groups = "drop")

  ward_prices <- df %>%
    group_by(ward) %>%
    summarise(
      ward_median_price = median(price, na.rm = TRUE),
      ward_listing_count = n(),
      .groups = "drop"
    )

  global_area_median <- median(df$area[df$has_area], na.rm = TRUE)
  if (is.na(global_area_median)) global_area_median <- 50

  global_ward_price <- median(df$price, na.rm = TRUE)

  df %>%
    left_join(area_medians, by = c("district_name", "category_name")) %>%
    left_join(room_modes, by = "category_name") %>%
    left_join(ward_prices, by = "ward") %>%
    mutate(
      area = if_else(!has_area, coalesce(area_median_group, global_area_median), area),
      rooms = if_else(!has_rooms, coalesce(room_mode_category, 0), rooms),
      price_per_m2 = if_else(!is.na(area) & area > 0, price / area, NA_real_),
      log_price = log1p(price),
      log_area = log1p(area),
      log_price_per_m2 = if_else(!is.na(price_per_m2) & price_per_m2 > 0,
                                 log1p(price_per_m2), NA_real_),
      distance_to_center = if_else(has_coord, haversine_km(lat, lon), NA_real_),
      distance_to_center = if_else(is.na(distance_to_center),
                                   median(distance_to_center, na.rm = TRUE),
                                   distance_to_center),
      ward_median_price = coalesce(ward_median_price, global_ward_price),
      ward_price_encoded = log1p(ward_median_price),
      listing_age_days = as.numeric(difftime(Sys.time(), posted_at, units = "days")),
      listing_age_days = if_else(is.na(listing_age_days) | listing_age_days < 0, 0, listing_age_days),
      posted_hour = hour(posted_at),
      posted_wday = wday(posted_at, label = TRUE),
      is_weekend_post = wday(posted_at) %in% c(1, 7),
      price_segment = ntile(price, 4),
      price_segment = factor(price_segment, labels = c("Re", "Trung binh", "Cao", "Cao cap")),
      transaction_type = as.factor(transaction_type),
      source = as.factor(source),
      district_name = as.factor(district_name),
      category_name = as.factor(category_name),
      ward = as.factor(ward)
    ) %>%
    select(-area_median_group, -room_mode_category, -is_rent_raw)
}

if (identical(environment(), globalenv())) {
  dir.create("data", showWarnings = FALSE)
  df <- read_project_data()
  featured <- build_features(df)
  write_csv(featured, FEATURED_CSV)
  cat(sprintf("Da tao %s voi %d dong, %d cot.\n",
              FEATURED_CSV, nrow(featured), ncol(featured)))
}
