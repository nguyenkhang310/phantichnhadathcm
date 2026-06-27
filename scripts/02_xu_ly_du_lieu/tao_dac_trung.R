source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()
source(PATHS$district_normalization_script)
source(PATHS$data_standardization_script)
source(PATHS$model_feature_script)

required_packages <- c("dplyr", "readr", "lubridate", "stringr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Thieu package: ", paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(readr)
library(lubridate)
library(stringr)

RAW_CSV <- PATHS$chotot_raw_csv
COMBINED_RAW_CSV <- PATHS$combined_raw_csv
SQLITE_PATH <- PATHS$chotot_sqlite
FEATURED_CSV <- PATHS$featured_csv

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

  stop("Chua co data. Hay chay scraper va scripts/02_xu_ly_du_lieu/gop_nguon_du_lieu.R truoc.")
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

strip_vietnamese <- function(x) {
  x <- tolower(as.character(x))
  replacements <- list(
    a = "[àáạảãâầấậẩẫăằắặẳẵ]",
    e = "[èéẹẻẽêềếệểễ]",
    i = "[ìíịỉĩ]",
    o = "[òóọỏõôồốộổỗơờớợởỡ]",
    u = "[ùúụủũưừứựửữ]",
    y = "[ỳýỵỷỹ]",
    d = "đ"
  )
  for (replacement in names(replacements)) {
    x <- gsub(replacements[[replacement]], replacement, x)
  }
  x
}

is_missing_label <- function(x) {
  key <- strip_vietnamese(trimws(as.character(x)))
  is.na(x) | key %in% c("", "unknown", "khong ro", "na", "nan", "null")
}

clean_display_label <- function(x, fallback = "Không rõ") {
  x <- trimws(as.character(x))
  ifelse(is_missing_label(x), fallback, x)
}

extract_first_number_pair <- function(text, pair_index = 1) {
  match <- str_match(text, "([0-9]+([\\.,][0-9]+)?)\\s*[xX]\\s*([0-9]+([\\.,][0-9]+)?)")
  value <- suppressWarnings(as.numeric(str_replace(match[, ifelse(pair_index == 1, 2, 4)], ",", ".")))
  ifelse(is.na(value) | value <= 0 | value > 1000, NA_real_, value)
}

extract_first_number_by_pattern <- function(text, pattern, max_value = 1000) {
  match <- str_match(text, pattern)
  value <- suppressWarnings(as.numeric(str_replace(match[, 2], ",", ".")))
  ifelse(is.na(value) | value < 0 | value > max_value, NA_real_, value)
}

has_text_pattern <- function(text, pattern) {
  as.integer(str_detect(text, pattern))
}

valid_price_per_m2 <- function(price_per_m2, is_rent) {
  !is.na(price_per_m2) &
    (
      (!is_rent & price_per_m2 >= 5e5 & price_per_m2 <= 2e9) |
        (is_rent & price_per_m2 >= 1e4 & price_per_m2 <= 5e6)
    )
}

normalize_duplicate_text <- function(x) {
  x <- tolower(str_squish(as.character(x)))
  if_else(is.na(x) | x == "", NA_character_, x)
}

build_features <- function(df) {
  required_cols <- c("price", "area", "rooms", "district_name", "category_name",
                     "ward", "lat", "lon", "posted_at", "category_id")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Data thieu cot: ", paste(missing_cols, collapse = ", "))
  }

  now_ref <- lubridate::now(tzone = "Asia/Ho_Chi_Minh")
  max_posted_at <- lubridate::as_datetime(as.Date(now_ref) + 1, tz = "Asia/Ho_Chi_Minh")

  if (!"source" %in% names(df)) df$source <- "unknown"
  if (!"title" %in% names(df)) df$title <- ""
  if (!"address" %in% names(df)) df$address <- ""
  if (!"source_id" %in% names(df)) {
    df$source_id <- if ("ad_id" %in% names(df)) as.character(df$ad_id) else seq_len(nrow(df))
  }
  if (!"is_rent" %in% names(df)) df$is_rent <- NA

  df <- df %>%
    mutate(
      category_id = as.character(category_id),
      source = as.character(source),
      source_id = as.character(source_id),
      title = clean_listing_title(title, fallback = "Không rõ"),
      address = clean_text_field(address, fallback = "Không rõ"),
      price = as.numeric(price),
      area = as.numeric(area),
      rooms = as.numeric(rooms),
      lat = as.numeric(lat),
      lon = as.numeric(lon),
      district_name = canonical_hcmc_district(district_name, address, title, if ("ad_url" %in% names(.)) ad_url else ""),
      category_name = clean_display_label(category_name),
      ward = clean_display_label(ward),
      text_raw = str_squish(paste(title, address, category_name)),
      text_key = strip_vietnamese(text_raw),
      frontage_width_m = extract_first_number_pair(text_key, 1),
      frontage_length_m = extract_first_number_pair(text_key, 2),
      frontage_ratio = if_else(!is.na(frontage_width_m) & !is.na(frontage_length_m) & frontage_length_m > 0,
                               frontage_width_m / frontage_length_m, NA_real_),
      inferred_floors = coalesce(
        extract_first_number_by_pattern(text_key, "h\\s*\\+\\s*([0-9]+([\\.,][0-9]+)?)", 80),
        extract_first_number_by_pattern(text_key, "([0-9]+([\\.,][0-9]+)?)\\s*(tang|lau|tam)", 80)
      ),
      inferred_rooms = coalesce(
        extract_first_number_by_pattern(text_key, "([0-9]+)\\s*(pn|phong ngu)", 50),
        rooms
      ),
      title_has_frontage = has_text_pattern(text_key, "\\bmt\\b|mat tien|mat pho|mat duong|mat bang"),
      title_has_alley = has_text_pattern(text_key, "\\bhxh\\b|hem|ngo "),
      title_has_car_access = has_text_pattern(text_key, "\\bhxh\\b|xe hoi|o to|oto"),
      title_has_corner = has_text_pattern(text_key, "goc|2 mat|hai mat"),
      title_has_elevator = has_text_pattern(text_key, "thang may|elevator"),
      title_has_furnished = has_text_pattern(text_key, "noi that|full noi that|full nt|furnished"),
      title_has_legal = has_text_pattern(text_key, "so hong|phap ly|giay to|hop le"),
      title_has_income_info = has_text_pattern(text_key, "\\bhd\\b|hop dong|dong tien|cho thue"),
      title_token_count = str_count(text_key, "\\S+"),
      posted_at_raw = posted_at,
      posted_at = suppressWarnings(as_datetime(posted_at_raw, tz = "Asia/Ho_Chi_Minh")),
      scraped_at = if ("scraped_at" %in% names(.)) {
        suppressWarnings(as_datetime(scraped_at, tz = "Asia/Ho_Chi_Minh"))
      } else {
        as_datetime(NA, tz = "Asia/Ho_Chi_Minh")
      },
      posted_date_source = case_when(
        !is.na(posted_at) ~ "posted_at",
        !is.na(scraped_at) ~ "scraped_at",
        TRUE ~ "fallback_now"
      ),
      posted_at = coalesce(posted_at, scraped_at),
      posted_date_source = if_else(
        !is.na(posted_at) & posted_at > max_posted_at & !is.na(scraped_at) & scraped_at <= max_posted_at,
        "scraped_at_future_fix",
        posted_date_source
      ),
      posted_at = if_else(
        !is.na(posted_at) & posted_at > max_posted_at & !is.na(scraped_at) & scraped_at <= max_posted_at,
        scraped_at,
        posted_at
      ),
      posted_at = if_else(!is.na(posted_at) & posted_at > max_posted_at, as_datetime(NA_real_), posted_at),
      posted_at = coalesce(posted_at, scraped_at, now_ref),
      posted_date_source = if_else(is.na(posted_date_source), "fallback_now", posted_date_source),
      lat = if_else(in_hcmc_bbox(lat, lon), lat, NA_real_),
      lon = if_else(in_hcmc_bbox(lat, lon), lon, NA_real_),
      has_area = !is.na(area) & area > 0,
      has_rooms = !is.na(rooms),
      has_coord = !is.na(lat) & !is.na(lon),
      is_rent_raw = suppressWarnings(as.logical(is_rent)),
      is_rent = coalesce(is_rent_raw, category_id %in% c("1030", "1050")),
      transaction_type = if_else(is_rent, "Cho thuê", "Bán"),
      price_per_m2_raw = if_else(!is.na(area) & area > 0, price / area, NA_real_)
    ) %>%
    filter(!is.na(price), price > 0) %>%
    filter(
      (is_rent & price >= 300000 & price <= 2e9) |
        (!is_rent & price >= 300000000 & price <= 500e9)
    ) %>%
    filter(is.na(price_per_m2_raw) | valid_price_per_m2(price_per_m2_raw, is_rent))

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
      inferred_rooms = coalesce(inferred_rooms, rooms),
      frontage_width_m = coalesce(frontage_width_m, median(frontage_width_m, na.rm = TRUE), 0),
      frontage_length_m = coalesce(frontage_length_m, median(frontage_length_m, na.rm = TRUE), 0),
      frontage_ratio = coalesce(frontage_ratio, median(frontage_ratio, na.rm = TRUE), 0),
      inferred_floors = coalesce(inferred_floors, median(inferred_floors, na.rm = TRUE), 0),
      inferred_rooms = coalesce(inferred_rooms, median(inferred_rooms, na.rm = TRUE), 0),
      title_token_count = coalesce(title_token_count, median(title_token_count, na.rm = TRUE), 0),
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
      posted_hour = coalesce(hour(posted_at), 0L),
      posted_wday = as.character(wday(posted_at, label = TRUE)),
      posted_wday = if_else(is.na(posted_wday) | posted_wday == "", as.character(wday(now_ref, label = TRUE)), posted_wday),
      is_weekend_post = wday(posted_at) %in% c(1, 7),
      price_segment = ntile(price, 4),
      price_segment = factor(price_segment, labels = c("Re", "Trung binh", "Cao", "Cao cap")),
      duplicate_key = if_else(
        !is.na(title) & title != "" & !is.na(price) & !is.na(area),
        paste(
          source,
          transaction_type,
          district_name,
          category_name,
          round(price / 100000) * 100000,
          round(area, 2),
          normalize_duplicate_text(title),
          sep = "|"
        ),
        paste0("row_", row_number())
      ),
      transaction_type = as.factor(transaction_type),
      source = as.factor(source),
      district_name = as.factor(district_name),
      category_name = as.factor(category_name),
      ward = as.factor(ward)
    ) %>%
    add_model_quality_features() %>%
    filter(valid_price_per_m2(price_per_m2, is_rent)) %>%
    arrange(desc(posted_at), desc(scraped_at)) %>%
    distinct(duplicate_key, .keep_all = TRUE) %>%
    select(
      -area_median_group, -room_mode_category, -is_rent_raw, -text_raw,
      -text_key, -posted_at_raw, -price_per_m2_raw, -duplicate_key
    )
}

if (sys.nframe() == 0) {
  dir.create(dirname(FEATURED_CSV), recursive = TRUE, showWarnings = FALSE)
  df <- read_project_data()
  featured <- build_features(df)
  write_csv(featured, FEATURED_CSV)
  cat(sprintf("Da tao %s voi %d dong, %d cot.\n",
              FEATURED_CSV, nrow(featured), ncol(featured)))
}
