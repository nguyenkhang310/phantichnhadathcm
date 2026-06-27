source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()
source(PATHS$district_normalization_script)
source(PATHS$data_standardization_script)

required_packages <- c("dplyr", "readr", "stringr", "lubridate")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Thieu package: ", paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(readr)
library(stringr)
library(lubridate)

parse_mogi_price_one <- function(gia_raw, gia_vnd) {
  raw <- str_to_lower(str_squish(as.character(gia_raw)))
  fallback <- suppressWarnings(as.numeric(gia_vnd))
  fallback <- ifelse(is.na(fallback) | fallback <= 0, NA_real_, fallback)
  if (is.na(raw) || raw == "") return(fallback)

  to_num <- function(x) suppressWarnings(as.numeric(str_replace(x, ",", ".")))
  total <- 0
  matched <- FALSE

  ty <- str_match(raw, "([0-9]+([\\.,][0-9]+)?)\\s*(tỷ|ty)")[, 2]
  if (!is.na(ty)) {
    total <- total + to_num(ty) * 1e9
    matched <- TRUE
  }

  trieu_after_ty <- str_match(raw, "(tỷ|ty)\\s*([0-9]+([\\.,][0-9]+)?)\\s*(triệu|trieu)")[, 3]
  trieu <- if (!is.na(trieu_after_ty)) {
    trieu_after_ty
  } else {
    str_match(raw, "([0-9]+([\\.,][0-9]+)?)\\s*(triệu|trieu)")[, 2]
  }
  if (!is.na(trieu)) {
    total <- total + to_num(trieu) * 1e6
    matched <- TRUE
  }

  nghin_after_trieu <- str_match(raw, "(triệu|trieu)\\s*([0-9]+([\\.,][0-9]+)?)\\s*(nghìn|nghin|ngàn|ngan|k\\b)")[, 3]
  nghin <- if (!is.na(nghin_after_trieu)) {
    nghin_after_trieu
  } else if (!matched) {
    str_match(raw, "([0-9]+([\\.,][0-9]+)?)\\s*(nghìn|nghin|ngàn|ngan|k\\b)")[, 2]
  } else {
    NA_character_
  }
  if (!is.na(nghin)) {
    total <- total + to_num(nghin) * 1e3
    matched <- TRUE
  }

  if (matched && total > 0) return(total)

  raw_digits <- str_replace_all(raw, "[^0-9]", "")
  parsed <- suppressWarnings(as.numeric(raw_digits))
  ifelse(is.na(parsed) | parsed <= 0, fallback, parsed)
}

parse_mogi_price <- function(gia_raw, gia_vnd) {
  mapply(parse_mogi_price_one, gia_raw, gia_vnd, USE.NAMES = FALSE)
}

parse_mogi_date <- function(x) {
  if (inherits(x, "Date")) return(as.character(x))
  raw <- str_squish(as.character(x))
  numeric_x <- suppressWarnings(as.numeric(raw))
  parsed_numeric <- as.Date(numeric_x, origin = "1970-01-01")
  parsed <- coalesce(
    suppressWarnings(ymd(raw)),
    suppressWarnings(dmy(raw)),
    parsed_numeric
  )
  ifelse(is.na(parsed), as.character(Sys.Date()), as.character(parsed))
}

category_id_from_name <- function(category_name, is_rent) {
  key <- district_strip_vietnamese(category_name)
  case_when(
    str_detect(key, "can ho|chung cu") ~ "1010",
    str_detect(key, "dat|nen") ~ "1040",
    str_detect(key, "phong") ~ "1050",
    str_detect(key, "mat bang|van phong|shop|kho|xuong") ~ "1030",
    TRUE ~ "1020"
  )
}

standardize_mogi <- function(df) {
  if (!".source_priority" %in% names(df)) df$.source_priority <- 0L
  out <- df %>%
    mutate(
      source = "mogi",
      source_group = loai_gd,
      is_rent = as.integer(str_to_lower(loai_gd) == "thue"),
      price = parse_mogi_price(gia_raw, gia_vnd),
      area = suppressWarnings(as.numeric(dien_tich_m2)),
      rooms = suppressWarnings(as.numeric(so_phong_ngu)),
      title = clean_listing_title(tieu_de),
      price_str = str_squish(as.character(gia_raw)),
      address = str_squish(as.character(dia_chi)),
      district_name = canonical_hcmc_district(quan_huyen, dia_chi, tieu_de, url),
      ward = district_clean_label(str_extract(address, "(Phường|Xã|Thị trấn) [^,]+")),
      category_name = district_clean_label(loai_bds),
      category_id = category_id_from_name(category_name, is_rent),
      ad_id = if_else(!is.na(id_nguon) & as.character(id_nguon) != "", as.character(id_nguon), url),
      source_id = paste0("mogi_", ad_id),
      lat = suppressWarnings(as.numeric(lat)),
      lon = suppressWarnings(as.numeric(lon)),
      image = NA_character_,
      ad_url = as.character(url),
      source_url = ad_url,
      posted_at = parse_mogi_date(ngay_dang),
      scraped_at = as.character(Sys.time()),
      page_fetched = NA_integer_,
      price_m = price / 1e6,
      price_per_m2 = if_else(!is.na(area) & area > 0, price / area, NA_real_),
      has_coord = as.integer(!is.na(lat) & !is.na(lon)),
      .source_priority = as.integer(.source_priority)
    ) %>%
    transmute(
      source, source_group, source_id, ad_id, title, price, price_str,
      area, rooms, address, ward, district_id = NA_character_, district_name,
      category_id, category_name, lat, lon, image, ad_url, source_url,
      posted_at, scraped_at, page_fetched, price_m, price_per_m2,
      has_coord, is_rent, .source_priority
    )
  priority <- out$.source_priority
  out <- standardize_listing_schema(out, default_source = "mogi")
  out$.source_priority <- priority
  out
}

normalize_listing_url <- function(x) {
  x <- str_squish(as.character(x))
  x <- if_else(is.na(x) | x == "", NA_character_, x)
  x <- str_replace(x, "/+$", "")
  str_to_lower(x)
}

read_mogi_source <- function(path, priority) {
  read_csv(path, col_types = cols(.default = col_character())) %>%
    mutate(.source_priority = priority)
}

run_import_mogi <- function() {
  if (!file.exists(PATHS$mogi_source_csv)) {
    stop("Khong tim thay file Mogi: ", PATHS$mogi_source_csv)
  }

  dir.create(dirname(PATHS$mogi_raw_csv), recursive = TRUE, showWarnings = FALSE)
  source_files <- tibble(
    path = c(PATHS$mogi_source_csv, PATHS$mogi_source_csv_2, PATHS$mogi_scraped_csv),
    priority = c(1L, 2L, 3L)
  ) %>%
    filter(file.exists(path))

  raw <- bind_rows(mapply(
    read_mogi_source,
    source_files$path,
    source_files$priority,
    SIMPLIFY = FALSE
  ))

  clean <- standardize_mogi(raw) %>%
    mutate(url_key = normalize_listing_url(ad_url)) %>%
    filter(!is.na(title), title != "", !is.na(ad_url), ad_url != "") %>%
    filter(!is.na(price), price > 0, !is.na(area), area > 0) %>%
    filter(
      (as.logical(is_rent) & price >= 300000 & price <= 2e9) |
        (!as.logical(is_rent) & price >= 300000000 & price <= 500e9)
    ) %>%
    filter(area >= 5, area <= 5000) %>%
    arrange(desc(.source_priority), desc(scraped_at)) %>%
    distinct(source_id, .keep_all = TRUE) %>%
    distinct(url_key, .keep_all = TRUE) %>%
    select(-.source_priority, -url_key)

  write_csv(clean, PATHS$mogi_raw_csv)
  message("Da import ", nrow(clean), " dong Mogi vao ", PATHS$mogi_raw_csv)
  print(clean %>% count(is_rent, name = "rows"))
  invisible(clean)
}

if (sys.nframe() == 0) {
  run_import_mogi()
}
