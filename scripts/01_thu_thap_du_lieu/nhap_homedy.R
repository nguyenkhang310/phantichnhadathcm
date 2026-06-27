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

parse_homedy_date <- function(x) {
  if (inherits(x, "Date")) return(as.character(x))

  raw <- str_squish(as.character(x))
  raw[raw %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_

  numeric_x <- suppressWarnings(as.numeric(raw))
  parsed_numeric <- as.Date(numeric_x, origin = "1970-01-01")
  parsed_ymd <- suppressWarnings(ymd(raw))
  parsed_dmy <- suppressWarnings(dmy(raw))

  parsed <- coalesce(parsed_ymd, parsed_dmy, parsed_numeric)
  ifelse(is.na(parsed), as.character(Sys.Date()), as.character(parsed))
}

category_id_from_name <- function(category_name, is_rent) {
  key <- district_strip_vietnamese(category_name)
  case_when(
    str_detect(key, "can ho|chung cu") ~ "1010",
    str_detect(key, "dat|nen") ~ "1040",
    str_detect(key, "phong") ~ "1050",
    str_detect(key, "mat bang|van phong|shop|kho|xuong") ~ "1030",
    is_rent ~ "1030",
    TRUE ~ "1020"
  )
}

standardize_homedy <- function(df) {
  df %>%
    mutate(
      source = "homedy",
      source_group = str_to_lower(as.character(loai_gd)),
      is_rent = as.integer(source_group == "thue"),
      price = suppressWarnings(as.numeric(gia_vnd)),
      area = suppressWarnings(as.numeric(dien_tich_m2)),
      rooms = suppressWarnings(as.numeric(so_phong_ngu)),
      title = clean_listing_title(tieu_de),
      price_str = str_squish(as.character(gia_raw)),
      address = str_squish(as.character(dia_chi)),
      district_name = canonical_hcmc_district(quan_huyen, dia_chi, tieu_de, url),
      ward = district_clean_label(str_extract(address, "(Phường|Xã|Thị trấn) [^,]+")),
      category_name = district_clean_label(loai_bds),
      category_id = category_id_from_name(category_name, as.logical(is_rent)),
      ad_id = if_else(!is.na(id_nguon) & as.character(id_nguon) != "", as.character(id_nguon), as.character(url)),
      source_id = paste0("homedy_", ad_id),
      lat = suppressWarnings(as.numeric(lat)),
      lon = suppressWarnings(as.numeric(lon)),
      image = NA_character_,
      ad_url = as.character(url),
      source_url = ad_url,
      posted_at = parse_homedy_date(ngay_dang),
      scraped_at = as.character(Sys.time()),
      page_fetched = NA_integer_,
      price_m = price / 1e6,
      price_per_m2 = if_else(!is.na(area) & area > 0, price / area, NA_real_),
      has_coord = as.integer(!is.na(lat) & !is.na(lon))
    ) %>%
    transmute(
      source, source_group, source_id, ad_id, title, price, price_str,
      area, rooms, address, ward, district_id = NA_character_, district_name,
      category_id, category_name, lat, lon, image, ad_url, source_url,
      posted_at, scraped_at, page_fetched, price_m, price_per_m2,
      has_coord, is_rent
    ) %>%
    standardize_listing_schema(default_source = "homedy")
}

run_import_homedy <- function() {
  if (!file.exists(PATHS$homedy_source_csv)) {
    stop("Khong tim thay file Homedy: ", PATHS$homedy_source_csv)
  }

  dir.create(dirname(PATHS$homedy_raw_csv), recursive = TRUE, showWarnings = FALSE)
  raw <- read_csv(PATHS$homedy_source_csv, show_col_types = FALSE)
  clean <- standardize_homedy(raw) %>%
    filter(!is.na(title), title != "", !is.na(ad_url), ad_url != "") %>%
    filter(!is.na(price), price > 0, !is.na(area), area > 0) %>%
    filter(
      (as.logical(is_rent) & price >= 300000 & price <= 2e9) |
        (!as.logical(is_rent) & price >= 300000000 & price <= 500e9)
    ) %>%
    filter(area >= 5, area <= 5000) %>%
    distinct(source_id, .keep_all = TRUE)

  write_csv(clean, PATHS$homedy_raw_csv)
  message("Da import ", nrow(clean), " dong Homedy vao ", PATHS$homedy_raw_csv)
  print(clean %>% count(is_rent, name = "rows"))
  invisible(clean)
}

if (sys.nframe() == 0) {
  run_import_homedy()
}
