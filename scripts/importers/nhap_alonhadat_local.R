# ============================================================
# IMPORT ALONHADAT LOCAL CSV - Chuan hoa vao schema noi bo
# Chay:
#   Rscript scripts/importers/nhap_alonhadat_local.R
# Input:
#   data/raw/alonhadat/alonhadat_local_nguon.csv
# Output:
#   data/raw/alonhadat/alonhadat_local_schema_chuan.csv
# ============================================================

source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()

required_packages <- c("dplyr", "readr", "stringr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Thieu package: ", paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(readr)
library(stringr)

STANDARD_COLS <- c(
  "source", "source_group", "source_id", "ad_id", "title", "price", "price_str",
  "area", "rooms", "address", "ward", "district_id", "district_name",
  "category_id", "category_name", "lat", "lon", "image", "ad_url", "source_url",
  "posted_at", "scraped_at", "page_fetched", "price_m", "price_per_m2",
  "has_coord", "is_rent"
)

# Hàm normalize_url: phân tích chuỗi đầu vào thành giá trị chuẩn.
normalize_url <- function(x) {
  x <- str_squish(as.character(x))
  x <- if_else(is.na(x) | x == "", NA_character_, x)
  x <- str_replace(x, "/+$", "")
  str_to_lower(x)
}

# Hàm standardize_alonhadat_local: làm sạch và chuẩn hóa dữ liệu nguồn.
standardize_alonhadat_local <- function(df) {
  missing <- setdiff(STANDARD_COLS, names(df))
  for (col in missing) df[[col]] <- NA

  df %>%
    mutate(
      source = "alonhadat",
      source_group = if_else(is.na(source_group) | source_group == "", "sale_local_csv", as.character(source_group)),
      ad_id = if_else(!is.na(ad_id) & as.character(ad_id) != "", as.character(ad_id), as.character(ad_url)),
      source_id = paste0("alonhadat_local_", ad_id),
      title = str_squish(as.character(title)),
      price = suppressWarnings(as.numeric(price)),
      price_str = str_squish(as.character(price_str)),
      area = suppressWarnings(as.numeric(area)),
      rooms = suppressWarnings(as.numeric(rooms)),
      address = str_squish(as.character(address)),
      ward = str_squish(as.character(ward)),
      district_id = as.character(district_id),
      district_name = str_squish(as.character(district_name)),
      category_id = as.character(category_id),
      category_name = str_squish(as.character(category_name)),
      lat = suppressWarnings(as.numeric(lat)),
      lon = suppressWarnings(as.numeric(lon)),
      image = as.character(image),
      ad_url = as.character(ad_url),
      source_url = if_else(is.na(source_url) | source_url == "", ad_url, as.character(source_url)),
      posted_at = as.character(posted_at),
      scraped_at = as.character(Sys.time()),
      page_fetched = suppressWarnings(as.integer(page_fetched)),
      price_m = price / 1e6,
      price_per_m2 = if_else(!is.na(area) & area > 0, price / area, NA_real_),
      has_coord = as.integer(!is.na(lat) & !is.na(lon)),
      is_rent = 0L
    ) %>%
    select(all_of(STANDARD_COLS))
}

# Hàm existing_alonhadat_urls: lấy URL Alonhadat đã có để tránh trùng.
existing_alonhadat_urls <- function() {
  if (!file.exists(PATHS$alonhadat_raw_csv)) return(character())
  read_csv(PATHS$alonhadat_raw_csv, show_col_types = FALSE) %>%
    mutate(url_key = normalize_url(ad_url)) %>%
    filter(!is.na(url_key), url_key != "") %>%
    pull(url_key) %>%
    unique()
}

# Hàm run_import_alonhadat_local: chạy toàn bộ bước xử lý chính.
run_import_alonhadat_local <- function() {
  if (!file.exists(PATHS$alonhadat_local_source_csv)) {
    stop("Khong tim thay file Alonhadat local: ", PATHS$alonhadat_local_source_csv)
  }

  dir.create(dirname(PATHS$alonhadat_local_raw_csv), recursive = TRUE, showWarnings = FALSE)
  raw <- read_csv(PATHS$alonhadat_local_source_csv, show_col_types = FALSE)
  old_urls <- existing_alonhadat_urls()

  clean <- standardize_alonhadat_local(raw) %>%
    mutate(url_key = normalize_url(ad_url)) %>%
    filter(!is.na(title), title != "", !is.na(ad_url), ad_url != "") %>%
    filter(!is.na(price), price >= 300000000, price <= 500e9) %>%
    filter(!is.na(area), area >= 5, area <= 5000) %>%
    filter(!(url_key %in% old_urls)) %>%
    distinct(source_id, .keep_all = TRUE) %>%
    distinct(url_key, .keep_all = TRUE) %>%
    select(-url_key)

  write_csv(clean, PATHS$alonhadat_local_raw_csv)
  message("Da import ", nrow(clean), " dong Alonhadat local vao ", PATHS$alonhadat_local_raw_csv)
  message("Da bo qua ", nrow(raw) - nrow(clean), " dong trung/khong hop le.")
  print(clean %>% count(is_rent, name = "rows"))
  invisible(clean)
}

if (sys.nframe() == 0) {
  run_import_alonhadat_local()
}
