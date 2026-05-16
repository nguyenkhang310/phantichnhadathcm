# ============================================================
# ALONHADAT SCRAPER - BDS TP.HCM
# Chay:
#   ALONHADAT_MAX_PAGES=5 Rscript scripts/alonhadat_scraper.R
# Output:
#   data/sources/alonhadat_raw.csv
# ============================================================

if (dir.exists("R_libs")) .libPaths(c(normalizePath("R_libs"), .libPaths()))

required_packages <- c("httr", "rvest", "xml2", "dplyr", "purrr", "readr", "stringr", "lubridate", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Thieu package: ", paste(missing_packages, collapse = ", "),
    "\nCai bang: install.packages(c(",
    paste(sprintf('\"%s\"', missing_packages), collapse = ", "), "))"
  )
}

library(httr)
library(rvest)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(lubridate)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

CFG_ALO <- list(
  max_pages = as.integer(Sys.getenv("ALONHADAT_MAX_PAGES", "6")),
  delay_min = as.numeric(Sys.getenv("ALONHADAT_DELAY_MIN", "1.0")),
  delay_max = as.numeric(Sys.getenv("ALONHADAT_DELAY_MAX", "2.5")),
  output_dir = "data/sources",
  csv_file = "alonhadat_raw.csv"
)

ALO_SOURCES <- tibble::tribble(
  ~source_group, ~category_id, ~category_name, ~is_rent, ~base_url,
  "sale_all", "1020", "Nhà ở", FALSE, "https://alonhadat.com.vn/can-ban-nha-dat/ho-chi-minh",
  "sale_house", "1020", "Nhà ở", FALSE, "https://alonhadat.com.vn/can-ban-nha/ho-chi-minh",
  "sale_house_alley", "1020", "Nhà ở", FALSE, "https://alonhadat.com.vn/can-ban-nha-trong-hem/ho-chi-minh",
  "sale_apartment", "1010", "Căn hộ/Chung cư", FALSE, "https://alonhadat.com.vn/can-ban-can-ho-chung-cu",
  "sale_land", "1040", "Đất", FALSE, "https://alonhadat.com.vn/can-ban-dat/ho-chi-minh",
  "rent_all", "1030", "Văn phòng/Mặt bằng", TRUE, "https://alonhadat.com.vn/cho-thue-nha-dat/ho-chi-minh",
  "rent_apartment", "1010", "Căn hộ/Chung cư", TRUE, "https://alonhadat.com.vn/cho-thue-can-ho-chung-cu"
)

UA_POOL <- c(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/124.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/124.0.0.0 Safari/537.36",
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 Chrome/124.0.0.0 Safari/537.36"
)

make_page_url <- function(base_url, page) {
  if (page <= 1) return(base_url)
  paste0(base_url, "/trang-", page)
}

safe_text <- function(node, selector) {
  value <- node %>% html_element(selector) %>% html_text2()
  if (length(value) == 0 || is.na(value)) NA_character_ else value
}

safe_attr <- function(node, selector, attr) {
  value <- node %>% html_element(selector) %>% html_attr(attr)
  if (length(value) == 0 || is.na(value)) NA_character_ else value
}

normalize_url <- function(path) {
  if (is.na(path) || path == "") return(NA_character_)
  if (str_starts(path, "http")) path else paste0("https://alonhadat.com.vn", path)
}

extract_id <- function(url) {
  id <- str_match(url %||% "", "-([0-9]+)\\.html$")[, 2]
  ifelse(is.na(id), paste0("alo_", digest_text(url)), id)
}

digest_text <- function(x) {
  paste0(abs(sum(utf8ToInt(x %||% ""))), "_", nchar(x %||% ""))
}

extract_number <- function(x) {
  parsed <- str_extract(x %||% "", "[0-9]+([\\.,][0-9]+)?")
  as.numeric(str_replace(parsed, ",", "."))
}

extract_district <- function(address) {
  if (is.na(address)) return("Unknown")
  patterns <- c(
    "Thành phố Thủ Đức", "Quận [0-9]+", "Quận Bình Tân", "Quận Bình Thạnh",
    "Quận Gò Vấp", "Quận Phú Nhuận", "Quận Tân Bình", "Quận Tân Phú",
    "Huyện Bình Chánh", "Huyện Cần Giờ", "Huyện Củ Chi", "Huyện Hóc Môn", "Huyện Nhà Bè"
  )
  found <- str_extract(address, str_c(patterns, collapse = "|"))
  ifelse(is.na(found), "Unknown", found)
}

extract_ward <- function(address) {
  if (is.na(address)) return("Unknown")
  found <- str_extract(address, "(Phường|Xã|Thị trấn) [^,]+")
  ifelse(is.na(found), "Unknown", found)
}

parse_date <- function(label) {
  today <- Sys.Date()
  label <- str_to_lower(label %||% "")
  if (str_detect(label, "hôm nay")) return(as.character(today))
  if (str_detect(label, "hôm qua")) return(as.character(today - 1))
  parsed <- suppressWarnings(dmy(str_extract(label, "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}")))
  if (is.na(parsed)) as.character(today) else as.character(parsed)
}

fetch_html <- function(url) {
  Sys.sleep(runif(1, CFG_ALO$delay_min, CFG_ALO$delay_max))
  res <- RETRY(
    "GET",
    url,
    add_headers(
      "User-Agent" = sample(UA_POOL, 1),
      "Accept-Language" = "vi-VN,vi;q=0.9,en;q=0.8"
    ),
    timeout(20),
    times = 3,
    pause_base = 2,
    terminate_on = c(404)
  )
  if (status_code(res) != 200) return(NULL)
  html <- content(res, as = "text", encoding = "UTF-8")
  if (str_detect(html, "Vui lòng xác minh không phải Robot")) return(NULL)
  read_html(html)
}

parse_listing_node <- function(node, meta, page_url, page) {
  link_node <- node %>% html_element("a.link")
  ad_url <- normalize_url(link_node %>% html_attr("href"))
  address <- safe_text(node, ".property-address")
  price <- as.numeric(safe_attr(node, ".price [itemprop='price']", "content"))
  area <- extract_number(safe_text(node, ".area"))
  title <- safe_text(node, ".property-title")
  posted_at <- parse_date(safe_text(node, ".created-date"))
  rooms <- extract_number(safe_text(node, ".bedroom"))
  image <- normalize_url(safe_attr(node, ".thumbnail img", "src"))

  tibble(
    source = "alonhadat",
    source_group = meta$source_group,
    source_id = paste0("alonhadat_", extract_id(ad_url)),
    ad_id = paste0("alonhadat_", extract_id(ad_url)),
    title = title,
    price = price,
    price_str = safe_text(node, ".price"),
    area = area,
    rooms = rooms,
    address = address,
    ward = extract_ward(address),
    district_id = NA_character_,
    district_name = extract_district(address),
    category_id = meta$category_id,
    category_name = meta$category_name,
    lat = NA_real_,
    lon = NA_real_,
    image = image,
    ad_url = ad_url,
    source_url = page_url,
    posted_at = posted_at,
    scraped_at = as.character(Sys.time()),
    page_fetched = page,
    price_m = price / 1e6,
    price_per_m2 = if_else(!is.na(area) & area > 0, price / area, NA_real_),
    has_coord = 0L,
    is_rent = as.integer(meta$is_rent)
  )
}

fetch_source_page <- function(meta, page) {
  page_url <- make_page_url(meta$base_url, page)
  message("Alonhadat: ", meta$source_group, " / trang ", page)
  doc <- fetch_html(page_url)
  if (is.null(doc)) return(tibble())

  nodes <- doc %>% html_elements("article.property-item")
  if (length(nodes) == 0) return(tibble())

  map_dfr(nodes, parse_listing_node, meta = meta, page_url = page_url, page = page)
}

clean_alonhadat <- function(df) {
  if (nrow(df) == 0) return(df)
  df %>%
    filter(str_detect(address %||% "", "Hồ Chí Minh|TP\\.HCM|Tp Hồ Chí Minh|Thành phố Hồ Chí Minh")) %>%
    filter(!is.na(price), price > 0, !is.na(area), area >= 5, area <= 5000) %>%
    filter(price >= 300000, price <= 500e9) %>%
    distinct(source_id, .keep_all = TRUE)
}

run_alonhadat_scrape <- function(max_pages = CFG_ALO$max_pages) {
  dir.create(CFG_ALO$output_dir, recursive = TRUE, showWarnings = FALSE)
  raw <- pmap_dfr(
    expand.grid(source_idx = seq_len(nrow(ALO_SOURCES)), page = seq_len(max_pages)),
    function(source_idx, page) fetch_source_page(ALO_SOURCES[source_idx, ], page)
  )
  clean <- clean_alonhadat(raw)
  out <- file.path(CFG_ALO$output_dir, CFG_ALO$csv_file)

  if (file.exists(out)) {
    old <- read_csv(out, show_col_types = FALSE)
    clean <- bind_rows(old, clean) %>%
      arrange(desc(scraped_at)) %>%
      distinct(source_id, .keep_all = TRUE)
  }

  write_csv(clean, out)
  message("Da luu ", nrow(clean), " dong vao ", out)
  invisible(clean)
}

if (identical(environment(), globalenv())) {
  run_alonhadat_scrape()
}
