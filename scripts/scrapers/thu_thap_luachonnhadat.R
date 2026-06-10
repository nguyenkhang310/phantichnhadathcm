# ============================================================
# LUACHONNHADAT SCRAPER - BDS TP.HCM
# Chay:
#   LUACHON_MAX_PAGES=4 Rscript scripts/scrapers/thu_thap_luachonnhadat.R
# Output:
#   data/raw/luachonnhadat/luachonnhadat_sach.csv
#   data/raw/luachonnhadat/luachonnhadat_schema_chuan.csv
# ============================================================

source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()

required_packages <- c("httr", "jsonlite", "dplyr", "purrr", "readr", "stringr", "lubridate", "rvest", "xml2", "tibble")
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
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(lubridate)
library(rvest)

# Hàm %||%: hỗ trợ xử lý dữ liệu trong script.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

CFG_LCN <- list(
  api_url = "https://luachonnhadat.vn/api/product/loadmore",
  api_key = Sys.getenv(
    "LUACHON_API_KEY",
    "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJoZWFkZXIiOnsiaWQiOiJsdWFjaG9ubmhhZGF0LnZuIiwidXNlciI6IjJkN2EzODY2ZjM4ZmM4ODliNjVhYjM4NjE1OTAxNmYyIn19.IpB1QKr0dcfj5uMfL_eX68xlzMZCqMcN9l6pgyZROR4"
  ),
  city_id = "1",
  max_pages = as.integer(Sys.getenv("LUACHON_MAX_PAGES", "4")),
  page_size = as.integer(Sys.getenv("LUACHON_PAGE_SIZE", "100")),
  delay_min = as.numeric(Sys.getenv("LUACHON_DELAY_MIN", "0.4")),
  delay_max = as.numeric(Sys.getenv("LUACHON_DELAY_MAX", "1.1")),
  fetch_details = identical(Sys.getenv("LUACHON_FETCH_DETAILS", "0"), "1"),
  output_dir = dirname(PATHS$luachon_assignment_csv),
  source_dir = dirname(PATHS$luachon_raw_csv),
  clean_csv = basename(PATHS$luachon_assignment_csv),
  standard_csv = basename(PATHS$luachon_raw_csv)
)

LCN_GROUPS <- tibble::tribble(
  ~source_group, ~t_id, ~loai_gd, ~is_rent,
  "sale", "1", "ban", 0L,
  "rent", "2", "thue", 1L
)

UA_POOL <- c(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/124.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/124.0.0.0 Safari/537.36",
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 Chrome/124.0.0.0 Safari/537.36"
)

# Hàm strip_html: loại dấu tiếng Việt để so khớp văn bản.
strip_html <- function(x) {
  x <- as.character(x %||% NA_character_)
  out <- x %>%
    str_replace_all("<[^>]+>", " ") %>%
    str_replace_all("&nbsp;", " ") %>%
    str_squish()
  if_else(is.na(x) | x == "", NA_character_, out)
}

# Hàm normalize_url: phân tích chuỗi đầu vào thành giá trị chuẩn.
normalize_url <- function(url) {
  url <- as.character(url)
  if_else(
    is.na(url) | url == "",
    NA_character_,
    if_else(str_starts(url, "http"), url, paste0("https://luachonnhadat.vn", url))
  )
}

# Hàm parse_price_vnd: phân tích chuỗi đầu vào thành giá trị chuẩn.
parse_price_vnd <- function(price_raw) {
  text <- str_to_lower(strip_html(price_raw) %||% "")
  if (text == "" || str_detect(text, "thỏa thuận|thoả thuận|liên hệ")) return(NA_real_)

  num <- str_extract(text, "[0-9]+([\\.,][0-9]+)?")
  if (is.na(num)) return(NA_real_)
  value <- as.numeric(str_replace(num, ",", "."))

  multiplier <- dplyr::case_when(
    str_detect(text, "tỷ|ty") ~ 1e9,
    str_detect(text, "triệu|trieu") ~ 1e6,
    str_detect(text, "nghìn|ngan|ngàn") ~ 1e3,
    TRUE ~ 1
  )

  value * multiplier
}

# Hàm parse_area: phân tích chuỗi đầu vào thành giá trị chuẩn.
parse_area <- function(area_raw) {
  text <- str_replace_all(as.character(area_raw %||% ""), ",", ".")
  value <- suppressWarnings(as.numeric(str_extract(text, "[0-9]+(\\.[0-9]+)?")))
  ifelse(is.na(value), NA_real_, value)
}

# Hàm parse_date: phân tích chuỗi đầu vào thành giá trị chuẩn.
parse_date <- function(date_raw) {
  parsed <- suppressWarnings(lubridate::dmy(date_raw))
  if_else(is.na(parsed), NA_character_, as.character(parsed))
}

# Hàm extract_district: chuẩn hóa tên quận huyện.
extract_district <- function(location) {
  location <- strip_html(location)
  first <- str_split_fixed(coalesce(location, ""), ",", 2)[, 1]
  if_else(is.na(location) | location == "", NA_character_, str_squish(first))
}

# Hàm infer_category: phân tích chuỗi đầu vào thành giá trị chuẩn.
infer_category <- function(title, url = NA_character_) {
  title_text <- str_to_lower(title %||% "")
  url_text <- str_to_lower(url %||% "")
  text <- str_c(title_text, " ", url_text)
  dplyr::case_when(
    str_detect(text, "căn hộ dịch vụ|can-ho-dich-vu") ~ "Căn hộ dịch vụ",
    str_detect(text, "shop\\s*house|shop-house") ~ "Shop house",
    str_detect(text, "nhà xưởng|nha-xuong|kho xưởng") ~ "Nhà xưởng",
    str_detect(text, "khách sạn|khach-san") ~ "Khách sạn",
    str_detect(text, "biệt thự|biet-thu") ~ "Biệt thự",
    str_detect(text, "chung cư|chung-cu") ~ "Chung cư",
    str_detect(text, "căn hộ|can-ho") ~ "Căn hộ",
    str_detect(text, "phòng|phong") ~ "Phòng cho thuê",
    str_detect(text, "mặt bằng|mat-bang") ~ "Mặt bằng",
    str_detect(title_text, "\\bđất\\b") | str_detect(url_text, "/(can-ban|cho-thue)-dat") ~ "Đất",
    str_detect(text, "nhà|nha") ~ "Nhà phố",
    TRUE ~ "Bất động sản"
  )
}

# Hàm extract_address_from_text: phân tích chuỗi đầu vào thành giá trị chuẩn.
extract_address_from_text <- function(text) {
  text <- strip_html(text)
  found <- str_match(text, "(?i)(vị trí|vi tri|địa chỉ|dia chi)\\s*:?\\s*([^\\n\\r\\.;]+(?:,\\s*[^\\n\\r\\.;]+){1,5})")[, 3]
  found <- str_squish(found)
  looks_like_address <- str_detect(
    str_to_lower(coalesce(found, "")),
    "đường|duong|phường|phuong|\\bp\\.|quận|quan|\\bq\\.|huyện|huyen|tp\\.?|hcm|hồ chí minh|ho chi minh"
  )
  if_else(is.na(found) | !looks_like_address, NA_character_, found)
}

# Hàm fetch_detail: nạp dữ liệu từ file, API hoặc cache.
fetch_detail <- function(url) {
  if (!CFG_LCN$fetch_details || is.na(url)) {
    return(list(address = NA_character_, category = NA_character_, description = NA_character_))
  }

  Sys.sleep(runif(1, CFG_LCN$delay_min, CFG_LCN$delay_max))
  res <- tryCatch(
    RETRY(
      "GET",
      url,
      add_headers(
        "User-Agent" = sample(UA_POOL, 1),
        "Accept-Language" = "vi-VN,vi;q=0.9,en;q=0.8"
      ),
      timeout(20),
      times = 2,
      pause_base = 1,
      terminate_on = c(404)
    ),
    error = function(e) NULL
  )
  if (is.null(res) || status_code(res) != 200) {
    return(list(address = NA_character_, category = NA_character_, description = NA_character_))
  }

  html <- content(res, as = "text", encoding = "UTF-8")
  doc <- read_html(html)
  selected_cat <- doc %>%
    html_element("select[name='catID'] option[selected]") %>%
    html_text2()

  list(
    address = doc %>% html_element(".vnt-pro-detail .i-address") %>% html_text2(),
    category = ifelse(length(selected_cat) == 0 || is.na(selected_cat), NA_character_, selected_cat),
    description = doc %>% html_element(".vnt-pro-detail .i-content") %>% html_text2()
  )
}

# Hàm fetch_listing_page: nạp dữ liệu từ file, API hoặc cache.
fetch_listing_page <- function(meta, page) {
  Sys.sleep(runif(1, CFG_LCN$delay_min, CFG_LCN$delay_max))
  message("Luachonnhadat: ", meta$loai_gd, " / trang ", page)

  res <- tryCatch(
    RETRY(
      "GET",
      CFG_LCN$api_url,
      query = list(
        page = page,
        page_size = CFG_LCN$page_size,
        tID = meta$t_id,
        city = CFG_LCN$city_id
      ),
      add_headers(
        "Authorization" = paste("Bearer", CFG_LCN$api_key),
        "User-Agent" = sample(UA_POOL, 1),
        "Accept" = "application/json, text/plain, */*",
        "Referer" = "https://luachonnhadat.vn/",
        "Accept-Language" = "vi-VN,vi;q=0.9,en;q=0.8"
      ),
      timeout(20),
      times = 3,
      pause_base = 1,
      terminate_on = c(401, 403, 404)
    ),
    error = function(e) NULL
  )

  if (is.null(res) || status_code(res) != 200) return(tibble())

  parsed <- tryCatch(fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE), error = function(e) NULL)
  items <- parsed$data$items %||% NULL
  if (is.null(items) || !is.data.frame(items) || nrow(items) == 0) return(tibble())

  tibble(items) %>%
    mutate(
      source = "luachonnhadat",
      source_group = meta$source_group,
      loai_gd = meta$loai_gd,
      is_rent = meta$is_rent,
      page_fetched = page,
      scraped_at = as.character(Sys.time())
    )
}

# Hàm standardize_luachon: làm sạch và chuẩn hóa dữ liệu nguồn.
standardize_luachon <- function(raw_df) {
  if (is.null(raw_df) || nrow(raw_df) == 0) return(tibble())

  detail_data <- map(raw_df$link, fetch_detail)
  detail_address <- map_chr(detail_data, "address")
  detail_category <- map_chr(detail_data, "category")
  detail_desc <- map_chr(detail_data, "description")

  raw_df %>%
    mutate(
      url = normalize_url(link),
      tieu_de = str_squish(as.character(title)),
      gia_raw = strip_html(coalesce(as.character(price_display), as.character(text_price))),
      dien_tich_raw = if_else(!is.na(total_area_used), paste0(total_area_used, " m²"), NA_character_),
      mo_ta = coalesce(strip_html(detail_desc), strip_html(short)),
      dia_chi = coalesce(str_squish(detail_address), strip_html(location)),
      quan_huyen = extract_district(location),
      loai_bds = coalesce(str_squish(detail_category), infer_category(tieu_de, url)),
      nguon = "luachonnhadat.vn",
      ngay_dang = parse_date(date_update),
      gia = map_dbl(gia_raw, parse_price_vnd),
      dien_tich = parse_area(total_area_used),
      lien_he = as.character(contact_mobile),
      image = normalize_url(src),
      source_id = paste0("luachonnhadat_", p_id),
      ad_id = source_id
    )
}

# Hàm clean_luachon: làm sạch và chuẩn hóa dữ liệu nguồn.
clean_luachon <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(tibble())

  df %>%
    mutate(
      across(c(tieu_de, gia_raw, dien_tich_raw, dia_chi, quan_huyen, loai_bds, loai_gd, url, nguon), ~ str_squish(as.character(.x))),
      dia_chi = if_else(is.na(dia_chi) | dia_chi == "", quan_huyen, dia_chi),
      loai_bds = if_else(is.na(loai_bds) | loai_bds == "", "Bất động sản", loai_bds)
    ) %>%
    filter(!is.na(tieu_de), tieu_de != "", !is.na(url), url != "") %>%
    filter(!is.na(gia_raw), gia_raw != "", !is.na(dien_tich_raw), dien_tich_raw != "") %>%
    filter(!is.na(quan_huyen), quan_huyen != "", str_detect(str_to_lower(strip_html(location)), "hồ chí minh|ho chi minh|hcm")) %>%
    filter(!is.na(gia), gia > 0) %>%
    filter(
      (is_rent == 1L & gia >= 300000 & gia <= 2e9) |
        (is_rent == 0L & gia >= 300000000 & gia <= 500e9)
    ) %>%
    filter(is.na(dien_tich) | (dien_tich >= 5 & dien_tich <= 10000)) %>%
    arrange(desc(scraped_at)) %>%
    distinct(url, .keep_all = TRUE)
}

# Hàm to_standard_schema: hỗ trợ xử lý dữ liệu trong script.
to_standard_schema <- function(df) {
  df %>%
    transmute(
      source = "luachonnhadat",
      source_group = source_group,
      source_id = source_id,
      ad_id = ad_id,
      title = tieu_de,
      price = gia,
      price_str = gia_raw,
      area = dien_tich,
      rooms = NA_real_,
      address = dia_chi,
      ward = "Không rõ",
      district_id = NA_character_,
      district_name = quan_huyen,
      category_id = NA_character_,
      category_name = loai_bds,
      lat = NA_real_,
      lon = NA_real_,
      image = image,
      ad_url = url,
      source_url = url,
      posted_at = ngay_dang,
      scraped_at = scraped_at,
      page_fetched = page_fetched,
      price_m = price / 1e6,
      price_per_m2 = if_else(!is.na(area) & area > 0, price / area, NA_real_),
      has_coord = 0L,
      is_rent = is_rent
    )
}

# Hàm to_assignment_schema: hỗ trợ xử lý dữ liệu trong script.
to_assignment_schema <- function(df) {
  df %>%
    transmute(
      tieu_de,
      gia_raw,
      dien_tich_raw,
      dia_chi,
      quan_huyen,
      loai_bds,
      loai_gd,
      url,
      nguon,
      ngay_dang,
      gia_vnd = gia,
      dien_tich_m2 = dien_tich,
      lien_he,
      mo_ta
    )
}

# Hàm run_luachon_scrape: chạy toàn bộ bước xử lý chính.
run_luachon_scrape <- function(max_pages = CFG_LCN$max_pages) {
  dir.create(CFG_LCN$output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(CFG_LCN$source_dir, recursive = TRUE, showWarnings = FALSE)

  raw <- purrr::pmap_dfr(
    expand.grid(group_idx = seq_len(nrow(LCN_GROUPS)), page = seq_len(max_pages)),
    function(group_idx, page) fetch_listing_page(LCN_GROUPS[group_idx, ], page)
  )

  if (nrow(raw) == 0) {
    stop("Khong lay duoc listing nao tu luachonnhadat.vn. Kiem tra ket noi mang/API key roi chay lai.")
  }

  clean <- raw %>%
    standardize_luachon() %>%
    clean_luachon()

  if (nrow(clean) == 0) {
    stop("Da goi API nhung khong con dong nao sau buoc clean. Kiem tra filter TP.HCM/gia/dien tich.")
  }

  assignment_df <- to_assignment_schema(clean)
  standard_df <- to_standard_schema(clean)

  clean_out <- file.path(CFG_LCN$output_dir, CFG_LCN$clean_csv)
  standard_out <- file.path(CFG_LCN$source_dir, CFG_LCN$standard_csv)

  write_csv(assignment_df, clean_out)
  write_csv(standard_df, standard_out)

  message("Da luu ", nrow(assignment_df), " dong vao ", clean_out)
  print(assignment_df %>% count(loai_gd, name = "so_tin"))
  message("Da luu schema noi bo vao ", standard_out)
  invisible(assignment_df)
}

if (sys.nframe() == 0) {
  run_luachon_scrape()
}
