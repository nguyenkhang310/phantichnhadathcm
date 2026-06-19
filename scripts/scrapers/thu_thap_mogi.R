#!/usr/bin/env Rscript

# ============================================================
# MOGI SCRAPER - uu tien bo sung tin thue TP.HCM
# Chay nhanh listing-only:
#   MOGI_MAX_PAGES=80 Rscript scripts/scrapers/thu_thap_mogi.R
# Bo sung tiep vao file crawl hien co:
#   MOGI_START_PAGE=121 MOGI_MAX_PAGES=125 MOGI_APPEND_EXISTING=1 Rscript scripts/scrapers/thu_thap_mogi.R
# Output:
#   data/raw/mogi/mogi_sach_crawl.csv
# ============================================================

source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()
source(PATHS$district_normalization_script)

required_packages <- c("dplyr", "httr", "lubridate", "purrr", "readr", "rvest", "stringr", "xml2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Thieu package: ", paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(httr)
library(lubridate)
library(purrr)
library(readr)
library(rvest)
library(stringr)
library(xml2)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

CFG_MOGI <- list(
  start_page = as.integer(Sys.getenv("MOGI_START_PAGE", "1")),
  max_pages = as.integer(Sys.getenv("MOGI_MAX_PAGES", "60")),
  groups = Sys.getenv("MOGI_GROUPS", "rent"),
  fetch_details = identical(Sys.getenv("MOGI_FETCH_DETAILS", "0"), "1"),
  append_existing = identical(Sys.getenv("MOGI_APPEND_EXISTING", "0"), "1"),
  delay_min = as.numeric(Sys.getenv("MOGI_DELAY_MIN", "0.25")),
  delay_max = as.numeric(Sys.getenv("MOGI_DELAY_MAX", "0.70")),
  output_csv = PATHS$mogi_scraped_csv
)

MOGI_OUTPUT_COLS <- c(
  "tieu_de", "gia_raw", "dien_tich_raw", "dia_chi", "quan_huyen", "loai_bds", "loai_gd",
  "url", "nguon", "so_phong_ngu", "gia_vnd", "dien_tich_m2", "ngay_dang", "phap_ly",
  "id_nguon", "lat", "lon"
)

MOGI_UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/120.0 Safari/537.36"

MOGI_SOURCES <- tibble::tribble(
  ~group, ~source_group, ~category_hint, ~base_url,
  "rent", "thue", "Căn hộ", "https://mogi.vn/ho-chi-minh/thue-can-ho-chung-cu",
  "rent", "thue", "Nhà phố", "https://mogi.vn/ho-chi-minh/thue-nha-mat-tien-pho",
  "rent", "thue", "Nhà phố", "https://mogi.vn/ho-chi-minh/thue-nha-hem-ngo",
  "rent", "thue", "Phòng/Cho thuê", "https://mogi.vn/ho-chi-minh/thue-phong-tro-khu-nha-tro",
  "rent", "thue", "Văn phòng, Mặt bằng kinh doanh", "https://mogi.vn/ho-chi-minh/thue-van-phong-toa-nha-cao-oc",
  "rent", "thue", "Văn phòng, Mặt bằng kinh doanh", "https://mogi.vn/ho-chi-minh/thue-mat-bang-cua-hang-shop-nhieu-muc-dich",
  "rent", "thue", "Biệt thự", "https://mogi.vn/ho-chi-minh/thue-nha-biet-thu-lien-ke",
  "sale", "ban", "Bất động sản khác", "https://mogi.vn/ho-chi-minh/mua-nha-dat"
)

normalize_text <- function(x) {
  if (length(x) == 0) return(NA_character_)
  x <- iconv(as.character(x), from = "UTF-8", to = "UTF-8", sub = "")
  x <- str_replace_all(x, "[\r\n\t]+", " ")
  x <- str_squish(x)
  x[x == ""] <- NA_character_
  x
}

read_mogi_html <- function(url) {
  Sys.sleep(stats::runif(1, CFG_MOGI$delay_min, CFG_MOGI$delay_max))
  response <- httr::GET(
    url,
    httr::user_agent(MOGI_UA),
    httr::add_headers(
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      Referer = "https://mogi.vn/"
    ),
    httr::timeout(20)
  )
  if (httr::status_code(response) != 200) {
    stop("Khong tai duoc Mogi: ", url, " (HTTP ", httr::status_code(response), ")")
  }
  xml2::read_html(httr::content(response, as = "text", encoding = "UTF-8"))
}

page_url <- function(base_url, page) {
  if (page <= 1) return(base_url)
  paste0(base_url, if (str_detect(base_url, "\\?")) "&" else "?", "cp=", page)
}

guess_category <- function(title, fallback) {
  key <- district_strip_vietnamese(paste(title %||% "", fallback %||% ""))
  dplyr::case_when(
    str_detect(key, "can ho|chung cu|officetel|studio") ~ "Căn hộ",
    str_detect(key, "phong|nha tro|kytuc|ky tuc") ~ "Phòng/Cho thuê",
    str_detect(key, "van phong|mat bang|shop|shophouse|kho|xuong") ~ "Văn phòng, Mặt bằng kinh doanh",
    str_detect(key, "biet thu|villa") ~ "Biệt thự",
    str_detect(key, "dat|nen") ~ "Đất nền",
    str_detect(key, "nha|pho") ~ "Nhà phố",
    TRUE ~ fallback
  )
}

extract_district <- function(text) {
  text <- normalize_text(text)
  if (is.na(text)) return(NA_character_)
  district <- str_extract(text, regex("Quận\\s*[^,]+|Huyện\\s*[^,]+|TP\\.?\\s*Thủ Đức|Thủ Đức|Bình Tân|Bình Thạnh|Gò Vấp|Tân Bình|Tân Phú|Phú Nhuận", ignore_case = TRUE))
  normalize_text(district)
}

parse_price_vnd <- function(x) {
  key <- district_strip_vietnamese(normalize_text(x))
  value <- suppressWarnings(as.numeric(str_replace(str_extract(key, "[0-9]+([\\.,][0-9]+)?"), ",", ".")))
  if (is.na(value)) return(NA_real_)
  if (str_detect(key, "ty|ti")) value * 1e9
  else if (str_detect(key, "trieu|tr\\b")) value * 1e6
  else if (str_detect(key, "nghin|ngan|k\\b")) value * 1e3
  else value
}

parse_area_m2 <- function(x) {
  value <- suppressWarnings(as.numeric(str_replace(str_extract(normalize_text(x), "[0-9]+([\\.,][0-9]+)?"), ",", ".")))
  ifelse(is.na(value) | value <= 0, NA_real_, value)
}

parse_rooms <- function(x) {
  value <- suppressWarnings(as.numeric(str_extract(district_strip_vietnamese(normalize_text(x)), "[0-9]+")))
  ifelse(is.na(value) | value < 0 | value > 50, NA_real_, value)
}

extract_id_from_url <- function(url) {
  id <- str_match(url, "id([0-9]+)")[, 2]
  ifelse(is.na(id), url, id)
}

parse_listing_node <- function(node, meta, source_url) {
  title <- normalize_text(html_text(html_node(node, ".prop-title"), trim = TRUE))
  href <- html_attr(html_node(node, "a.link-overlay"), "href")
  if (is.na(href) || href == "") href <- html_attr(html_node(node, "a[href*='-id']"), "href")
  if (!is.na(href) && !str_starts(href, "http")) href <- xml2::url_absolute(href, source_url)

  price_raw <- normalize_text(html_text(html_node(node, ".price"), trim = TRUE))
  address <- normalize_text(html_text(html_node(node, ".prop-addr"), trim = TRUE))
  attrs <- normalize_text(html_text(html_nodes(node, ".prop-attr li, .prop-attr div"), trim = TRUE))
  attrs <- attrs[!is.na(attrs)]
  area_raw <- attrs[str_detect(attrs, regex("m2|m²", ignore_case = TRUE))][1] %||% NA_character_
  rooms_raw <- attrs[str_detect(attrs, regex("phòng|phong|pn", ignore_case = TRUE))][1] %||% NA_character_

  tibble(
    tieu_de = title,
    gia_raw = price_raw,
    dien_tich_raw = normalize_text(area_raw),
    dia_chi = address,
    quan_huyen = extract_district(address),
    loai_bds = guess_category(title, meta$category_hint),
    loai_gd = meta$source_group,
    url = normalize_text(href),
    nguon = "mogi.vn",
    so_phong_ngu = parse_rooms(rooms_raw),
    gia_vnd = parse_price_vnd(price_raw),
    dien_tich_m2 = parse_area_m2(area_raw),
    ngay_dang = as.character(Sys.Date()),
    phap_ly = NA_character_,
    id_nguon = normalize_text(extract_id_from_url(href)),
    lat = NA_real_,
    lon = NA_real_
  )
}

parse_listing_page <- function(url, meta) {
  doc <- read_mogi_html(url)
  nodes <- html_nodes(doc, ".props li")
  if (length(nodes) == 0) nodes <- html_nodes(doc, "li")
  if (length(nodes) == 0) return(tibble())

  map_dfr(nodes, parse_listing_node, meta = meta, source_url = url) %>%
    filter(!is.na(url), !is.na(tieu_de), !is.na(gia_vnd), !is.na(dien_tich_m2))
}

format_mogi_output <- function(df) {
  missing_cols <- setdiff(MOGI_OUTPUT_COLS, names(df))
  for (col in missing_cols) df[[col]] <- NA

  df %>%
    transmute(
      tieu_de = as.character(tieu_de),
      gia_raw = as.character(gia_raw),
      dien_tich_raw = as.character(dien_tich_raw),
      dia_chi = as.character(dia_chi),
      quan_huyen = as.character(quan_huyen),
      loai_bds = as.character(loai_bds),
      loai_gd = as.character(loai_gd),
      url = as.character(url),
      nguon = as.character(nguon),
      so_phong_ngu = suppressWarnings(as.numeric(so_phong_ngu)),
      gia_vnd = suppressWarnings(as.numeric(gia_vnd)),
      dien_tich_m2 = suppressWarnings(as.numeric(dien_tich_m2)),
      ngay_dang = as.character(ngay_dang),
      phap_ly = as.character(phap_ly),
      id_nguon = as.character(id_nguon),
      lat = suppressWarnings(as.numeric(lat)),
      lon = suppressWarnings(as.numeric(lon))
    )
}

parse_detail_page <- function(url) {
  doc <- read_mogi_html(url)
  address <- normalize_text(html_text(html_node(doc, ".address"), trim = TRUE))
  info <- html_nodes(doc, ".info-attrs .info-attr")
  pairs <- map_dfr(info, function(node) {
    values <- normalize_text(html_text(html_nodes(node, "span"), trim = TRUE))
    if (length(values) < 2) return(tibble(label = NA_character_, value = NA_character_))
    tibble(label = values[1], value = values[2])
  })
  detail_value <- function(pattern) {
    row <- pairs %>% filter(str_detect(label, regex(pattern, ignore_case = TRUE)))
    if (nrow(row) == 0) NA_character_ else row$value[1]
  }

  iframe <- html_node(doc, ".map-content iframe, iframe")
  iframe_url <- html_attr(iframe, "data-src") %||% html_attr(iframe, "src")
  coords <- str_match(iframe_url %||% "", "[?&]q=([0-9\\.-]+),([0-9\\.-]+)")

  tibble(
    url = url,
    dia_chi_detail = address,
    ngay_dang_detail = detail_value("ngày đăng"),
    phap_ly_detail = detail_value("pháp lý"),
    id_nguon_detail = detail_value("mã"),
    lat_detail = suppressWarnings(as.numeric(coords[, 2])),
    lon_detail = suppressWarnings(as.numeric(coords[, 3]))
  )
}

select_sources <- function(groups) {
  groups <- str_split(groups, ",", simplify = FALSE)[[1]] %>% str_squish()
  if ("all" %in% groups) return(MOGI_SOURCES)
  MOGI_SOURCES %>% filter(group %in% groups)
}

run_mogi_scrape <- function(
    start_page = CFG_MOGI$start_page,
    max_pages = CFG_MOGI$max_pages,
    groups = CFG_MOGI$groups,
    fetch_details = CFG_MOGI$fetch_details,
    append_existing = CFG_MOGI$append_existing,
    output_csv = CFG_MOGI$output_csv) {
  dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
  sources <- select_sources(groups)
  if (nrow(sources) == 0) stop("Khong co nhom Mogi hop le: ", groups)
  start_page <- max(as.integer(start_page), 1L)
  max_pages <- as.integer(max_pages)
  if (is.na(max_pages) || max_pages < start_page) {
    stop("MOGI_MAX_PAGES phai >= MOGI_START_PAGE.")
  }

  listings <- list()
  for (i in seq_len(nrow(sources))) {
    meta <- sources[i, ]
    seen_source_urls <- character()
    for (page in seq.int(start_page, max_pages)) {
      url <- page_url(meta$base_url, page)
      message("Mogi: ", meta$source_group, " / ", meta$category_hint, " / trang ", page)
      page_data <- tryCatch(
        parse_listing_page(url, meta),
        error = function(e) {
          message("Bo qua trang Mogi ", url, ": ", conditionMessage(e))
          tibble()
        }
      )
      if (nrow(page_data) == 0) break

      page_urls <- unique(tolower(str_replace(page_data$url, "/+$", "")))
      new_urls <- setdiff(page_urls, seen_source_urls)
      if (length(new_urls) == 0) {
        message("Dung nhom Mogi ", meta$category_hint, ": trang ", page, " khong co URL moi.")
        break
      }
      seen_source_urls <- unique(c(seen_source_urls, new_urls))
      listings[[length(listings) + 1L]] <- page_data
    }
  }

  clean <- bind_rows(listings) %>%
    distinct(url, .keep_all = TRUE)

  if (nrow(clean) == 0) {
    stop("Khong lay duoc dong Mogi nao.")
  }

  if (fetch_details) {
    details <- map_dfr(clean$url, function(url) {
      message("Mogi detail: ", url)
      tryCatch(parse_detail_page(url), error = function(e) tibble(url = url))
    })
    clean <- clean %>%
      left_join(details, by = "url") %>%
      mutate(
        dia_chi = coalesce(dia_chi_detail, dia_chi),
        ngay_dang = coalesce(ngay_dang_detail, ngay_dang),
        phap_ly = coalesce(phap_ly_detail, phap_ly),
        id_nguon = coalesce(id_nguon_detail, id_nguon),
        lat = coalesce(lat_detail, lat),
        lon = coalesce(lon_detail, lon)
      ) %>%
      select(names(bind_rows(listings)))
  }

  clean <- clean %>%
    mutate(
      quan_huyen = canonical_hcmc_district(quan_huyen, dia_chi, tieu_de, url),
      loai_gd = if_else(loai_gd == "thue", "thue", "ban")
    ) %>%
    filter(
      !is.na(tieu_de), !is.na(url), !is.na(gia_vnd), !is.na(dien_tich_m2),
      (loai_gd == "thue" & gia_vnd >= 300000 & gia_vnd <= 2e9) |
        (loai_gd == "ban" & gia_vnd >= 300000000 & gia_vnd <= 500e9),
      dien_tich_m2 >= 5, dien_tich_m2 <= 5000
    ) %>%
    select(all_of(MOGI_OUTPUT_COLS)) %>%
    format_mogi_output()

  if (append_existing && file.exists(output_csv)) {
    existing <- read_csv(output_csv, show_col_types = FALSE) %>%
      format_mogi_output()
    clean <- bind_rows(clean, existing) %>%
      distinct(url, .keep_all = TRUE)
  }

  write_csv(clean, output_csv, na = "")
  message("Da luu ", nrow(clean), " dong Mogi crawl vao ", output_csv)
  print(clean %>% count(loai_gd, name = "rows"))
  invisible(clean)
}

if (sys.nframe() == 0) {
  run_mogi_scrape()
}
