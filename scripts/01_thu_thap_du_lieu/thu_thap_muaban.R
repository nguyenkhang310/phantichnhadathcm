source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()
source(PATHS$data_standardization_script)

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
library(tibble)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

CFG_MUA <- list(
  max_pages = as.integer(Sys.getenv("MUABAN_MAX_PAGES", "3")),
  delay_min = as.numeric(Sys.getenv("MUABAN_DELAY_MIN", "1.0")),
  delay_max = as.numeric(Sys.getenv("MUABAN_DELAY_MAX", "2.2")),
  browser_wait = as.numeric(Sys.getenv("MUABAN_BROWSER_WAIT", "8")),
  use_browser = !identical(Sys.getenv("MUABAN_USE_BROWSER", "1"), "0"),
  output_dir = dirname(PATHS$muaban_raw_csv),
  csv_file = basename(PATHS$muaban_raw_csv)
)

MUABAN_SOURCES <- tibble::tribble(
  ~source_group, ~category_id, ~category_name, ~is_rent, ~base_url,
  "sale_all", "1020", "Nhà đất", FALSE, "https://muaban.net/bat-dong-san/ban-nha-dat-chung-cu-ho-chi-minh",
  "rent_all", "1030", "Nhà đất cho thuê", TRUE, "https://muaban.net/bat-dong-san/cho-thue-nha-dat-ho-chi-minh"
)

UA_POOL <- c(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/124.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/124.0.0.0 Safari/537.36",
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 Chrome/124.0.0.0 Safari/537.36"
)

make_page_url <- function(base_url, page) {
  if (page <= 1) return(base_url)
  paste0(base_url, "?page=", page)
}

normalize_url <- function(path) {
  if (is.na(path) || path == "") return(NA_character_)
  if (str_starts(path, "http")) path else paste0("https://muaban.net", path)
}

is_cloudflare_challenge <- function(html) {
  str_detect(html, "Just a moment|challenge-platform|__cf_chl|Enable JavaScript and cookies")
}

render_html_chromote <- function(url, wait_seconds = CFG_MUA$browser_wait) {
  if (!CFG_MUA$use_browser || !requireNamespace("chromote", quietly = TRUE)) {
    return(NULL)
  }

  message("Muaban: thu render bang Chrome headless...")
  session <- chromote::ChromoteSession$new()
  tryCatch({
    session$Page$enable()
    session$Network$enable()
    session$Network$setUserAgentOverride(
      userAgent = sample(UA_POOL, 1),
      acceptLanguage = "vi-VN,vi;q=0.9,en;q=0.8"
    )
    session$Page$navigate(url)
    tryCatch(session$Page$loadEventFired(timeout_ = 20), error = function(e) NULL)
    Sys.sleep(wait_seconds)
    html <- session$Runtime$evaluate(
      "document.documentElement.outerHTML",
      returnByValue = TRUE
    )$result$value
    if (is.null(html) || is.na(html) || html == "") NULL else html
  }, error = function(e) {
    message("Chrome render loi: ", conditionMessage(e))
    NULL
  }, finally = {
    try(session$close(), silent = TRUE)
  })
}

safe_text <- function(node) {
  value <- node %>% html_text2()
  if (length(value) == 0 || is.na(value)) "" else value
}

strip_vietnamese <- function(x) {
  x <- str_to_lower(as.character(x))
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
    x <- str_replace_all(x, replacements[[replacement]], replacement)
  }
  x
}

parse_price_vnd <- function(price_raw, is_rent = FALSE) {
  if (length(price_raw) == 0 || is.na(price_raw) || price_raw == "") return(NA_real_)
  text <- strip_vietnamese(price_raw)
  if (text == "" || str_detect(text, "thoa thuan|lien he")) return(NA_real_)

  value_text <- str_extract(text, "[0-9]+([\\.,][0-9]+)?")
  if (is.na(value_text)) return(NA_real_)
  value <- suppressWarnings(as.numeric(str_replace(value_text, ",", ".")))
  if (is.na(value)) return(NA_real_)

  multiplier <- dplyr::case_when(
    str_detect(text, "ty|ti") ~ 1e9,
    str_detect(text, "trieu|tr") ~ 1e6,
    str_detect(text, "nghin|ngan") ~ 1e3,
    TRUE ~ if (is_rent && value < 1000) 1e6 else 1
  )

  value * multiplier
}

parse_area_m2 <- function(text) {
  text <- str_replace_all(as.character(text %||% ""), ",", ".")
  value <- str_match(text, "([0-9]+(?:\\.[0-9]+)?)\\s*(?:m²|m2|m\\b)")[, 2]
  value <- suppressWarnings(as.numeric(value))
  ifelse(is.na(value), NA_real_, value)
}

parse_rooms <- function(text) {
  value <- str_match(str_to_upper(as.character(text %||% "")), "([0-9]+)\\s*PN")[, 2]
  value <- suppressWarnings(as.numeric(value))
  ifelse(is.na(value), NA_real_, value)
}

parse_posted_at <- function(label) {
  if (length(label) == 0 || is.na(label) || label == "") return(as.character(Sys.Date()))
  label <- str_to_lower(str_squish(as.character(label)))
  today <- Sys.Date()
  if (label == "" || str_detect(label, "hom nay|hôm nay|phut truoc|phút trước|gio truoc|giờ trước")) {
    return(as.character(today))
  }
  if (str_detect(label, "hom qua|hôm qua")) return(as.character(today - 1))
  parsed <- suppressWarnings(dmy(str_extract(label, "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}")))
  if (is.na(parsed)) as.character(today) else as.character(parsed)
}

extract_district <- function(address) {
  if (is.na(address) || address == "") return("Không rõ")
  parts <- str_split(address, ",")[[1]] %>% str_squish()
  district <- parts[str_detect(parts, "Quận|Huyện|TP\\. Thủ Đức|Thủ Đức|Bình Tân|Bình Thạnh|Gò Vấp|Tân Bình|Tân Phú|Phú Nhuận")]
  if (length(district) == 0) return("Không rõ")
  district[[length(district)]]
}

extract_ward <- function(address) {
  if (is.na(address) || address == "") return("Không rõ")
  parts <- str_split(address, ",")[[1]] %>% str_squish()
  ward <- parts[str_detect(parts, "Phường|Xã|Thị trấn")]
  if (length(ward) == 0) return("Không rõ")
  ward[[1]]
}

infer_category <- function(title, text, is_rent) {
  full <- strip_vietnamese(str_c(title, " ", text))
  dplyr::case_when(
    str_detect(full, "can ho|chung cu|apartment") ~ "Căn hộ/Chung cư",
    str_detect(full, "dat|nen") ~ "Đất",
    str_detect(full, "phong tro|nha tro") ~ "Phòng trọ",
    str_detect(full, "van phong|mat bang|cua hang|shop") ~ "Văn phòng/Mặt bằng",
    str_detect(full, "xuong|kho") ~ "Kho xưởng",
    is_rent ~ "Nhà đất cho thuê",
    TRUE ~ "Nhà ở"
  )
}

digest_text <- function(x) {
  paste0(abs(sum(utf8ToInt(x %||% ""))), "_", nchar(x %||% ""))
}

extract_muaban_id <- function(url) {
  id <- str_match(url %||% "", "([0-9]+)(?:\\.htm|$|\\?)")[, 2]
  if (is.na(id) || id == "") digest_text(url) else id
}

looks_like_listing_href <- function(href) {
  href <- as.character(href)
  !is.na(href) &
    str_detect(href, "^/|^https://muaban\\.net") &
    str_detect(href, "bat-dong-san") &
    str_detect(href, "id[0-9]{5,}")
}

html_has_muaban_listing <- function(html) {
  if (is.null(html) || is.na(html) || html == "") return(FALSE)
  doc <- tryCatch(read_html(html), error = function(e) NULL)
  if (is.null(doc)) return(FALSE)
  hrefs <- doc %>% html_elements("a[href]") %>% html_attr("href")
  sum(looks_like_listing_href(hrefs), na.rm = TRUE) > 0
}

candidate_card <- function(anchor) {
  node <- anchor
  for (i in seq_len(7)) {
    text <- safe_text(node)
    if (str_detect(text, "m²|m2") &&
        str_detect(strip_vietnamese(text), "ty|trieu|thoa thuan") &&
        str_length(text) < 3500) {
      return(node)
    }
    parent <- xml2::xml_parent(node)
    if (is.na(xml2::xml_name(parent))) break
    node <- parent
  }
  anchor
}

parse_listing_anchor <- function(anchor, meta, page_url, page) {
  href <- normalize_url(anchor %>% html_attr("href"))
  title <- clean_listing_title(anchor %>% html_text2())
  card <- candidate_card(anchor)
  card_text <- safe_text(card)
  lines <- str_split(card_text, "\\n+")[[1]] %>%
    str_squish() %>%
    .[. != ""]

  price_line <- lines[str_detect(strip_vietnamese(lines), "thoa thuan|[0-9].*(ty|trieu|nghin|/thang)")]
  area_line <- lines[str_detect(lines, "m²|m2")]
  date_line <- lines[str_detect(lines, "Hôm nay|hôm nay|Hôm qua|hôm qua|phút trước|giờ trước|[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}")]
  address_line <- lines[str_detect(lines, "TP\\.HCM|TPHCM|Hồ Chí Minh|Ho Chi Minh")]

  price_str <- if (length(price_line) > 0) price_line[[1]] else NA_character_
  area_str <- if (length(area_line) > 0) area_line[[1]] else NA_character_
  address <- if (length(address_line) > 0) address_line[[1]] else "TP.HCM"
  posted_label <- if (length(date_line) > 0) date_line[[1]] else NA_character_

  description <- lines[
    !lines %in% c(title, price_str, area_str, address, posted_label) &
      str_length(lines) > 30
  ]
  description <- if (length(description) > 0) description[[1]] else NA_character_

  category_name <- infer_category(title, card_text, meta$is_rent)
  category_id <- dplyr::case_when(
    identical(category_name, "Căn hộ/Chung cư") ~ "1010",
    category_name %in% c("Văn phòng/Mặt bằng", "Kho xưởng") ~ "1030",
    identical(category_name, "Đất") ~ "1040",
    identical(category_name, "Phòng trọ") ~ "1050",
    TRUE ~ meta$category_id
  )

  price <- parse_price_vnd(price_str, meta$is_rent)
  area <- parse_area_m2(area_str %||% card_text)
  rooms <- parse_rooms(area_str %||% card_text)

  tibble(
    source = "muaban",
    source_group = meta$source_group,
    source_id = paste0("muaban_", extract_muaban_id(href)),
    ad_id = paste0("muaban_", extract_muaban_id(href)),
    title = clean_listing_title(title),
    price = price,
    price_str = price_str,
    area = area,
    rooms = rooms,
    address = address,
    ward = extract_ward(address),
    district_id = NA_character_,
    district_name = extract_district(address),
    category_id = category_id,
    category_name = category_name,
    lat = NA_real_,
    lon = NA_real_,
    image = NA_character_,
    ad_url = href,
    source_url = page_url,
    posted_at = parse_posted_at(posted_label),
    scraped_at = as.character(Sys.time()),
    page_fetched = page,
    price_m = price / 1e6,
    price_per_m2 = if_else(!is.na(area) & area > 0, price / area, NA_real_),
    has_coord = 0L,
    is_rent = as.integer(meta$is_rent)
  )
}

fetch_html <- function(url) {
  Sys.sleep(runif(1, CFG_MUA$delay_min, CFG_MUA$delay_max))
  res <- tryCatch(
    RETRY(
      "GET",
      url,
      add_headers(
        "User-Agent" = sample(UA_POOL, 1),
        "Accept-Language" = "vi-VN,vi;q=0.9,en;q=0.8",
        "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
      ),
      timeout(25),
      times = 2,
      pause_base = 1,
      terminate_on = c(401, 403, 404)
    ),
    error = function(e) NULL
  )
  if (is.null(res)) return(NULL)

  html <- content(res, as = "text", encoding = "UTF-8")
  if (is_cloudflare_challenge(html)) {
    browser_html <- render_html_chromote(url)
    if (!is.null(browser_html) && (!is_cloudflare_challenge(browser_html) || html_has_muaban_listing(browser_html))) {
      return(read_html(browser_html))
    }
    stop("Muaban.net dang bat Cloudflare challenge cho request tu script/browser. Khong ghi CSV rong; hay thu lai sau hoac dung nguon khac neu van bi chan.")
  }
  if (status_code(res) != 200) return(NULL)
  read_html(html)
}

fetch_source_page <- function(meta, page) {
  page_url <- make_page_url(meta$base_url, page)
  message("Muaban: ", meta$source_group, " / trang ", page)
  doc <- fetch_html(page_url)
  if (is.null(doc)) return(tibble())

  anchors <- doc %>% html_elements("a[href]")
  hrefs <- anchors %>% html_attr("href")
  keep <- looks_like_listing_href(hrefs)
  anchors <- anchors[keep]
  if (length(anchors) == 0) return(tibble())

  map_dfr(anchors, parse_listing_anchor, meta = meta, page_url = page_url, page = page) %>%
    filter(!is.na(title), title != "", !is.na(ad_url), ad_url != "") %>%
    distinct(ad_url, .keep_all = TRUE)
}

clean_muaban <- function(df) {
  if (nrow(df) == 0) return(df)
  df %>%
    mutate(title = clean_listing_title(title)) %>%
    filter(str_detect(address %||% "", "TP\\.HCM|TPHCM|Hồ Chí Minh|Ho Chi Minh")) %>%
    filter(!is.na(price), price > 0, !is.na(area), area >= 5, area <= 5000) %>%
    filter(
      (as.logical(is_rent) & price >= 300000 & price <= 2e9) |
        (!as.logical(is_rent) & price >= 300000000 & price <= 500e9)
    ) %>%
    arrange(desc(scraped_at)) %>%
    distinct(source_id, .keep_all = TRUE) %>%
    standardize_listing_schema(default_source = "muaban")
}

standardize_muaban_types <- function(df) {
  if (nrow(df) == 0) return(df)
  df %>%
    mutate(
      across(any_of(c(
        "source", "source_group", "source_id", "ad_id", "title", "price_str",
        "address", "ward", "district_id", "district_name", "category_id",
        "category_name", "image", "ad_url", "source_url", "posted_at",
        "scraped_at"
      )), as.character),
      across(any_of(c("price", "area", "rooms", "lat", "lon", "price_m", "price_per_m2")), as.numeric),
      across(any_of(c("has_coord", "is_rent", "page_fetched")), as.integer)
    )
}

run_muaban_scrape <- function(max_pages = CFG_MUA$max_pages) {
  dir.create(CFG_MUA$output_dir, recursive = TRUE, showWarnings = FALSE)
  raw <- purrr::pmap_dfr(
    expand.grid(source_idx = seq_len(nrow(MUABAN_SOURCES)), page = seq_len(max_pages)),
    function(source_idx, page) {
      tryCatch(
        fetch_source_page(MUABAN_SOURCES[source_idx, ], page),
        error = function(e) {
          meta <- MUABAN_SOURCES[source_idx, ]
          message("Bo qua Muaban ", meta$source_group, " trang ", page, ": ", conditionMessage(e))
          tibble()
        }
      )
    }
  )
  clean <- clean_muaban(raw) %>% standardize_muaban_types()
  out <- file.path(CFG_MUA$output_dir, CFG_MUA$csv_file)

  if (nrow(clean) == 0) {
    if (file.exists(out)) {
      old <- read_csv(out, show_col_types = FALSE) %>% standardize_muaban_types()
      if (nrow(old) > 0) {
        message("Khong co tin Muaban moi hop le; giu nguyen ", nrow(old), " dong trong ", out)
        return(invisible(old))
      }
    }
    stop("Khong parse duoc listing Muaban hop le. Khong ghi CSV rong; kiem tra selector, Cloudflare hoac URL.")
  }

  clean <- combine_listing_history(clean, out, default_source = "muaban")

  write_listing_csv(clean, out, default_source = "muaban", keep_existing_if_empty = TRUE)
  message("Da luu ", nrow(clean), " dong vao ", out)
  print(clean %>% count(source_group, is_rent, name = "so_tin"))
  invisible(clean)
}

if (sys.nframe() == 0) {
  run_muaban_scrape()
}
