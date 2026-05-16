# ============================================================
# CHOTOT SCRAPER v3 - BDS TP.HCM + SQLite
# Chay: Rscript chotot_scraper_v3.R
# Output: data/hcmc_bds.sqlite va data/hcmc_bds_raw.csv
# ============================================================

if (dir.exists("R_libs")) .libPaths(c(normalizePath("R_libs"), .libPaths()))

required_packages <- c("httr", "jsonlite", "dplyr", "purrr", "furrr", "future",
                       "readr", "lubridate", "DBI", "RSQLite")
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
library(furrr)
library(future)
library(readr)
library(lubridate)
library(DBI)
library(RSQLite)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

CFG <- list(
  max_pages = as.integer(Sys.getenv("CHOTOT_MAX_PAGES", "50")),
  limit = 20,
  delay_min = 0.15,
  delay_max = 0.40,
  n_workers = as.integer(Sys.getenv("CHOTOT_WORKERS", "4")),
  retry_times = 3,
  retry_delay = 2,
  output_dir = "data",
  sqlite_file = "hcmc_bds.sqlite",
  csv_file = "hcmc_bds_raw.csv",
  use_area_filter = identical(Sys.getenv("CHOTOT_USE_AREA_FILTER", "0"), "1")
)

DISTRICTS <- c(
  "13001" = "Quận 1", "13002" = "Quận 2", "13003" = "Quận 3",
  "13004" = "Quận 4", "13005" = "Quận 5", "13006" = "Quận 6",
  "13007" = "Quận 7", "13008" = "Quận 8", "13009" = "Quận 9",
  "13010" = "Quận 10", "13011" = "Quận 11", "13012" = "Quận 12",
  "13013" = "Bình Thạnh", "13014" = "Tân Bình", "13015" = "Tân Phú",
  "13016" = "Phú Nhuận", "13017" = "Gò Vấp", "13018" = "Bình Tân",
  "13019" = "Thủ Đức"
)

CATEGORIES <- c(
  "1010" = "Căn hộ/Chung cư",
  "1020" = "Nhà ở",
  "1030" = "Văn phòng/Mặt bằng",
  "1040" = "Đất",
  "1050" = "Phòng trọ"
)

UA_POOL <- c(
  "Mozilla/5.0 (iPhone; CPU iPhone OS 16_6 like Mac OS X) AppleWebKit/605.1.15 Mobile/15E148",
  "Mozilla/5.0 (Linux; Android 13; Pixel 7) AppleWebKit/537.36 Chrome/114.0.0.0 Mobile Safari/537.36",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/120.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Firefox/121.0"
)

price_ranges <- list(
  "1010" = c(300e3, 200e9),
  "1020" = c(300e3, 300e9),
  "1030" = c(1e6, 500e6),
  "1040" = c(100e6, 500e9),
  "1050" = c(300e3, 50e6)
)

parse_posted_at <- function(date_value, list_time_value) {
  primary <- suppressWarnings(as.numeric(date_value))
  if (!is.na(primary)) return(as.character(as_datetime(primary, origin = "1970-01-01")))

  secondary <- suppressWarnings(as.numeric(list_time_value))
  if (!is.na(secondary)) return(as.character(as_datetime(secondary, origin = "1970-01-01")))

  parsed <- suppressWarnings(as_datetime(date_value))
  if (!is.na(parsed)) return(as.character(parsed))

  as.character(Sys.time())
}

init_db <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS listings (
      ad_id TEXT PRIMARY KEY,
      title TEXT,
      price REAL,
      price_str TEXT,
      area REAL,
      rooms REAL,
      address TEXT,
      ward TEXT,
      district_id TEXT,
      district_name TEXT,
      category_id TEXT,
      category_name TEXT,
      lat REAL,
      lon REAL,
      image TEXT,
      ad_url TEXT,
      posted_at TEXT,
      scraped_at TEXT,
      page_fetched INTEGER,
      price_m REAL,
      price_per_m2 REAL,
      has_coord INTEGER,
      is_rent INTEGER
    )
  ")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_listings_district ON listings(district_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_listings_category ON listings(category_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_listings_price ON listings(price)")
}

fetch_page <- function(district_id = NA_character_, district_name = NA_character_,
                       category_id, category_name, page) {
  Sys.sleep(runif(1, CFG$delay_min, CFG$delay_max))

  attempt <- function() {
    query <- list(
      region_v2 = "13000",
      cg = category_id,
      limit = CFG$limit,
      o = (page - 1) * CFG$limit,
      st = "s,k"
    )
    if (CFG$use_area_filter && !is.na(district_id)) query$area <- district_id

    GET(
      "https://gateway.chotot.com/v1/public/ad-listing",
      query = query,
      add_headers(
        "User-Agent" = sample(UA_POOL, 1),
        "Accept" = "application/json, text/plain, */*",
        "Referer" = "https://www.chotot.com/",
        "Origin" = "https://www.chotot.com"
      ),
      timeout(12)
    )
  }

  res <- NULL
  for (i in seq_len(CFG$retry_times)) {
    tryCatch({
      res <- attempt()
      if (status_code(res) == 200) break
    }, error = function(e) NULL)
    Sys.sleep(CFG$retry_delay * i)
  }

  if (is.null(res) || status_code(res) != 200) return(NULL)

  parsed <- tryCatch(content(res, as = "parsed", simplifyVector = TRUE),
                     error = function(e) NULL)
  ads <- parsed$ads %||% NULL
  if (is.null(ads)) return(NULL)
  if (is.list(ads) && !is.data.frame(ads)) {
    ads <- tryCatch(as.data.frame(ads), error = function(e) NULL)
  }
  if (is.null(ads) || !is.data.frame(ads) || nrow(ads) == 0) return(NULL)

  safe_col <- function(x, default = NA) {
    if (is.null(x)) rep(default, nrow(ads)) else x
  }

  tibble(
    ad_id = as.character(safe_col(ads$list_id)),
    title = as.character(safe_col(ads$subject)),
    price = as.numeric(safe_col(ads$price, 0)),
    price_str = as.character(safe_col(ads$price_string)),
    area = as.numeric(safe_col(ads$size, NA)),
    rooms = as.numeric(safe_col(ads$rooms, NA)),
    address = as.character(safe_col(ads$area_name)),
    ward = as.character(safe_col(ads$ward_name)),
    district_id = as.character(safe_col(ads$area, district_id)),
    district_name = as.character(safe_col(ads$area_name, district_name)),
    category_id = category_id,
    category_name = as.character(safe_col(ads$category_name, category_name)),
    lat = as.numeric(safe_col(ads$latitude, NA)),
    lon = as.numeric(safe_col(ads$longitude, NA)),
    image = map_chr(ads$images %||% vector("list", nrow(ads)),
                    ~ if (length(.x) > 0) as.character(.x[[1]]) else NA_character_),
    ad_url = paste0("https://www.chotot.com/", safe_col(ads$list_id, "")),
    posted_at = map2_chr(safe_col(ads$date, NA), safe_col(ads$list_time, NA), parse_posted_at),
    scraped_at = as.character(Sys.time()),
    page_fetched = page
  )
}

clean_listings <- function(raw_df) {
  if (is.null(raw_df) || nrow(raw_df) == 0) return(raw_df)

  raw_df %>%
    distinct(ad_id, .keep_all = TRUE) %>%
    filter(!is.na(price), price > 0) %>%
    rowwise() %>%
    filter({
      rng <- price_ranges[[category_id]]
      !is.null(rng) && price >= rng[1] && price <= rng[2]
    }) %>%
    ungroup() %>%
    filter(is.na(area) | (area >= 5 & area <= 5000)) %>%
    mutate(
      price_m = price / 1e6,
      price_per_m2 = if_else(!is.na(area) & area > 0, price / area, NA_real_),
      has_coord = as.integer(!is.na(lat) & !is.na(lon)),
      is_rent = as.integer(category_id %in% c("1030", "1050"))
    )
}

upsert_listings <- function(df, db_path) {
  if (is.null(df) || nrow(df) == 0) return(0L)
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  before <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM listings")$n
  dbBegin(con)
  tryCatch({
    dbWriteTable(con, "tmp_listings", df, temporary = TRUE, overwrite = TRUE)
    cols <- dbListFields(con, "listings")
    sql <- paste0(
      "INSERT OR IGNORE INTO listings (", paste(cols, collapse = ", "), ") ",
      "SELECT ", paste(cols, collapse = ", "), " FROM tmp_listings"
    )
    dbExecute(con, sql)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  after <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM listings")$n
  after - before
}

run_scrape <- function(max_pages = CFG$max_pages) {
  dir.create(CFG$output_dir, showWarnings = FALSE)
  db_path <- file.path(CFG$output_dir, CFG$sqlite_file)
  csv_path <- file.path(CFG$output_dir, CFG$csv_file)
  init_db(db_path)

  if (CFG$use_area_filter) {
    combos <- expand.grid(
      district_id = names(DISTRICTS),
      category_id = names(CATEGORIES),
      page = seq_len(max_pages),
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        district_name = DISTRICTS[district_id],
        category_name = CATEGORIES[category_id]
      )
  } else {
    combos <- expand.grid(
      category_id = names(CATEGORIES),
      page = seq_len(max_pages),
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        district_id = NA_character_,
        district_name = NA_character_,
        category_name = CATEGORIES[category_id]
      )
  }

  combos <- combos %>% slice_sample(prop = 1)

  cat(sprintf("Tong API calls: %d\n", nrow(combos)))
  cat(sprintf("SQLite: %s\n", db_path))

  if (CFG$n_workers <= 1) {
    plan(sequential)
  } else {
    plan(multisession, workers = CFG$n_workers)
  }
  on.exit(plan(sequential), add = TRUE)

  raw_df <- future_pmap_dfr(
    combos,
    function(district_id, category_id, page, district_name, category_name) {
      fetch_page(district_id, district_name, category_id, category_name, page)
    },
    .progress = TRUE,
    .options = furrr_options(seed = TRUE)
  )

  clean_df <- clean_listings(raw_df)
  inserted <- upsert_listings(clean_df, db_path)

  con <- dbConnect(SQLite(), db_path)
  all_df <- dbReadTable(con, "listings")
  dbDisconnect(con)
  write_csv(all_df, csv_path)

  cat(sprintf("Records lay duoc: %d\n", nrow(clean_df)))
  cat(sprintf("Records moi vao SQLite: %d\n", inserted))
  cat(sprintf("Tong records SQLite: %d\n", nrow(all_df)))
  invisible(all_df)
}

refresh_data <- function(pages = 3) {
  old_pages <- CFG$max_pages
  on.exit(CFG$max_pages <<- old_pages, add = TRUE)
  CFG$max_pages <<- pages
  run_scrape(max_pages = pages)
}

if (identical(environment(), globalenv())) {
  run_scrape()
}
