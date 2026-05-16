#!/usr/bin/env Rscript

# Lightweight data refresh for the Shiny app.
# It updates raw data, rebuilds combined/featured CSV files, and avoids model retraining.

if (dir.exists("R_libs")) .libPaths(c(normalizePath("R_libs"), .libPaths()))

required_packages <- c("readr", "dplyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Thieu package: ", paste(missing_packages, collapse = ", "))
}

library(readr)
library(dplyr)

FEATURED_CSV <- "data/hcmc_bds_featured.csv"
UPDATE_LOG <- "data/update_log.csv"

count_rows <- function(path) {
  if (!file.exists(path)) return(0L)
  nrow(read_csv(path, show_col_types = FALSE))
}

append_update_log <- function(status, before_rows, after_rows, message_text = "") {
  dir.create("data", showWarnings = FALSE)
  entry <- tibble(
    updated_at = as.character(Sys.time()),
    status = status,
    before_rows = before_rows,
    after_rows = after_rows,
    new_rows = after_rows - before_rows,
    message = message_text
  )

  if (file.exists(UPDATE_LOG)) {
    old <- read_csv(UPDATE_LOG, show_col_types = FALSE)
    write_csv(bind_rows(old, entry), UPDATE_LOG)
  } else {
    write_csv(entry, UPDATE_LOG)
  }

  invisible(entry)
}

run_update_data <- function(
    chotot_pages = as.integer(Sys.getenv("CHOTOT_UPDATE_PAGES", "3")),
    alonhadat_pages = as.integer(Sys.getenv("ALONHADAT_UPDATE_PAGES", "1")),
    include_alonhadat = identical(Sys.getenv("INCLUDE_ALONHADAT_UPDATE", "1"), "1")) {
  before_rows <- count_rows(FEATURED_CSV)

  tryCatch({
    message("== 1/4 Cap nhat nhanh du lieu Cho Tot ==")
    source("scripts/chotot_scraper_v3.R", local = TRUE)
    refresh_data(pages = chotot_pages)

    if (include_alonhadat) {
      message("== 2/4 Cap nhat nhanh du lieu Alonhadat ==")
      source("scripts/alonhadat_scraper.R", local = TRUE)
      run_alonhadat_scrape(max_pages = alonhadat_pages)
    } else {
      message("== 2/4 Bo qua Alonhadat theo cau hinh ==")
    }

    message("== 3/4 Gop nguon du lieu ==")
    source("scripts/merge_sources.R", local = TRUE)
    run_merge_sources()

    message("== 4/4 Tao lai feature dataset ==")
    source("scripts/feature_engineering.R", local = TRUE)
    df <- read_project_data()
    featured <- build_features(df)
    write_csv(featured, FEATURED_CSV)

    after_rows <- nrow(featured)
    log_entry <- append_update_log("success", before_rows, after_rows)
    message("Da cap nhat data: ", before_rows, " -> ", after_rows, " dong.")
    invisible(log_entry)
  }, error = function(e) {
    after_rows <- count_rows(FEATURED_CSV)
    append_update_log("failed", before_rows, after_rows, conditionMessage(e))
    stop(e)
  })
}

if (identical(environment(), globalenv())) {
  run_update_data()
}
