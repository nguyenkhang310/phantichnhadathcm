#!/usr/bin/env Rscript

# Lightweight data refresh for the Shiny app.
# It updates raw data, rebuilds combined/featured CSV files, and avoids model retraining.

source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()

required_packages <- c("readr", "dplyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Thieu package: ", paste(missing_packages, collapse = ", "))
}

library(readr)
library(dplyr)

FEATURED_INPUT_CSV <- PATHS$featured_csv
FEATURED_CSV <- PATHS$featured_csv
UPDATE_LOG <- PATHS$update_log_csv

# Hàm count_rows: đếm hoặc kiểm tra điều kiện xử lý.
count_rows <- function(path) {
  if (!file.exists(path)) return(0L)
  nrow(read_csv(path, show_col_types = FALSE))
}

# Hàm append_update_log: lưu hoặc cập nhật dữ liệu đầu ra.
append_update_log <- function(status, before_rows, after_rows, message_text = "") {
  dir.create(PATHS$data_dir, showWarnings = FALSE)
  entry <- tibble(
    updated_at = as.character(Sys.time()),
    status = status,
    before_rows = before_rows,
    after_rows = after_rows,
    new_rows = after_rows - before_rows,
    message = message_text
  )

  if (file.exists(UPDATE_LOG)) {
    old <- read_csv(UPDATE_LOG, show_col_types = FALSE) %>%
      mutate(
        updated_at = as.character(updated_at),
        status = as.character(status),
        before_rows = as.integer(before_rows),
        after_rows = as.integer(after_rows),
        new_rows = as.integer(new_rows),
        message = as.character(message)
      )
    write_csv(bind_rows(old, entry), UPDATE_LOG)
  } else {
    write_csv(entry, UPDATE_LOG)
  }

  invisible(entry)
}

# Hàm run_update_data: chạy toàn bộ bước xử lý chính.
run_update_data <- function(
    chotot_pages = as.integer(Sys.getenv("CHOTOT_UPDATE_PAGES", "3")),
    alonhadat_pages = as.integer(Sys.getenv("ALONHADAT_UPDATE_PAGES", "1")),
    luachon_pages = as.integer(Sys.getenv("LUACHON_UPDATE_PAGES", "1")),
    muaban_pages = as.integer(Sys.getenv("MUABAN_UPDATE_PAGES", "1")),
    include_alonhadat = identical(Sys.getenv("INCLUDE_ALONHADAT_UPDATE", "1"), "1"),
    include_alonhadat_local = identical(Sys.getenv("INCLUDE_ALONHADAT_LOCAL_IMPORT", "1"), "1"),
    include_luachon = identical(Sys.getenv("INCLUDE_LUACHON_UPDATE", "1"), "1"),
    include_muaban = identical(Sys.getenv("INCLUDE_MUABAN_UPDATE", "1"), "1"),
    include_mogi = identical(Sys.getenv("INCLUDE_MOGI_IMPORT", "1"), "1"),
    include_homedy = identical(Sys.getenv("INCLUDE_HOMEDY_IMPORT", "1"), "1")) {
  before_rows <- count_rows(FEATURED_INPUT_CSV)

  tryCatch({
    message("== 1/9 Cap nhat nhanh du lieu Cho Tot ==")
    source(PATHS$chotot_scraper_script, local = TRUE)
    refresh_data(pages = chotot_pages)

    if (include_alonhadat) {
      message("== 2/9 Cap nhat nhanh du lieu Alonhadat ==")
      source(PATHS$alonhadat_scraper_script, local = TRUE)
      run_alonhadat_scrape(max_pages = alonhadat_pages)
    } else {
      message("== 2/9 Bo qua Alonhadat theo cau hinh ==")
    }

    if (include_alonhadat_local && file.exists(PATHS$alonhadat_local_source_csv)) {
      message("== 3/9 Import lai du lieu Alonhadat local tu CSV local ==")
      tryCatch({
        source(PATHS$import_alonhadat_local_script, local = TRUE)
        run_import_alonhadat_local()
      }, error = function(e) {
        message("Bo qua import Alonhadat local: ", conditionMessage(e))
      })
    } else if (include_alonhadat_local) {
      message("== 3/9 Bo qua Alonhadat local: khong tim thay ", PATHS$alonhadat_local_source_csv, " ==")
    } else {
      message("== 3/9 Bo qua Alonhadat local theo cau hinh ==")
    }

    if (include_luachon) {
      message("== 4/9 Cap nhat nhanh du lieu Luachonnhadat ==")
      tryCatch({
        source(PATHS$luachon_scraper_script, local = TRUE)
        run_luachon_scrape(max_pages = luachon_pages)
      }, error = function(e) {
        message("Bo qua Luachonnhadat: ", conditionMessage(e))
      })
    } else {
      message("== 4/9 Bo qua Luachonnhadat theo cau hinh ==")
    }

    if (include_muaban) {
      message("== 5/9 Cap nhat nhanh du lieu Muaban ==")
      tryCatch({
        source(PATHS$muaban_scraper_script, local = TRUE)
        run_muaban_scrape(max_pages = muaban_pages)
      }, error = function(e) {
        message("Bo qua Muaban: ", conditionMessage(e))
      })
    } else {
      message("== 5/9 Bo qua Muaban theo cau hinh ==")
    }

    if (include_mogi && file.exists(PATHS$mogi_source_csv)) {
      message("== 6/9 Import lai du lieu Mogi tu CSV local ==")
      tryCatch({
        source(PATHS$import_mogi_script, local = TRUE)
        run_import_mogi()
      }, error = function(e) {
        message("Bo qua import Mogi: ", conditionMessage(e))
      })
    } else if (include_mogi) {
      message("== 6/9 Bo qua Mogi: khong tim thay ", PATHS$mogi_source_csv, " ==")
    } else {
      message("== 6/9 Bo qua Mogi theo cau hinh ==")
    }

    if (include_homedy && file.exists(PATHS$homedy_source_csv)) {
      message("== 7/9 Import lai du lieu Homedy tu CSV local ==")
      tryCatch({
        source(PATHS$import_homedy_script, local = TRUE)
        run_import_homedy()
      }, error = function(e) {
        message("Bo qua import Homedy: ", conditionMessage(e))
      })
    } else if (include_homedy) {
      message("== 7/9 Bo qua Homedy: khong tim thay ", PATHS$homedy_source_csv, " ==")
    } else {
      message("== 7/9 Bo qua Homedy theo cau hinh ==")
    }

    message("== 8/9 Gop nguon du lieu ==")
    source(PATHS$merge_sources_script, local = TRUE)
    run_merge_sources()

    message("== 9/9 Tao lai feature dataset ==")
    source(PATHS$feature_engineering_script, local = TRUE)
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

if (sys.nframe() == 0) {
  run_update_data()
}
