#!/usr/bin/env Rscript

# Lightweight data refresh for the Shiny app.
# It updates raw data, rebuilds combined/featured CSV files, and avoids model retraining.
# Optional target mode:
#   UPDATE_TO_TARGET=1 TARGET_ROWS=30000 DRY_RUN=1 Rscript scripts/pipeline/cap_nhat_du_lieu.R
#   UPDATE_TO_TARGET=1 TARGET_ROWS=30000 Rscript scripts/pipeline/cap_nhat_du_lieu.R

source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()

required_packages <- c("readr", "dplyr", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Thieu package: ", paste(missing_packages, collapse = ", "))
}

library(readr)
library(dplyr)
library(tibble)

FEATURED_INPUT_CSV <- PATHS$featured_csv
FEATURED_CSV <- PATHS$featured_csv
UPDATE_LOG <- PATHS$update_log_csv

TARGET_UPDATE_PROFILES <- tibble::tribble(
  ~label, ~mogi_start_page, ~mogi_pages, ~chotot_pages, ~alonhadat_pages, ~luachon_pages, ~muaban_pages,
  "mogi_thue_120_trang", 1L, 120L, 0L, 0L, 0L, 0L,
  "mogi_thue_140_trang", 121L, 140L, 0L, 0L, 0L, 0L,
  "mogi_thue_180_trang", 141L, 180L, 0L, 0L, 0L, 0L,
  "chotot_bo_sung_10_trang", NA_integer_, 0L, 10L, 0L, 0L, 0L
)

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
    mogi_start_page = as.integer(Sys.getenv("MOGI_START_PAGE", "1")),
    mogi_pages = as.integer(Sys.getenv("MOGI_UPDATE_PAGES", "30")),
    include_chotot = identical(Sys.getenv("INCLUDE_CHOTOT_UPDATE", "1"), "1"),
    include_alonhadat = identical(Sys.getenv("INCLUDE_ALONHADAT_UPDATE", "1"), "1"),
    include_alonhadat_local = identical(Sys.getenv("INCLUDE_ALONHADAT_LOCAL_IMPORT", "1"), "1"),
    include_luachon = identical(Sys.getenv("INCLUDE_LUACHON_UPDATE", "1"), "1"),
    include_muaban = identical(Sys.getenv("INCLUDE_MUABAN_UPDATE", "1"), "1"),
    include_mogi_scrape = identical(Sys.getenv("INCLUDE_MOGI_SCRAPE", "0"), "1"),
    mogi_append_existing = identical(Sys.getenv("MOGI_APPEND_EXISTING", "0"), "1"),
    include_mogi = identical(Sys.getenv("INCLUDE_MOGI_IMPORT", "1"), "1"),
    include_homedy = identical(Sys.getenv("INCLUDE_HOMEDY_IMPORT", "1"), "1")) {
  before_rows <- count_rows(FEATURED_INPUT_CSV)

  tryCatch({
    if (include_chotot && chotot_pages > 0) {
      message("== 1/10 Cap nhat nhanh du lieu Cho Tot ==")
      source(PATHS$chotot_scraper_script, local = TRUE)
      refresh_data(pages = chotot_pages)
    } else {
      message("== 1/10 Bo qua Cho Tot theo cau hinh ==")
    }

    if (include_alonhadat) {
      message("== 2/10 Cap nhat nhanh du lieu Alonhadat ==")
      source(PATHS$alonhadat_scraper_script, local = TRUE)
      run_alonhadat_scrape(max_pages = alonhadat_pages)
    } else {
      message("== 2/10 Bo qua Alonhadat theo cau hinh ==")
    }

    if (include_alonhadat_local && file.exists(PATHS$alonhadat_local_source_csv)) {
      message("== 3/10 Import lai du lieu Alonhadat local tu CSV local ==")
      tryCatch({
        source(PATHS$import_alonhadat_local_script, local = TRUE)
        run_import_alonhadat_local()
      }, error = function(e) {
        message("Bo qua import Alonhadat local: ", conditionMessage(e))
      })
    } else if (include_alonhadat_local) {
      message("== 3/10 Bo qua Alonhadat local: khong tim thay ", PATHS$alonhadat_local_source_csv, " ==")
    } else {
      message("== 3/10 Bo qua Alonhadat local theo cau hinh ==")
    }

    if (include_luachon) {
      message("== 4/10 Cap nhat nhanh du lieu Luachonnhadat ==")
      tryCatch({
        source(PATHS$luachon_scraper_script, local = TRUE)
        run_luachon_scrape(max_pages = luachon_pages)
      }, error = function(e) {
        message("Bo qua Luachonnhadat: ", conditionMessage(e))
      })
    } else {
      message("== 4/10 Bo qua Luachonnhadat theo cau hinh ==")
    }

    if (include_muaban) {
      message("== 5/10 Cap nhat nhanh du lieu Muaban ==")
      tryCatch({
        source(PATHS$muaban_scraper_script, local = TRUE)
        run_muaban_scrape(max_pages = muaban_pages)
      }, error = function(e) {
        message("Bo qua Muaban: ", conditionMessage(e))
      })
    } else {
      message("== 5/10 Bo qua Muaban theo cau hinh ==")
    }

    if (include_mogi_scrape) {
      message("== 6/10 Crawl bo sung du lieu Mogi, uu tien tin thue ==")
      tryCatch({
        source(PATHS$mogi_scraper_script, local = TRUE)
        run_mogi_scrape(
          start_page = mogi_start_page,
          max_pages = mogi_pages,
          groups = "rent",
          fetch_details = FALSE,
          append_existing = mogi_append_existing
        )
      }, error = function(e) {
        message("Bo qua crawl Mogi: ", conditionMessage(e))
      })
    } else {
      message("== 6/10 Bo qua crawl Mogi theo cau hinh ==")
    }

    if (include_mogi && file.exists(PATHS$mogi_source_csv)) {
      message("== 7/10 Import lai du lieu Mogi tu CSV local/crawl ==")
      tryCatch({
        source(PATHS$import_mogi_script, local = TRUE)
        run_import_mogi()
      }, error = function(e) {
        message("Bo qua import Mogi: ", conditionMessage(e))
      })
    } else if (include_mogi) {
      message("== 7/10 Bo qua Mogi: khong tim thay ", PATHS$mogi_source_csv, " ==")
    } else {
      message("== 7/10 Bo qua Mogi theo cau hinh ==")
    }

    if (include_homedy && file.exists(PATHS$homedy_source_csv)) {
      message("== 8/10 Import lai du lieu Homedy tu CSV local ==")
      tryCatch({
        source(PATHS$import_homedy_script, local = TRUE)
        run_import_homedy()
      }, error = function(e) {
        message("Bo qua import Homedy: ", conditionMessage(e))
      })
    } else if (include_homedy) {
      message("== 8/10 Bo qua Homedy: khong tim thay ", PATHS$homedy_source_csv, " ==")
    } else {
      message("== 8/10 Bo qua Homedy theo cau hinh ==")
    }

    message("== 9/10 Gop nguon du lieu ==")
    source(PATHS$merge_sources_script, local = TRUE)
    run_merge_sources()

    message("== 10/10 Tao lai feature dataset ==")
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

print_target_update_plan <- function(before_rows, target_rows, profiles = TARGET_UPDATE_PROFILES) {
  need_rows <- max(target_rows - before_rows, 0L)
  cat("Dataset hien tai: ", before_rows, " dong\n", sep = "")
  cat("Muc tieu: ", target_rows, " dong\n", sep = "")
  cat("Can them: ", need_rows, " dong\n\n", sep = "")
  cat("Profile se thu theo thu tu:\n")
  print(profiles)
  invisible(need_rows)
}

run_update_to_target <- function(
    target_rows = as.integer(Sys.getenv("TARGET_ROWS", "20000")),
    dry_run = identical(Sys.getenv("DRY_RUN", "0"), "1"),
    profiles = TARGET_UPDATE_PROFILES) {
  before_rows <- count_rows(FEATURED_INPUT_CSV)
  need_rows <- print_target_update_plan(before_rows, target_rows, profiles)

  if (need_rows <= 0) {
    message("Dataset da dat moc muc tieu, khong can cap nhat.")
    return(invisible(before_rows))
  }
  if (dry_run) {
    message("DRY_RUN=1 nen chi in ke hoach, chua crawl/import du lieu.")
    return(invisible(before_rows))
  }

  old_area_filter <- Sys.getenv("CHOTOT_USE_AREA_FILTER", unset = NA_character_)
  old_workers <- Sys.getenv("CHOTOT_WORKERS", unset = NA_character_)
  Sys.setenv(CHOTOT_USE_AREA_FILTER = "1")
  if (is.na(old_workers) || old_workers == "") Sys.setenv(CHOTOT_WORKERS = "4")
  on.exit({
    if (is.na(old_area_filter)) Sys.unsetenv("CHOTOT_USE_AREA_FILTER") else Sys.setenv(CHOTOT_USE_AREA_FILTER = old_area_filter)
    if (is.na(old_workers)) Sys.unsetenv("CHOTOT_WORKERS") else Sys.setenv(CHOTOT_WORKERS = old_workers)
  }, add = TRUE)

  after_rows <- before_rows
  for (i in seq_len(nrow(profiles))) {
    profile <- profiles[i, ]
    message("== Thu profile ", profile$label, " ==")
    mogi_start_page <- ifelse(is.na(profile$mogi_start_page), 1L, profile$mogi_start_page)
    mogi_append_existing <- profile$mogi_pages > 0 && mogi_start_page > 1L

    ok <- tryCatch({
      run_update_data(
        chotot_pages = profile$chotot_pages,
        alonhadat_pages = profile$alonhadat_pages,
        luachon_pages = profile$luachon_pages,
        muaban_pages = profile$muaban_pages,
        include_alonhadat = FALSE,
        include_alonhadat_local = TRUE,
        include_luachon = FALSE,
        include_muaban = FALSE,
        include_chotot = profile$chotot_pages > 0,
        include_mogi_scrape = profile$mogi_pages > 0,
        include_mogi = TRUE,
        include_homedy = TRUE,
        mogi_start_page = mogi_start_page,
        mogi_append_existing = mogi_append_existing,
        mogi_pages = profile$mogi_pages
      )
      TRUE
    }, error = function(e) {
      message("Profile ", profile$label, " loi: ", conditionMessage(e))
      FALSE
    })

    after_rows <- count_rows(FEATURED_CSV)
    message("So dong sau profile ", profile$label, ": ", after_rows)
    if (ok && after_rows >= target_rows) break
  }

  if (after_rows < target_rows) {
    message("Chua dat ", target_rows, " dong. Can bo sung nguon local moi hoac tang profile crawl co kiem soat.")
  } else {
    message("Da dat muc tieu: ", after_rows, " dong.")
  }

  invisible(after_rows)
}

if (sys.nframe() == 0) {
  if (identical(Sys.getenv("UPDATE_TO_TARGET", "0"), "1")) {
    run_update_to_target()
  } else {
    run_update_data()
  }
}
