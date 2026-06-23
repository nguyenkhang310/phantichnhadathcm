#!/usr/bin/env Rscript

# Data refresh + conditional retraining.
# Retraining is triggered only when the data changed enough or the model is stale.

source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()

required_packages <- c("readr", "dplyr", "lubridate")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Thieu package: ", paste(missing_packages, collapse = ", "))
}

library(readr)
library(dplyr)
library(lubridate)

FEATURED_INPUT_CSV <- PATHS$featured_csv
FEATURED_CSV <- PATHS$featured_csv
METADATA_PATH <- PATHS$model_metadata_rds
AUTO_LOG <- PATHS$auto_update_log_csv

# Hàm count_rows: đếm hoặc kiểm tra điều kiện xử lý.
count_rows <- function(path) {
  if (!file.exists(path)) return(0L)
  nrow(read_csv(path, show_col_types = FALSE))
}

# Hàm model_age_days: hỗ trợ xử lý dữ liệu trong script.
model_age_days <- function() {
  if (!file.exists(METADATA_PATH)) return(Inf)
  metadata <- readRDS(METADATA_PATH)
  trained_at <- suppressWarnings(as_datetime(metadata$trained_at))
  if (is.na(trained_at)) return(Inf)
  as.numeric(difftime(Sys.time(), trained_at, units = "days"))
}

# Hàm append_auto_log: lưu hoặc cập nhật dữ liệu đầu ra.
append_auto_log <- function(status, before_rows, after_rows, retrained, reason, message_text = "") {
  dir.create(PATHS$data_dir, showWarnings = FALSE)
  entry <- tibble(
    updated_at = as.character(Sys.time()),
    status = status,
    before_rows = before_rows,
    after_rows = after_rows,
    new_rows = after_rows - before_rows,
    retrained = retrained,
    reason = reason,
    message = message_text
  )

  if (file.exists(AUTO_LOG)) {
    old <- read_csv(AUTO_LOG, show_col_types = FALSE) %>%
      mutate(
        updated_at = as.character(updated_at),
        status = as.character(status),
        before_rows = as.integer(before_rows),
        after_rows = as.integer(after_rows),
        new_rows = as.integer(new_rows),
        retrained = as.logical(retrained),
        reason = as.character(reason),
        message = as.character(message)
      )
    write_csv(bind_rows(old, entry), AUTO_LOG)
  } else {
    write_csv(entry, AUTO_LOG)
  }

  invisible(entry)
}

# Hàm should_retrain: đếm hoặc kiểm tra điều kiện xử lý.
should_retrain <- function(before_rows, after_rows, min_new_ratio, max_model_age_days, force = FALSE) {
  if (force) return(list(value = TRUE, reason = "force"))
  if (!file.exists(METADATA_PATH)) return(list(value = TRUE, reason = "missing_model_metadata"))
  if (before_rows == 0 && after_rows > 0) return(list(value = TRUE, reason = "first_dataset"))

  new_ratio <- if (before_rows > 0) (after_rows - before_rows) / before_rows else 0
  if (is.finite(new_ratio) && new_ratio >= min_new_ratio) {
    return(list(value = TRUE, reason = paste0("new_data_ratio_", round(new_ratio * 100, 1), "%")))
  }

  age <- model_age_days()
  if (is.finite(age) && age >= max_model_age_days) {
    return(list(value = TRUE, reason = paste0("model_age_", round(age, 1), "_days")))
  }

  list(value = FALSE, reason = "data_refreshed_model_still_valid")
}

# Hàm run_auto_update: chạy toàn bộ bước xử lý chính.
run_auto_update <- function(
    min_new_ratio = as.numeric(Sys.getenv("RETRAIN_MIN_NEW_RATIO", "0.12")),
    max_model_age_days = as.numeric(Sys.getenv("RETRAIN_MAX_MODEL_AGE_DAYS", "7")),
    force_retrain = identical(Sys.getenv("FORCE_RETRAIN", "0"), "1")) {
  before_rows <- count_rows(FEATURED_INPUT_CSV)

  tryCatch({
    source(PATHS$update_data_script, local = TRUE)
    run_update_data()
    after_rows <- count_rows(FEATURED_CSV)

    decision <- should_retrain(before_rows, after_rows, min_new_ratio, max_model_age_days, force_retrain)
    if (decision$value) {
      message("== Huấn luyện lại mô hình: ", decision$reason, " ==")
      source(PATHS$train_models_script, local = TRUE)
    } else {
      message("Bỏ qua huấn luyện lại: ", decision$reason)
    }

    append_auto_log("success", before_rows, after_rows, decision$value, decision$reason)
  }, error = function(e) {
    after_rows <- count_rows(FEATURED_CSV)
    append_auto_log("failed", before_rows, after_rows, FALSE, "error", conditionMessage(e))
    stop(e)
  })
}

if (sys.nframe() == 0) {
  run_auto_update()
}
