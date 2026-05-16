#!/usr/bin/env Rscript

# Data refresh + conditional retraining.
# Retraining is triggered only when the data changed enough or the model is stale.

if (dir.exists("R_libs")) .libPaths(c(normalizePath("R_libs"), .libPaths()))

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

FEATURED_CSV <- "data/hcmc_bds_featured.csv"
METADATA_PATH <- "models/model_metadata.rds"
AUTO_LOG <- "data/auto_update_log.csv"

count_rows <- function(path) {
  if (!file.exists(path)) return(0L)
  nrow(read_csv(path, show_col_types = FALSE))
}

model_age_days <- function() {
  if (!file.exists(METADATA_PATH)) return(Inf)
  metadata <- readRDS(METADATA_PATH)
  trained_at <- suppressWarnings(as_datetime(metadata$trained_at))
  if (is.na(trained_at)) return(Inf)
  as.numeric(difftime(Sys.time(), trained_at, units = "days"))
}

append_auto_log <- function(status, before_rows, after_rows, retrained, reason, message_text = "") {
  dir.create("data", showWarnings = FALSE)
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
    old <- read_csv(AUTO_LOG, show_col_types = FALSE)
    write_csv(bind_rows(old, entry), AUTO_LOG)
  } else {
    write_csv(entry, AUTO_LOG)
  }

  invisible(entry)
}

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

run_auto_update <- function(
    min_new_ratio = as.numeric(Sys.getenv("RETRAIN_MIN_NEW_RATIO", "0.12")),
    max_model_age_days = as.numeric(Sys.getenv("RETRAIN_MAX_MODEL_AGE_DAYS", "7")),
    force_retrain = identical(Sys.getenv("FORCE_RETRAIN", "0"), "1")) {
  before_rows <- count_rows(FEATURED_CSV)

  tryCatch({
    source("scripts/update_data.R", local = TRUE)
    run_update_data()
    after_rows <- count_rows(FEATURED_CSV)

    decision <- should_retrain(before_rows, after_rows, min_new_ratio, max_model_age_days, force_retrain)
    if (decision$value) {
      message("== Retrain model: ", decision$reason, " ==")
      source("scripts/train_models.R", local = TRUE)
    } else {
      message("Bo qua retrain: ", decision$reason)
    }

    append_auto_log("success", before_rows, after_rows, decision$value, decision$reason)
  }, error = function(e) {
    after_rows <- count_rows(FEATURED_CSV)
    append_auto_log("failed", before_rows, after_rows, FALSE, "error", conditionMessage(e))
    stop(e)
  })
}

if (identical(environment(), globalenv())) {
  run_auto_update()
}
