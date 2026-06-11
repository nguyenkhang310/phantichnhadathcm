#!/usr/bin/env Rscript

# Kiem tra nhanh dashboard va artifact model.
# Script nay khong crawl web, chi kiem tra data/model da tao co doc va du doan duoc.

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

must_exist <- function(path) {
  if (!file.exists(path)) stop("Thieu file bat buoc: ", path)
}

FEATURED_PATH <- PATHS$featured_csv
METRICS_PATH <- PATHS$metrics_csv
REGISTRY_PATH <- PATHS$registry_csv

must_exist(FEATURED_PATH)
must_exist(PATHS$sale_model_rds)
must_exist(PATHS$rent_model_rds)
must_exist(METRICS_PATH)
must_exist(REGISTRY_PATH)

app_env <- new.env(parent = globalenv())
sys.source("app.R", envir = app_env)

df <- app_env$load_data()
if (nrow(df) < 100) stop("Data qua it dong de demo: ", nrow(df))

metrics <- app_env$load_metrics()
registry <- app_env$load_registry()
if (nrow(metrics) == 0) stop(METRICS_PATH, " rong")
if (nrow(registry) == 0) stop(REGISTRY_PATH, " rong")

make_segment_prediction <- function(transaction_type) {
  segment_df <- df %>% filter(transaction_type == !!transaction_type)
  if (nrow(segment_df) == 0) stop("Khong co du lieu segment: ", transaction_type)
  sample_row <- segment_df %>% slice(1)
  input_row <- app_env$build_prediction_row(
    df = df,
    district = sample_row$district_name[[1]],
    category = sample_row$category_name[[1]],
    ward = sample_row$ward[[1]],
    area = sample_row$area[[1]],
    rooms = sample_row$rooms[[1]],
    transaction_type = transaction_type
  )
  pred <- app_env$predict_price(input_row, identical(transaction_type, "Cho thuê"))
  if (is.na(pred) || !is.finite(pred) || pred <= 0) {
    stop("Du doan khong hop le cho segment: ", transaction_type)
  }
  pred
}

sale_pred <- make_segment_prediction("Bán")
rent_pred <- make_segment_prediction("Cho thuê")

cat("Validation OK\n")
cat("Rows:", nrow(df), "\n")
cat("Best sale model:", registry %>% filter(segment == "sale") %>% pull(best_model), "\n")
cat("Best rent model:", registry %>% filter(segment == "rent") %>% pull(best_model), "\n")
cat("Sample sale prediction:", round(sale_pred), "\n")
cat("Sample rent prediction:", round(rent_pred), "\n")
