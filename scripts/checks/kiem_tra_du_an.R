#!/usr/bin/env Rscript

# Kiểm tra nhanh dashboard, dữ liệu và artifact mô hình.
# Script này không crawl web, chỉ kiểm tra dữ liệu/mô hình đã tạo.

source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()
source(PATHS$display_labels_script)

required_packages <- c("readr", "dplyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Thiếu package: ", paste(missing_packages, collapse = ", "))
}

library(readr)
library(dplyr)

must_exist <- function(path) {
  if (!file.exists(path)) stop("Thiếu file bắt buộc: ", path)
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
if (nrow(df) < 100) stop("Dữ liệu quá ít dòng để demo: ", nrow(df))

metrics <- app_env$load_metrics()
registry <- app_env$load_registry()
if (nrow(metrics) == 0) stop(METRICS_PATH, " rỗng")
if (nrow(registry) == 0) stop(REGISTRY_PATH, " rỗng")

make_segment_prediction <- function(transaction_type) {
  segment_df <- df %>% filter(transaction_type == !!transaction_type)
  if (nrow(segment_df) == 0) stop("Không có dữ liệu phân khúc: ", transaction_type)
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
    stop("Dự đoán không hợp lệ cho phân khúc: ", transaction_type)
  }
  pred
}

sale_pred <- make_segment_prediction("Bán")
rent_pred <- make_segment_prediction("Cho thuê")

cat("Kiểm tra OK\n")
cat("Số dòng:", nrow(df), "\n")
cat("Mô hình bán tốt nhất:", registry %>% filter(segment == "sale") %>% pull(best_model) %>% model_label_vi(), "\n")
cat("Mô hình cho thuê tốt nhất:", registry %>% filter(segment == "rent") %>% pull(best_model) %>% model_label_vi(), "\n")
cat("Dự đoán mẫu giá bán:", round(sale_pred), "\n")
cat("Dự đoán mẫu giá thuê:", round(rent_pred), "\n")
