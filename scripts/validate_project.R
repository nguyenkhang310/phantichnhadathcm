#!/usr/bin/env Rscript

# Fast local validation for the dashboard + ML artifacts.
# This does not scrape the web; it checks that generated data/models can be loaded and used.

if (dir.exists("R_libs")) .libPaths(c(normalizePath("R_libs"), .libPaths()))

required_packages <- c("readr", "dplyr", "shiny", "plotly", "DT", "leaflet", "randomForest", "xgboost", "Matrix")
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

must_exist("data/hcmc_bds_featured.csv")
must_exist("models/price_models_sale.rds")
must_exist("models/price_models_rent.rds")
must_exist("models/model_metrics.csv")
must_exist("models/model_registry.csv")

app_env <- new.env(parent = globalenv())
sys.source("app.R", envir = app_env)

df <- app_env$load_data()
if (nrow(df) < 100) stop("Data qua it dong de demo: ", nrow(df))

metrics <- app_env$load_metrics()
registry <- app_env$load_registry()
if (nrow(metrics) == 0) stop("model_metrics.csv rong")
if (nrow(registry) == 0) stop("model_registry.csv rong")

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
