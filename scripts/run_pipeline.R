#!/usr/bin/env Rscript

if (dir.exists("R_libs")) {
  .libPaths(c(normalizePath("R_libs"), .libPaths()))
}

message("== 1/4 Scrape dữ liệu Chợ Tốt ==")
source("scripts/chotot_scraper_v3.R", local = TRUE)
run_scrape()

message("== 2/4 Feature engineering ==")
source("scripts/feature_engineering.R", local = TRUE)
df <- read_project_data()
featured <- build_features(df)
readr::write_csv(featured, FEATURED_CSV)

message("== 3/4 EDA plots ==")
source("scripts/eda_analysis.R", local = TRUE)
run_eda()

message("== 4/4 Train models ==")
source("scripts/train_models.R", local = TRUE)

message("Hoàn tất pipeline. Chạy app bằng: Rscript run_app.R")
