#!/usr/bin/env Rscript

if (dir.exists("R_libs")) {
  .libPaths(c(normalizePath("R_libs"), .libPaths()))
}

message("== 1/6 Scrape dữ liệu Chợ Tốt ==")
source("scripts/chotot_scraper_v3.R", local = TRUE)
run_scrape()

message("== 2/6 Scrape dữ liệu Alonhadat ==")
source("scripts/alonhadat_scraper.R", local = TRUE)
run_alonhadat_scrape()

message("== 3/6 Gộp dữ liệu nhiều nguồn ==")
source("scripts/merge_sources.R", local = TRUE)
run_merge_sources()

message("== 4/6 Feature engineering ==")
source("scripts/feature_engineering.R", local = TRUE)
df <- read_project_data()
featured <- build_features(df)
readr::write_csv(featured, FEATURED_CSV)

message("== 5/6 EDA plots ==")
source("scripts/eda_analysis.R", local = TRUE)
run_eda()

message("== 6/6 Train models ==")
source("scripts/train_models.R", local = TRUE)

message("Hoàn tất pipeline. Chạy app bằng: Rscript run_app.R")
