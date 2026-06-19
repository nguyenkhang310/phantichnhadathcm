#!/usr/bin/env Rscript

source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()

# Hàm run_optional_scraper: chạy toàn bộ bước xử lý chính.
run_optional_scraper <- function(label, script_path, function_name) {
  tryCatch({
    source(script_path, local = TRUE)
    get(function_name)()
    TRUE
  }, error = function(e) {
    message("Bỏ qua ", label, ": ", conditionMessage(e))
    FALSE
  })
}

# Hàm run_pipeline: chạy toàn bộ bước xử lý chính.
run_pipeline <- function() {
  message("== 1/12 Scrape dữ liệu Chợ Tốt ==")
  source(PATHS$chotot_scraper_script, local = TRUE)
  run_scrape()

  message("== 2/12 Scrape dữ liệu Alonhadat ==")
  source(PATHS$alonhadat_scraper_script, local = TRUE)
  run_alonhadat_scrape()

  message("== 3/12 Import dữ liệu Alonhadat local từ CSV ==")
  source(PATHS$import_alonhadat_local_script, local = TRUE)
  run_import_alonhadat_local()

  message("== 4/12 Scrape dữ liệu Luachonnhadat ==")
  source(PATHS$luachon_scraper_script, local = TRUE)
  run_luachon_scrape()

  message("== 5/12 Scrape dữ liệu Muaban (nguồn phụ) ==")
  run_optional_scraper("Muaban", PATHS$muaban_scraper_script, "run_muaban_scrape")

  message("== 6/12 Scrape bổ sung dữ liệu Mogi ==")
  run_optional_scraper("Mogi", PATHS$mogi_scraper_script, "run_mogi_scrape")

  message("== 7/12 Import dữ liệu Mogi từ CSV/crawl ==")
  source(PATHS$import_mogi_script, local = TRUE)
  run_import_mogi()

  message("== 8/12 Import dữ liệu Homedy từ CSV ==")
  source(PATHS$import_homedy_script, local = TRUE)
  run_import_homedy()

  message("== 9/12 Gộp dữ liệu nhiều nguồn ==")
  source(PATHS$merge_sources_script, local = TRUE)
  run_merge_sources()

  message("== 10/12 Feature engineering ==")
  source(PATHS$feature_engineering_script, local = TRUE)
  df <- read_project_data()
  featured <- build_features(df)
  readr::write_csv(featured, FEATURED_CSV)

  message("== 11/12 EDA plots ==")
  source(PATHS$eda_script, local = TRUE)
  run_eda()

  message("== 12/12 Train models ==")
  source(PATHS$train_models_script, local = TRUE)

  message("Hoàn tất pipeline. Featured data: ", PATHS$featured_csv)
  message("Chạy app bằng: Rscript app.R")
}

if (sys.nframe() == 0) {
  run_pipeline()
}
