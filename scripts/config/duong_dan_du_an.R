# Cấu hình đường dẫn dùng chung cho toàn dự án.

use_local_r_libs <- function() {
  local_libs <- ".local/thu_vien_r"
  existing <- local_libs[dir.exists(local_libs)]
  if (length(existing) > 0) {
    .libPaths(unique(c(normalizePath(existing), .libPaths())))
  }
  invisible(.libPaths())
}

PATHS <- list(
  data_dir = "data",
  raw_dir = "data/raw",
  main_dir = "data/main",
  interim_dir = "data/interim",
  cache_dir = "data/cache",
  log_dir = "data/logs",
  model_dir = "models",
  plot_dir = "plots",

  chotot_raw_csv = "data/raw/chotot/chotot_schema_chuan.csv",
  chotot_sqlite = "data/cache/cache_chotot.sqlite",

  alonhadat_raw_csv = "data/raw/alonhadat/alonhadat_schema_chuan.csv",
  alonhadat_local_source_csv = "data/raw/alonhadat/alonhadat_local_nguon.csv",
  alonhadat_local_clean_csv = "data/raw/alonhadat/alonhadat_local_sach.csv",
  alonhadat_local_raw_csv = "data/raw/alonhadat/alonhadat_local_schema_chuan.csv",

  luachon_assignment_csv = "data/raw/luachonnhadat/luachonnhadat_sach.csv",
  luachon_raw_csv = "data/raw/luachonnhadat/luachonnhadat_schema_chuan.csv",

  muaban_raw_csv = "data/raw/muaban/muaban_schema_chuan.csv",

  mogi_source_csv = "data/raw/mogi/mogi_sach_goc.csv",
  mogi_raw_2_csv = "data/raw/mogi/mogi_tho_bo_sung.csv",
  mogi_source_csv_2 = "data/raw/mogi/mogi_sach_bo_sung.csv",
  mogi_scraped_csv = "data/raw/mogi/mogi_sach_crawl.csv",
  mogi_raw_csv = "data/raw/mogi/mogi_schema_chuan.csv",

  homedy_source_csv = "data/raw/homedy/homedy_sach_goc.csv",
  homedy_raw_csv = "data/raw/homedy/homedy_schema_chuan.csv",

  combined_raw_csv = "data/interim/du_lieu_gop_nguon.csv",
  featured_csv = "data/main/du_lieu_chinh.csv",

  update_log_csv = "data/logs/nhat_ky_cap_nhat.csv",
  auto_update_log_csv = "data/logs/nhat_ky_tu_dong_cap_nhat.csv",

  metrics_csv = "models/chi_so_mo_hinh.csv",
  registry_csv = "models/dang_ky_mo_hinh.csv",
  clusters_csv = "models/cum_gia_quan_huyen.csv",
  rf_importance_sale_csv = "models/do_quan_trong_bien_ban.csv",
  rf_importance_rent_csv = "models/do_quan_trong_bien_thue.csv",
  sale_model_rds = "models/mo_hinh_gia_ban.rds",
  rent_model_rds = "models/mo_hinh_gia_thue.rds",
  kmeans_model_rds = "models/mo_hinh_phan_cum_gia_dien_tich.rds",
  model_metadata_rds = "models/thong_tin_mo_hinh.rds",

  eda_summary_csv = "plots/tom_tat_eda_hcm.csv",

  district_normalization_script = "scripts/lib/chuan_hoa_quan_huyen.R",
  chotot_scraper_script = "scripts/scrapers/thu_thap_chotot.R",
  alonhadat_scraper_script = "scripts/scrapers/thu_thap_alonhadat.R",
  luachon_scraper_script = "scripts/scrapers/thu_thap_luachonnhadat.R",
  muaban_scraper_script = "scripts/scrapers/thu_thap_muaban.R",
  mogi_scraper_script = "scripts/scrapers/thu_thap_mogi.R",
  import_alonhadat_local_script = "scripts/importers/nhap_alonhadat_local.R",
  import_mogi_script = "scripts/importers/nhap_mogi.R",
  import_homedy_script = "scripts/importers/nhap_homedy.R",
  merge_sources_script = "scripts/processing/gop_nguon_du_lieu.R",
  feature_engineering_script = "scripts/processing/tao_dac_trung.R",
  eda_script = "scripts/analysis/phan_tich_eda.R",
  train_models_script = "scripts/models/huan_luyen_mo_hinh.R",
  update_data_script = "scripts/pipeline/cap_nhat_du_lieu.R",
  auto_update_script = "scripts/pipeline/tu_dong_cap_nhat.R"
)

PATHS$source_raw_csvs <- c(
  PATHS$alonhadat_raw_csv,
  PATHS$alonhadat_local_raw_csv,
  PATHS$luachon_raw_csv,
  PATHS$muaban_raw_csv,
  PATHS$mogi_raw_csv,
  PATHS$homedy_raw_csv
)
