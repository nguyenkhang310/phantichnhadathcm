# ============================================================
# ỨNG DỤNG SHINY - BĐS TP.HCM
# app.R là tên quy ước để shiny::runApp(".") tự nhận diện.
# Chạy: Rscript -e 'shiny::runApp(".", host="127.0.0.1", port=3838)'
# ============================================================

source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()

GOI_CAN_THIET <- c(
  "shiny", "dplyr", "readr", "lubridate",
  "ggplot2", "plotly", "DT", "randomForest",
  "leaflet", "Matrix", "xgboost"
)

nap_goi_ung_dung <- function(goi = GOI_CAN_THIET) {
  goi_thieu <- goi[
    !vapply(goi, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(goi_thieu) > 0) {
    stop(
      "Thiếu package: ", paste(goi_thieu, collapse = ", "),
      "\nCài bằng: install.packages(c(",
      paste(sprintf('\"%s\"', goi_thieu), collapse = ", "), "))"
    )
  }

  invisible(lapply(goi, library, character.only = TRUE))
}

nap_goi_ung_dung()
source(PATHS$district_normalization_script)

DATA_PATH <- PATHS$featured_csv
RAW_PATH <- PATHS$combined_raw_csv
METRICS_PATH <- PATHS$metrics_csv
SALE_MODEL_PATH <- PATHS$sale_model_rds
RENT_MODEL_PATH <- PATHS$rent_model_rds
CLUSTER_PATH <- PATHS$clusters_csv
REGISTRY_PATH <- PATHS$registry_csv
RF_IMPORTANCE_SALE_PATH <- PATHS$rf_importance_sale_csv
RF_IMPORTANCE_RENT_PATH <- PATHS$rf_importance_rent_csv

sys.source("ung_dung/ham_ho_tro_ung_dung.R", envir = environment())
sys.source("ung_dung/giao_dien_ung_dung.R", envir = environment())
sys.source("ung_dung/may_chu_ung_dung.R", envir = environment())

shinyApp(ui, server)
