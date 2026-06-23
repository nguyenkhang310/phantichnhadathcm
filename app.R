# ============================================================
# ỨNG DỤNG SHINY - BĐS TP.HCM
# app.R là tên quy ước để shiny::runApp(".") tự nhận diện.
# Chạy trực tiếp: Rscript app.R
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
source(PATHS$display_labels_script)

lay_bien_moi_truong <- function(ten, mac_dinh = "") {
  gia_tri <- Sys.getenv(ten, unset = "")
  if (nzchar(gia_tri)) gia_tri else mac_dinh
}

DIA_CHI_UNG_DUNG <- lay_bien_moi_truong("BDS_APP_HOST", "127.0.0.1")
CONG_UNG_DUNG <- suppressWarnings(as.integer(
  lay_bien_moi_truong("BDS_APP_PORT", lay_bien_moi_truong("PORT", "3838"))
))

if (is.na(CONG_UNG_DUNG) || CONG_UNG_DUNG <= 0) {
  stop("BDS_APP_PORT phải là một số nguyên dương.")
}

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

lay_pid_theo_cong <- function(cong) {
  if (Sys.which("lsof") == "") return(character())
  pids <- tryCatch(
    system2("lsof", c("-ti", paste0("tcp:", cong)), stdout = TRUE, stderr = FALSE),
    warning = function(w) character(),
    error = function(e) character()
  )
  unique(pids[nzchar(pids)])
}

cong_dang_ban <- function(cong) {
  length(lay_pid_theo_cong(cong)) > 0
}

giai_phong_cong <- function(cong, thoi_gian_cho_giay = 8) {
  pids <- lay_pid_theo_cong(cong)
  if (length(pids) == 0) return(invisible(TRUE))

  message("Port ", cong, " đang bận bởi PID: ", paste(pids, collapse = ", "), ". Đang dừng process cũ...")
  system2("kill", c("-TERM", pids), stdout = FALSE, stderr = FALSE)

  deadline <- Sys.time() + thoi_gian_cho_giay
  while (Sys.time() < deadline) {
    Sys.sleep(0.25)
    if (!cong_dang_ban(cong)) return(invisible(TRUE))
  }

  remaining <- lay_pid_theo_cong(cong)
  if (length(remaining) > 0) {
    message("Port ", cong, " vẫn bận. Force kill PID: ", paste(remaining, collapse = ", "))
    system2("kill", c("-KILL", remaining), stdout = FALSE, stderr = FALSE)
    Sys.sleep(0.5)
  }

  if (cong_dang_ban(cong)) {
    stop("Không thể giải phóng port ", cong, ". Hãy kiểm tra process đang giữ port này.")
  }

  invisible(TRUE)
}

ung_dung_shiny <- shinyApp(ui, server)

if (sys.nframe() == 0) {
  giai_phong_cong(CONG_UNG_DUNG)
  shiny::runApp(".", host = DIA_CHI_UNG_DUNG, port = CONG_UNG_DUNG, launch.browser = FALSE)
} else {
  ung_dung_shiny
}
