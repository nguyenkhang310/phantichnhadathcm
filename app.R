# ============================================================
# SHINY APP - BĐS TP.HCM
# Giao diện dashboard tùy biến, backend giữ pipeline R.
# Chạy: Rscript -e 'shiny::runApp(".", host="127.0.0.1", port=3838)'
# ============================================================

if (dir.exists("R_libs")) .libPaths(c(normalizePath("R_libs"), .libPaths()))

required_packages <- c(
  "shiny", "dplyr", "readr", "lubridate",
  "ggplot2", "plotly", "DT", "randomForest", "leaflet", "Matrix", "xgboost"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Thiếu package: ", paste(missing_packages, collapse = ", "),
    "\nCài bằng: install.packages(c(",
    paste(sprintf('\"%s\"', missing_packages), collapse = ", "), "))"
  )
}

library(shiny)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)
library(randomForest)
library(leaflet)
library(Matrix)
library(xgboost)

DATA_PATH <- "data/hcmc_bds_featured.csv"
RAW_PATH <- "data/hcmc_bds_raw.csv"
METRICS_PATH <- "models/model_metrics.csv"
SALE_MODEL_PATH <- "models/price_models_sale.rds"
RENT_MODEL_PATH <- "models/price_models_rent.rds"
CLUSTER_PATH <- "models/kmeans_area_price.csv"
REGISTRY_PATH <- "models/model_registry.csv"
UPDATE_LOG_PATH <- "data/auto_update_log.csv"

in_hcmc_bbox <- function(lat, lon) {
  !is.na(lat) & !is.na(lon) &
    lat >= 10.30 & lat <= 11.20 &
    lon >= 106.00 & lon <= 107.30
}

load_data <- function() {
  if (file.exists(DATA_PATH)) {
    df <- read_csv(DATA_PATH, show_col_types = FALSE)
  } else if (file.exists(RAW_PATH)) {
    df <- read_csv(RAW_PATH, show_col_types = FALSE)
  } else {
    df <- tibble(
      title = character(), district_name = character(), category_name = character(),
      price = numeric(), area = numeric(), rooms = numeric(), ward = character(),
      posted_at = as.POSIXct(character()), lat = numeric(), lon = numeric(),
      price_per_m2 = numeric(), is_rent = logical(), ad_url = character()
    )
  }

  df %>%
    mutate(
      title = if_else(is.na(title) | title == "", "Tin bất động sản", as.character(title)),
      source = if ("source" %in% names(.)) if_else(is.na(source) | source == "", "Không rõ", as.character(source)) else "Không rõ",
      district_name = if_else(is.na(district_name) | district_name == "", "Không rõ", as.character(district_name)),
      category_name = if_else(is.na(category_name) | category_name == "", "Không rõ", as.character(category_name)),
      transaction_type = if ("transaction_type" %in% names(.)) {
        if_else(is.na(transaction_type) | transaction_type == "", if_else(as.logical(is_rent), "Cho thuê", "Bán"), as.character(transaction_type))
      } else {
        if_else(as.logical(is_rent), "Cho thuê", "Bán")
      },
      ward = if_else(is.na(ward) | ward == "", "Không rõ", as.character(ward)),
      price = as.numeric(price),
      area = as.numeric(area),
      rooms = as.numeric(rooms),
      price_m = price / 1e6,
      price_b = price / 1e9,
      price_per_m2 = if_else(!is.na(area) & area > 0, price / area, NA_real_),
      posted_at = suppressWarnings(as_datetime(posted_at)),
      is_rent = as.logical(is_rent),
      lat = if_else(in_hcmc_bbox(lat, lon), lat, NA_real_),
      lon = if_else(in_hcmc_bbox(lat, lon), lon, NA_real_)
    )
}

load_metrics <- function() {
  if (!file.exists(METRICS_PATH)) return(tibble())
  read_csv(METRICS_PATH, show_col_types = FALSE)
}

load_registry <- function() {
  if (!file.exists(REGISTRY_PATH)) return(tibble())
  read_csv(REGISTRY_PATH, show_col_types = FALSE)
}

load_update_status <- function() {
  if (!file.exists(UPDATE_LOG_PATH)) {
    return("Chưa có lịch sử cập nhật")
  }
  latest <- read_csv(UPDATE_LOG_PATH, show_col_types = FALSE) %>% slice_tail(n = 1)
  if (nrow(latest) == 0) return("Chưa có lịch sử cập nhật")
  status <- if (identical(latest$status[[1]], "success")) "Đã cập nhật" else "Lỗi cập nhật"
  retrain <- if (isTRUE(latest$retrained[[1]])) " · đã retrain" else " · giữ model cũ"
  paste0(status, " ", format(as_datetime(latest$updated_at[[1]]), "%d/%m %H:%M"), retrain)
}

format_vnd <- function(x) {
  vapply(x, function(value) {
    if (is.na(value) || !is.finite(value)) return("Chưa có dữ liệu")
    if (value >= 1e9) {
      paste0(format(round(value / 1e9, 2), big.mark = ","), " tỷ")
    } else {
      paste0(format(round(value / 1e6, 1), big.mark = ","), " triệu")
    }
  }, character(1))
}

format_vnd_full <- function(x) paste0(format_vnd(x), " VND")

listing_url <- function(ad_url) {
  ifelse(
    !is.na(ad_url) & ad_url != "",
    ifelse(grepl("\\.htm$", ad_url), ad_url, paste0(ad_url, ".htm")),
    NA_character_
  )
}

format_number_vi <- function(x, digits = 1) {
  format(round(x, digits), big.mark = ".", decimal.mark = ",", nsmall = digits, trim = TRUE)
}

price_display_info <- function(transaction_type) {
  if (identical(transaction_type, "Cho thuê")) {
    list(value_col = "price_m", axis = "Giá thuê (triệu VND)", unit = "triệu VND", digits = 1)
  } else {
    list(value_col = "price_b", axis = "Giá bán (tỷ VND)", unit = "tỷ VND", digits = 2)
  }
}

price_m2_display_info <- function(transaction_type) {
  if (identical(transaction_type, "Cho thuê")) {
    list(scale = 1e3, axis = "Nghìn VND/m²", unit = "nghìn VND/m²", digits = 1)
  } else {
    list(scale = 1e6, axis = "Triệu VND/m²", unit = "triệu VND/m²", digits = 1)
  }
}

feature_label_vi <- function(feature) {
  dplyr::recode(
    as.character(feature),
    area = "Diện tích",
    rooms = "Số phòng",
    posted_hour = "Giờ đăng tin",
    distance_to_center = "Khoảng cách tới trung tâm",
    ward_price_encoded = "Mặt bằng giá phường/xã",
    listing_age_days = "Tuổi tin đăng",
    district_name = "Khu vực",
    category_name = "Loại bất động sản",
    posted_wday = "Thứ đăng tin",
    source = "Nguồn dữ liệu",
    .default = as.character(feature)
  )
}

format_metric <- function(x) {
  vapply(x, function(value) {
    if (is.na(value) || !is.finite(value)) return("NA")
    format(round(value, 3), nsmall = 3)
  }, character(1))
}

active_or_all <- function(x, all_label = "Tất cả") {
  if (is.null(x) || length(x) == 0 || identical(x, "__all__")) all_label else paste(x, collapse = ", ")
}

safe_range <- function(x, default) {
  if (is.null(x) || length(x) < 2) default else x
}

is_selected_filter <- function(x) {
  !is.null(x) && length(x) > 0 && !identical(x, "__all__")
}

plot_sample <- function(df, max_n = 1600) {
  if (nrow(df) > max_n) dplyr::slice_sample(df, n = max_n) else df
}

best_model_label <- function(metrics) {
  if (nrow(metrics) == 0 || !"mape" %in% names(metrics)) return("Chưa có")
  best <- metrics %>% filter(segment == "sale") %>% arrange(mape) %>% slice(1)
  if (nrow(best) == 0) best <- metrics %>% arrange(mape) %>% slice(1)
  paste0(best$model[[1]], " · MAPE ", round(best$mape[[1]] * 100, 1), "%")
}

best_model_name_only <- function(metrics) {
  if (nrow(metrics) == 0 || !"mape" %in% names(metrics)) return("Chưa có")
  best <- metrics %>% filter(segment == "sale") %>% arrange(mape) %>% slice(1)
  if (nrow(best) == 0) best <- metrics %>% arrange(mape) %>% slice(1)
  best$model[[1]]
}

best_model_mape_only <- function(metrics) {
  if (nrow(metrics) == 0 || !"mape" %in% names(metrics)) return(NULL)
  best <- metrics %>% filter(segment == "sale") %>% arrange(mape) %>% slice(1)
  if (nrow(best) == 0) best <- metrics %>% arrange(mape) %>% slice(1)
  paste0("MAPE ", round(best$mape[[1]] * 100, 1), "%")
}

best_model_from_bundle <- function(bundle) {
  if (!is.null(bundle$best_model) && !is.na(bundle$best_model)) {
    return(bundle$best_model)
  }
  "Random Forest"
}

model_label_vi <- function(model_name) {
  dplyr::case_when(
    identical(model_name, "Linear Regression") ~ "Linear Regression",
    identical(model_name, "Random Forest") ~ "Random Forest",
    identical(model_name, "XGBoost") ~ "XGBoost",
    identical(model_name, "RF + XGBoost Ensemble") ~ "RF + XGBoost Ensemble",
    TRUE ~ as.character(model_name)
  )
}

prepare_prediction_for_bundle <- function(input_row, bundle) {
  if (!is.null(bundle$ward_encoding)) {
    input_row <- input_row %>%
      select(-any_of("ward_price_encoded")) %>%
      left_join(
        bundle$ward_encoding$table %>% select(ward, encoded_value),
        by = "ward"
      ) %>%
      mutate(ward_price_encoded = coalesce(encoded_value, bundle$ward_encoding$global)) %>%
      select(-encoded_value)
  }

  if (!is.null(bundle$factor_levels)) {
    for (col in names(bundle$factor_levels)) {
      if (col %in% names(input_row)) {
        value <- as.character(input_row[[col]])
        valid_levels <- bundle$factor_levels[[col]]
        value[is.na(value) | !(value %in% valid_levels)] <- valid_levels[[1]]
        input_row[[col]] <- factor(value, levels = valid_levels)
      }
    }
  }

  input_row
}

make_xgb_prediction_matrix <- function(formula, input_row, feature_names) {
  rhs_formula <- delete.response(terms(formula))
  x <- sparse.model.matrix(rhs_formula, data = input_row)[, -1, drop = FALSE]
  missing_cols <- setdiff(feature_names, colnames(x))
  if (length(missing_cols) > 0) {
    zeros <- Matrix::sparseMatrix(
      i = integer(0),
      j = integer(0),
      dims = c(nrow(x), length(missing_cols))
    )
    colnames(zeros) <- missing_cols
    x <- cbind(x, zeros)
  }
  extra_cols <- setdiff(colnames(x), feature_names)
  if (length(extra_cols) > 0) {
    x <- x[, setdiff(colnames(x), extra_cols), drop = FALSE]
  }
  x[, feature_names, drop = FALSE]
}

predict_log_with_model <- function(bundle, model_name, input_row) {
  pred <- switch(
    model_name,
    "Linear Regression" = predict(bundle$lm, newdata = input_row),
    "Random Forest" = predict(bundle$random_forest, newdata = input_row),
    "XGBoost" = {
      x <- make_xgb_prediction_matrix(bundle$formula, input_row, bundle$xgb_feature_names)
      predict(bundle$xgboost, x)
    },
    NA_real_
  )
  as.numeric(pred)
}

predict_price <- function(input_row, is_rent) {
  model_path <- if (is_rent) RENT_MODEL_PATH else SALE_MODEL_PATH
  if (!file.exists(model_path)) return(NA_real_)

  bundle <- readRDS(model_path)
  input_row <- prepare_prediction_for_bundle(input_row, bundle)
  model_name <- best_model_from_bundle(bundle)

  pred_log <- tryCatch({
    if (identical(model_name, "RF + XGBoost Ensemble")) {
      rf_pred <- predict_log_with_model(bundle, "Random Forest", input_row)
      xgb_pred <- predict_log_with_model(bundle, "XGBoost", input_row)
      mean(c(rf_pred, xgb_pred), na.rm = TRUE)
    } else {
      predict_log_with_model(bundle, model_name, input_row)
    }
  }, error = function(e) NA_real_)

  if (!is.null(bundle$train_log_bounds) && length(bundle$train_log_bounds) == 2) {
    pred_log <- pmin(pmax(pred_log, bundle$train_log_bounds[[1]]), bundle$train_log_bounds[[2]])
  }

  expm1(as.numeric(pred_log))
}

prediction_model_label <- function(is_rent) {
  model_path <- if (is_rent) RENT_MODEL_PATH else SALE_MODEL_PATH
  if (!file.exists(model_path)) return("Chưa có model")
  bundle <- readRDS(model_path)
  model_label_vi(best_model_from_bundle(bundle))
}

build_prediction_row <- function(df, district, category, ward, area, rooms, transaction_type) {
  is_rent <- identical(transaction_type, "Cho thuê")
  segment_df <- df %>% filter(is_rent == !!is_rent)
  if (nrow(segment_df) == 0) segment_df <- df

  local_df <- segment_df %>%
    filter(district_name == district, category_name == category)
  if (nrow(local_df) == 0) {
    local_df <- segment_df %>% filter(district_name == district)
  }
  if (nrow(local_df) == 0) local_df <- segment_df

  mode_chr <- function(x, default = "Không rõ") {
    x <- as.character(x[!is.na(x) & x != ""])
    if (length(x) == 0) default else names(sort(table(x), decreasing = TRUE))[1]
  }

  tibble(
    district_name = district,
    category_name = category,
    ward = if_else(is.na(ward) | ward == "", "Không rõ", ward),
    source = mode_chr(local_df$source, "chotot"),
    area = as.numeric(area),
    rooms = as.numeric(rooms),
    posted_hour = hour(Sys.time()),
    posted_wday = wday(Sys.time(), label = TRUE),
    listing_age_days = 0,
    distance_to_center = median(local_df$distance_to_center, na.rm = TRUE),
    ward_price_encoded = median(local_df$ward_price_encoded, na.rm = TRUE),
    is_rent = is_rent,
    transaction_type = transaction_type
  ) %>%
    mutate(
      distance_to_center = if_else(is.na(distance_to_center), median(df$distance_to_center, na.rm = TRUE), distance_to_center),
      distance_to_center = if_else(is.na(distance_to_center), 0, distance_to_center),
      ward_price_encoded = if_else(is.na(ward_price_encoded), median(df$ward_price_encoded, na.rm = TRUE), ward_price_encoded),
      ward_price_encoded = if_else(is.na(ward_price_encoded), 0, ward_price_encoded)
    )
}

price_color <- function(price) {
  dplyr::case_when(
    is.na(price) ~ "#64748b",
    price < 3e9 ~ "#059669",
    price < 8e9 ~ "#d97706",
    TRUE ~ "#ed1c24"
  )
}

kpi_card <- function(label, value, hint = NULL, icon = "chart-line", tone = "default", delta = NULL, value_class = "") {
  div(
    class = paste("kpi-card", paste0("tone-", tone)),
    div(
      class = "kpi-top",
      div(class = "kpi-label", label),
      div(class = "kpi-icon", icon(icon))
    ),
    div(class = paste("kpi-value", value_class), value),
    div(
      class = "kpi-foot",
      if (!is.null(delta)) span(class = "kpi-delta", delta),
      if (!is.null(hint)) span(class = "kpi-hint", hint)
    )
  )
}

app_panel <- function(title, subtitle = NULL, ..., class = "") {
  div(
    class = paste("app-panel", class),
    div(
      class = "panel-head",
      div(
        div(class = "panel-title", title),
        if (!is.null(subtitle)) div(class = "panel-subtitle", subtitle)
      )
    ),
    div(class = "panel-body", ...)
  )
}

filter_field <- function(label, ..., icon_name = NULL, class = "") {
  div(
    class = paste("filter-field", class),
    div(
      class = "filter-field-label",
      if (!is.null(icon_name)) icon(icon_name),
      span(label)
    ),
    ...
  )
}

filter_actions <- function(reset_id) {
  div(
    class = "filter-actions",
    actionButton(reset_id, label = tagList(icon("rotate-left"), span("Đặt lại")), class = "btn-filter-reset")
  )
}

filter_select <- function(input_id, choices, all_label) {
  selectInput(
    input_id,
    NULL,
    choices = c(setNames("__all__", all_label), setNames(choices, choices)),
    selected = "__all__",
    selectize = FALSE
  )
}

chart_mode_control <- function(input_id, selected = "Bán") {
  div(
    class = "chart-mode",
    span(class = "chart-mode-label", "Hiển thị"),
    radioButtons(
      input_id,
      label = NULL,
      choices = c("Bán", "Cho thuê"),
      selected = selected,
      inline = TRUE
    )
  )
}

chart_palette <- c("#0072bc", "#10b981", "#f59e0b", "#ef4444", "#00a8e8")

interactive_chart <- function(plot, tooltip = "all") {
  ggplotly(plot, tooltip = tooltip) %>%
    layout(
      font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
      hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#d7e6f5", font = list(color = "#1f2937")),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    ) %>%
    config(displayModeBar = FALSE, responsive = TRUE)
}

app_css <- HTML("
:root {
  --radius: 16px;
  --bg: #f4f7fb;
  --fg: #1e293b;
  --muted: #64748b;
  --card: #ffffff;
  --border: #e2e8f0;
  --primary: #0072bc;
  --primary-dark: #005a94;
  --primary-alpha: rgba(0, 114, 188, 0.08);
  --sidebar: #ffffff;
  --sidebar-hover: #f8fafc;
  --success: #10b981;
  --warning: #f59e0b;
  --danger: #ef4444;
  --chart1: #0072bc;
  --chart2: #10b981;
  --chart3: #f59e0b;
  --chart4: #ef4444;
  --chart5: #00a8e8;
}

/* =========================================================
   SENIOR DEV GRID FIXES (Kill Bootstrap 3 float bugs)
   ========================================================= */
.row {
  display: flex !important;
  flex-wrap: wrap !important;
  margin-left: -12px !important;
  margin-right: -12px !important;
}
.row::before, .row::after {
  display: none !important; 
}
[class*='col-'] {
  display: flex !important;
  flex-direction: column !important;
  padding-left: 12px !important;
  padding-right: 12px !important;
  min-width: 0; /* Prevent flex children from bursting */
}

/* Base Styles */
body {
  font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Arial, sans-serif;
  font-feature-settings: 'cv11', 'ss01';
  background: var(--bg);
  color: var(--fg);
  margin: 0;
  padding: 0;
}
html, body, .container-fluid { min-height: 100%; }

.app-shell { display: flex; min-height: 100vh; width: 100%; background: var(--bg); }
.app-sidebar {
  position: sticky; top: 0; width: 260px; min-width: 260px; height: 100vh;
  display: flex; flex-direction: column; background: var(--sidebar); color: var(--fg);
  border-right: 1px solid rgba(226, 232, 240, 0.8); box-shadow: 4px 0 24px rgba(15, 23, 42, 0.02); z-index: 30;
}

/* Brand */
.app-brand { display: flex; flex-direction: column; align-items: center; justify-content: center; padding: 32px 20px 24px; text-align: center; gap: 16px; }
.brand-logo { display: flex; align-items: center; justify-content: center; width: 120px; height: auto; transition: transform 0.3s cubic-bezier(0.34, 1.56, 0.64, 1); }
.brand-logo:hover { transform: scale(1.08) translateY(-2px); }
.brand-logo img { width: 100%; height: auto; object-fit: contain; filter: drop-shadow(0 8px 16px rgba(0, 114, 188, 0.15)); }
.brand-text { display: flex; flex-direction: column; gap: 4px; }
.brand-title { color: var(--primary-dark); font-size: 14px; font-weight: 800; text-transform: uppercase; letter-spacing: 0.06em; }
.brand-subtitle { color: var(--muted); font-size: 12px; font-weight: 600; }

/* Navigation */
.nav-section-label { padding: 24px 24px 12px; font-size: 11px; font-weight: 700; letter-spacing: 0.1em; color: #94a3b8; text-transform: uppercase; }
.app-nav-link {
  display: flex; align-items: center; gap: 14px; min-height: 44px; margin: 4px 16px; padding: 0 16px;
  color: #64748b; text-decoration: none !important; border-radius: 12px; font-size: 14px; font-weight: 600; transition: all 0.2s ease;
}
.app-nav-link:hover, .app-nav-link:focus { background: var(--sidebar-hover); color: var(--primary); transform: translateX(4px); }
.app-nav-link.active { background: var(--primary-alpha); color: var(--primary); font-weight: 700; }
.app-nav-link i { width: 20px; text-align: center; font-size: 16px; color: #94a3b8; transition: color 0.2s ease, transform 0.2s ease; }
.app-nav-link:hover i, .app-nav-link.active i { color: var(--primary); transform: scale(1.1); }
.app-sidebar-footer { margin-top: auto; padding: 24px; color: #94a3b8; font-size: 12px; line-height: 1.6; text-align: center; }

/* Topbar */
.app-main { min-width: 0; flex: 1; display: flex; flex-direction: column; }
.app-topbar {
  position: sticky; top: 0; z-index: 100; height: 72px; display: flex; align-items: center; padding: 0 32px;
  background: rgba(255, 255, 255, 0.85); backdrop-filter: blur(20px); -webkit-backdrop-filter: blur(20px);
  border-bottom: 1px solid rgba(226, 232, 240, 0.6); box-shadow: 0 4px 20px rgba(15, 23, 42, 0.02);
}
.topbar-title-wrap { min-width: 0; display: flex; flex-direction: column; justify-content: center; }
.topbar-title { color: var(--fg); font-size: 18px; font-weight: 800; line-height: 1.2; letter-spacing: -0.01em; }
.topbar-subtitle { margin-top: 4px; color: var(--muted); font-size: 13px; font-weight: 500; }
.status-badge { display: inline-flex; align-items: center; gap: 8px; padding: 6px 14px; border-radius: 999px; background: rgba(16, 185, 129, 0.1); border: 1px solid rgba(16, 185, 129, 0.2); color: #059669; font-size: 12px; font-weight: 700; }
.status-dot { width: 8px; height: 8px; border-radius: 50%; background: var(--success); box-shadow: 0 0 0 3px rgba(16, 185, 129, 0.2); animation: pulse 2s infinite; }
.topbar-actions { display: flex; align-items: center; gap: 16px; min-width: 0; }
.btn-refresh-data {
  min-height: 40px; display: inline-flex; align-items: center; gap: 8px; padding: 0 16px;
  border: 1px solid rgba(0, 114, 188, 0.2); border-radius: 10px; background: #ffffff; color: var(--primary);
  font-size: 13px; font-weight: 700; box-shadow: 0 2px 8px rgba(15, 23, 42, 0.04); transition: all 0.2s ease;
}
.btn-refresh-data:hover, .btn-refresh-data:focus { background: var(--primary-alpha); border-color: rgba(0, 114, 188, 0.4); transform: translateY(-1px); box-shadow: 0 4px 12px rgba(0, 114, 188, 0.15); }
.refresh-status { max-width: 260px; color: var(--muted); font-size: 13px; font-weight: 600; }

@keyframes pulse { 0% { box-shadow: 0 0 0 0 rgba(16, 185, 129, 0.4); } 70% { box-shadow: 0 0 0 6px rgba(16, 185, 129, 0); } 100% { box-shadow: 0 0 0 0 rgba(16, 185, 129, 0); } }

/* Main Content */
.app-content { flex: 1; min-width: 0; }
.app-content > .tabbable > .nav { display: none; }
.app-content .tab-content { border: 0; padding: 0; }
.app-content .tab-pane { padding: 0; }
.page-wrap { padding: 32px 40px 48px; max-width: 1600px; margin: 0 auto; }
.page-title { font-size: 28px; font-weight: 800; color: var(--fg); margin: 0; letter-spacing: -0.02em; }
.page-subtitle { color: var(--muted); font-size: 15px; margin: 8px 0 24px; line-height: 1.5; max-width: 800px; }

/* KPIs */
.kpi-grid { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 24px; margin-bottom: 24px; }
.kpi-card {
  background: var(--card); border: 1px solid rgba(255, 255, 255, 0.6); border-radius: var(--radius); padding: 24px;
  box-shadow: 0 4px 20px rgba(15, 23, 42, 0.03), inset 0 0 0 1px rgba(226, 232, 240, 0.5);
  transition: all 0.3s cubic-bezier(0.34, 1.56, 0.64, 1); position: relative; overflow: hidden;
}
.kpi-card::before {
  content: ''; position: absolute; top: 0; left: 0; width: 100%; height: 4px;
  background: linear-gradient(90deg, var(--primary), #00a8e8); opacity: 0; transition: opacity 0.3s ease;
}
.kpi-card:hover { transform: translateY(-4px); box-shadow: 0 20px 40px rgba(15, 23, 42, 0.08), inset 0 0 0 1px rgba(226, 232, 240, 0.8); z-index: 10; }
.kpi-card:hover::before { opacity: 1; }
.kpi-top { display: flex; justify-content: space-between; gap: 12px; align-items: flex-start; }
.kpi-label { font-size: 12px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.08em; color: var(--muted); }
.kpi-icon { width: 44px; height: 44px; display: flex; align-items: center; justify-content: center; border-radius: 12px; background: var(--primary-alpha); color: var(--primary); font-size: 20px; }
.tone-success .kpi-icon { background: rgba(16, 185, 129, 0.1); color: var(--success); }
.tone-warning .kpi-icon { background: rgba(245, 158, 11, 0.1); color: var(--warning); }
.tone-danger .kpi-icon { background: rgba(239, 68, 68, 0.1); color: var(--danger); }
.kpi-value { margin-top: auto; padding-top: 16px; font-size: 34px; line-height: 1.1; font-weight: 800; color: var(--fg); font-variant-numeric: tabular-nums; letter-spacing: -0.02em; word-break: break-word; }
.kpi-value.text-mode { font-size: 22px; line-height: 1.3; }
.kpi-foot { margin-top: 16px; padding-top: 16px; display: flex; flex-wrap: wrap; align-items: center; gap: 10px; min-height: 20px; font-size: 13px; color: var(--muted); border-top: 1px dashed rgba(226, 232, 240, 0.8); }
.kpi-delta { color: var(--success); background: rgba(16, 185, 129, 0.1); padding: 4px 8px; border-radius: 6px; font-weight: 700; font-size: 12px; white-space: nowrap; }

/* Panels */
.app-panel {
  background: var(--card); border: 1px solid rgba(255, 255, 255, 0.6); border-radius: var(--radius);
  box-shadow: 0 4px 20px rgba(15, 23, 42, 0.03), inset 0 0 0 1px rgba(226, 232, 240, 0.5); 
  margin-bottom: 24px; transition: all 0.3s ease;
  flex: 1; display: flex; flex-direction: column; width: 100%;
  overflow: visible !important; /* CRITICAL FIX: DO NOT HIDE DROPDOWNS */
}
.app-panel:hover { box-shadow: 0 12px 32px rgba(15, 23, 42, 0.06), inset 0 0 0 1px rgba(226, 232, 240, 0.8); z-index: 40; }
.app-panel.nested-panel { box-shadow: none; border: 1px solid var(--border); margin-bottom: 0; }
.app-panel.nested-panel:hover { transform: none; box-shadow: none; }

/* Filter Card Specially Elevated */
.app-panel.filter-card { position: relative; z-index: 90; }
.app-panel.filter-card:hover { transform: none; box-shadow: 0 4px 20px rgba(15, 23, 42, 0.03); z-index: 95; }

.panel-head {
  padding: 20px 24px; border-bottom: 1px solid rgba(226, 232, 240, 0.6); display: flex; justify-content: space-between; gap: 16px; background: rgba(248, 250, 252, 0.5);
  border-top-left-radius: var(--radius); border-top-right-radius: var(--radius);
}
.panel-title { font-size: 16px; font-weight: 800; color: var(--fg); letter-spacing: -0.01em; }
.panel-subtitle { font-size: 13px; color: var(--muted); margin-top: 4px; font-weight: 500; }
.panel-body { padding: 24px; flex: 1; display: flex; flex-direction: column; min-height: 0; }

/* Chart Containers */
.html-widget { flex: 1 !important; width: 100% !important; min-height: 300px; }

/* Forms & Filters */
.filter-panel { display: flex; flex-direction: column; gap: 18px; }
.chart-mode { display: inline-flex; align-items: center; gap: 10px; min-height: 36px; margin: 0 0 12px; padding: 4px 12px; border: 1px solid rgba(226, 232, 240, 0.8); border-radius: 10px; background: #f8fafc; }
.chart-mode-label { color: #64748b; font-size: 12px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.06em; }
.chart-mode .form-group { margin: 0; }
.chart-mode .radio-inline { min-height: 24px; margin: 0 0 0 8px; padding-left: 24px; color: #334155; font-size: 13px; font-weight: 600; }
.chart-mode input[type='radio'] { margin-top: 4px; }
.filter-toolbar { display: grid; grid-template-columns: 1fr 1.2fr 1.2fr .95fr .95fr auto; gap: 16px; align-items: end; }
.filter-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 16px; }
.filter-field { min-width: 0; }
.filter-field .form-group { margin-bottom: 0; }
.filter-field > .form-group > label, .filter-field label.control-label, .predict-form label.control-label { display: none; }
.filter-field-label { display: flex; align-items: center; gap: 8px; min-height: 20px; margin-bottom: 10px; color: #475569; font-size: 12px; font-weight: 700; letter-spacing: 0.05em; text-transform: uppercase; }
.filter-field-label i { width: 16px; color: var(--primary); text-align: center; }
.filter-summary { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 12px; border: 1px solid var(--border); background: #f8fafc; border-radius: 12px; padding: 14px; }
.filter-summary.full { grid-template-columns: repeat(3, minmax(0, 1fr)); }
.filter-chip { min-width: 0; padding: 12px; border: 1px solid rgba(226, 232, 240, 0.8); border-radius: 10px; background: #ffffff; box-shadow: 0 2px 6px rgba(15, 23, 42, 0.02); }
.filter-chip-label { color: var(--muted); font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.05em; }
.filter-chip-value { margin-top: 6px; color: var(--fg); font-size: 16px; font-weight: 800; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.filter-actions { display: flex; justify-content: flex-end; align-items: flex-end; }
.filter-toolbar > .filter-actions { grid-column: -2 / -1; }

.btn-filter-reset {
  min-height: 42px; padding: 0 16px; border: 1px solid rgba(226, 232, 240, 0.8); border-radius: 10px;
  background: #ffffff; color: #475569; font-size: 13px; font-weight: 700; box-shadow: 0 2px 6px rgba(15, 23, 42, 0.02); transition: all 0.2s ease;
}
.btn-filter-reset:hover, .btn-filter-reset:focus { border-color: rgba(0, 114, 188, 0.3); color: var(--primary); background: var(--primary-alpha); transform: translateY(-1px); }
.form-control, .selectize-input { min-height: 44px; border: 1px solid #cbd5e1; border-radius: 10px; box-shadow: 0 1px 2px rgba(15, 23, 42, 0.02); color: var(--fg); font-size: 14px; line-height: 22px; transition: all 0.2s ease; }
.filter-field select.form-control, .predict-form select.form-control {
  appearance: none; -webkit-appearance: none; cursor: pointer; padding: 10px 36px 10px 14px; background-color: #ffffff;
  background-image: linear-gradient(45deg, transparent 50%, #64748b 50%), linear-gradient(135deg, #64748b 50%, transparent 50%);
  background-position: calc(100% - 20px) 19px, calc(100% - 15px) 19px; background-size: 5px 5px, 5px 5px; background-repeat: no-repeat;
}
.filter-field select.form-control:hover, .predict-form select.form-control:hover { border-color: #94a3b8; }
.filter-field select.form-control:focus, .predict-form select.form-control:focus, .filter-field .form-control:focus, .predict-form .form-control:focus { border-color: var(--primary); box-shadow: 0 0 0 4px rgba(0, 114, 188, 0.15); outline: none; background-color: #ffffff; }

/* Selectize Superior z-index */
.selectize-input { display: flex !important; align-items: center; gap: 6px; padding: 9px 36px 9px 14px; }
.selectize-control.multi .selectize-input:after, .selectize-control.single .selectize-input:after {
  content: ''; position: absolute; right: 16px; top: 50%; width: 0; height: 0; margin-top: -3px;
  border-left: 6px solid transparent; border-right: 6px solid transparent; border-top: 6px solid #64748b; transition: transform 0.2s ease;
}
.selectize-control.dropdown-active .selectize-input:after { transform: rotate(180deg); border-top-color: var(--primary); }
.selectize-input.focus { border-color: var(--primary); box-shadow: 0 0 0 4px rgba(0, 114, 188, 0.15); background: #ffffff; }
.selectize-input input { font-size: 14px !important; line-height: 22px !important; color: var(--fg) !important; }
.selectize-dropdown {
  z-index: 99999 !important; border: 1px solid #cbd5e1; border-radius: 12px;
  box-shadow: 0 16px 40px rgba(15, 23, 42, 0.15); margin-top: 8px; color: var(--fg); font-size: 14px; overflow: hidden;
}
.selectize-dropdown .option { padding: 10px 14px; line-height: 1.4; cursor: pointer; transition: background 0.1s; }
.selectize-dropdown .active { background: var(--primary-alpha); color: var(--primary-dark); font-weight: 600; }
.selectize-dropdown .selected { background: #f8fafc; color: #64748b; }

.irs--shiny { height: 48px; }
.irs--shiny .irs-line { height: 8px; border: 0; background: #e2e8f0; border-radius: 4px; }
.irs--shiny .irs-bar { height: 8px; background: var(--primary); border-color: var(--primary); }
.irs--shiny .irs-handle { top: 18px; width: 20px; height: 20px; border: 4px solid #ffffff; background: var(--primary); box-shadow: 0 2px 8px rgba(15, 23, 42, 0.2); border-radius: 50%; }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: var(--primary-dark); border-color: var(--primary-dark); border-radius: 6px; font-size: 12px; font-weight: 700; padding: 2px 6px; }

.btn-primary { background: linear-gradient(135deg, var(--primary), #00a8e8); border: none; border-radius: 10px; font-weight: 700; color: white; box-shadow: 0 4px 12px rgba(0, 114, 188, 0.25); transition: all 0.2s ease; }
.btn-primary:hover { background: linear-gradient(135deg, #005a94, #0077b6); box-shadow: 0 6px 16px rgba(0, 114, 188, 0.35); transform: translateY(-1px); }

.predict-form { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 16px; }
.predict-form .wide { grid-column: 1 / -1; }
.predict-form .action-button { min-height: 48px; font-size: 15px; }
.prediction-hero {
  border-radius: var(--radius); border: 1px solid rgba(0, 114, 188, 0.15);
  background: linear-gradient(135deg, rgba(0, 114, 188, 0.05), #ffffff);
  padding: 32px; text-align: center; box-shadow: 0 8px 24px rgba(0, 114, 188, 0.06);
}
.prediction-label { font-size: 14px; text-transform: uppercase; letter-spacing: 0.1em; color: var(--muted); font-weight: 800; margin-bottom: 12px; }
.prediction-value { color: transparent; background: linear-gradient(135deg, var(--primary), #00a8e8); -webkit-background-clip: text; background-clip: text; font-size: 46px; line-height: 1.1; font-weight: 800; font-variant-numeric: tabular-nums; }
.prediction-note { margin-top: 12px; color: #64748b; font-size: 14px; font-weight: 500; }

.map-shell { position: relative; overflow: hidden; border-radius: var(--radius); border: 1px solid var(--border); box-shadow: 0 4px 20px rgba(15, 23, 42, 0.04); flex: 1; min-height: 600px; }
.map-legend { position: absolute; z-index: 500; bottom: 20px; left: 20px; background: rgba(255, 255, 255, 0.95); backdrop-filter: blur(8px); border: 1px solid rgba(226, 232, 240, 0.8); border-radius: 10px; padding: 12px 16px; font-size: 13px; font-weight: 500; box-shadow: 0 8px 24px rgba(15, 23, 42, 0.1); }
.dot { display: inline-block; width: 12px; height: 12px; border-radius: 50%; margin-right: 6px; vertical-align: middle; }
.dot-low { background: var(--success); } .dot-mid { background: var(--warning); } .dot-high { background: var(--danger); }

.dataTables_wrapper { font-size: 14px; }
.table > thead > tr > th { background: #f8fafc; color: #334155; border-bottom: 2px solid var(--border); font-weight: 700; padding: 12px; }
.table > tbody > tr > td { padding: 12px; border-bottom: 1px solid #f1f5f9; }

@media (max-width: 1200px) {
  .kpi-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
  .filter-toolbar { grid-template-columns: repeat(3, minmax(0, 1fr)); }
}
@media (max-width: 900px) {
  .app-shell { flex-direction: column; }
  .app-sidebar { position: relative; width: 100%; min-width: 0; height: auto; border-right: none; border-bottom: 1px solid var(--border); box-shadow: none; z-index: 100; }
  .app-brand { flex-direction: row; padding: 16px 24px; justify-content: flex-start; text-align: left; }
  .brand-logo { width: 64px; }
  .nav-section-label, .app-sidebar-footer { display: none; }
  .app-sidebar nav { display: flex; overflow-x: auto; -webkit-overflow-scrolling: touch; padding: 12px 24px; gap: 10px; }
  .app-nav-link { margin: 0; padding: 10px 20px; border-radius: 999px; background: #f1f5f9; white-space: nowrap; min-height: 44px; display: flex; align-items: center; }
  .page-wrap { padding: 24px; }
  .kpi-value { font-size: 28px; }
  .kpi-value.text-mode { font-size: 20px; }
}
@media (max-width: 640px) {
  .kpi-grid { grid-template-columns: 1fr; }
  .filter-toolbar { grid-template-columns: 1fr; }
  .filter-grid, .filter-summary.full { grid-template-columns: 1fr; }
  .page-wrap { padding: 16px; }
  .kpi-value { font-size: 28px; }
  .prediction-value { font-size: 36px; }
  .topbar-subtitle { display: none; }
  .app-topbar { padding: 0 20px; height: 64px; }
  .refresh-status, .btn-refresh-data span { display: none; }
}
")

nav_link <- function(id, label, icon_name) {
  actionLink(
    inputId = paste0("nav_", id),
    label = tagList(icon(icon_name), span(label)),
    class = "app-nav-link"
  )
}

ui <- fluidPage(
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("BĐS TP.HCM — Phân tích & dự đoán giá bất động sản"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&family=JetBrains+Mono:wght@500;600;700&display=swap"
    ),
    tags$style(app_css),
    tags$script(HTML("
      $(document).on('shiny:value', function(e) {
        if (e.name === 'tabs') {
          var tab = e.value;
          $('.app-nav-link').removeClass('active');
          $('#nav_' + tab).addClass('active');
        }
      });
      // Set active on nav click immediately
      $(document).on('click', '.app-nav-link', function() {
        $('.app-nav-link').removeClass('active');
        $(this).addClass('active');
      });
      // Set initial active state
      $(document).on('shiny:connected', function() {
        $('#nav_overview').addClass('active');
      });
    "))
  ),
  div(
    class = "app-shell",
    tags$aside(
      class = "app-sidebar",
      div(
        class = "app-brand",
        div(class = "brand-logo", tags$img(src = "hcmute-logo.png", alt = "HCM-UTE")),
        div(
          class = "brand-text",
          div(class = "brand-title", "Môn: Lập Trình R"),
          div(class = "brand-subtitle", "Đồ án Cuối kỳ")
        )
      ),
      div(class = "nav-section-label", "BẢNG ĐIỀU KHIỂN"),
      nav_link("overview", "Tổng quan", "chart-line"),
      nav_link("map", "Bản đồ dữ liệu", "map-location-dot"),
      nav_link("analysis", "Phân tích giá", "chart-column"),
      nav_link("predict", "Dự đoán giá", "calculator"),
      nav_link("clusters", "Phân cụm khu vực", "layer-group"),
      nav_link("data", "Dữ liệu", "table"),
      nav_link("about", "Về đồ án", "graduation-cap"),
      div(class = "app-sidebar-footer", "Nguồn dữ liệu: Chợ Tốt + Alonhadat", br(), "© 2026 HCMUTE")
    ),
    div(
      class = "app-main",
      tags$header(
        class = "app-topbar",
        div(
          class = "topbar-title-wrap",
          div(class = "topbar-title", "Hệ thống Phân tích & Dự đoán Giá Bất động sản"),
          div(class = "topbar-subtitle", "Dữ liệu thị trường TP.HCM")
        ),
        div(style = "flex: 1;"),
        div(
          class = "topbar-actions",
          div(class = "refresh-status", textOutput("refresh_status", inline = TRUE)),
          actionButton("refresh_data", label = tagList(icon("rotate"), span("Làm mới dữ liệu")), class = "btn-refresh-data"),
          span(class = "status-badge", span(class = "status-dot"), "Pipeline sẵn sàng")
        )
      ),
      tags$main(
        class = "app-content",
        tabsetPanel(
          id = "tabs",
          type = "hidden",
          tabPanel(
            title = "overview", value = "overview",
            div(
              class = "page-wrap",
              h1(class = "page-title", "Tổng quan thị trường"),
              div(class = "page-subtitle", "Snapshot dữ liệu thu thập từ nhiều nguồn cho TP.HCM, cập nhật theo pipeline R."),
              uiOutput("kpi_cards"),
              fluidRow(
                column(8, app_panel("Số tin theo quận/huyện", "Top khu vực có nhiều tin đăng nhất", chart_mode_control("district_plot_tx"), plotlyOutput("district_plot", height = 330))),
                column(4, app_panel("Cơ cấu loại bất động sản", "Tỉ trọng theo số lượng tin", chart_mode_control("category_plot_tx"), plotlyOutput("category_plot", height = 330)))
              ),
              app_panel("Hiệu năng mô hình", "So sánh RMSE / MAE / MAPE / R² giữa các mô hình", tableOutput("metrics_table"))
            )
          ),
          tabPanel(
            title = "map", value = "map",
            div(
              class = "page-wrap",
              h1(class = "page-title", "Bản đồ dữ liệu"),
              div(class = "page-subtitle", "Bản đồ tương tác: màu marker thể hiện mức giá, click marker để xem chi tiết tin đăng."),
              app_panel(
                "Bộ lọc bản đồ",
                uiOutput("map_filter_summary"),
                div(
                  class = "filter-toolbar",
                  filter_field("Nguồn", uiOutput("map_source_filter"), icon_name = "database"),
                  filter_field("Giao dịch", uiOutput("map_transaction_filter"), icon_name = "tags"),
                  filter_field("Khu vực", uiOutput("map_district_filter"), icon_name = "location-dot"),
                  filter_field("Loại BĐS", uiOutput("map_category_filter"), icon_name = "building"),
                  filter_field("Khoảng giá", sliderInput("map_price_range", NULL, min = 0, max = 100, value = c(0, 100), step = 1, post = " tỷ"), icon_name = "coins"),
                  filter_field("Diện tích", sliderInput("map_area_range", NULL, min = 0, max = 1000, value = c(0, 1000), step = 10, post = " m²"), icon_name = "ruler-combined"),
                  filter_actions("reset_map_filters")
                ),
                class = "filter-card"
              ),
              div(
                class = "map-shell",
                leafletOutput("listing_map", height = 640),
                div(class = "map-legend", div(style = "font-weight:700;margin-bottom:4px;", "Mức giá"),
                    span(class = "dot dot-low"), "Thấp ",
                    span(class = "dot dot-mid", style = "margin-left:8px;"), "Trung bình ",
                    span(class = "dot dot-high", style = "margin-left:8px;"), "Cao")
              )
            )
          ),
          tabPanel(
            title = "analysis", value = "analysis",
            div(
              class = "page-wrap",
              h1(class = "page-title", "Phân tích giá"),
              div(class = "page-subtitle", "So sánh giá theo diện tích, khu vực và loại bất động sản."),
              fluidRow(
                column(3, app_panel("Bộ lọc", NULL, div(class = "filter-panel",
                  filter_field("Nguồn", uiOutput("source_filter"), icon_name = "database"),
                  filter_field("Giao dịch", uiOutput("transaction_filter"), icon_name = "tags"),
                  filter_field("Khu vực", uiOutput("district_filter"), icon_name = "location-dot"),
                  filter_field("Loại BĐS", uiOutput("category_filter"), icon_name = "building"),
                  filter_field("Khoảng giá", sliderInput("price_range", NULL, min = 0, max = 100, value = c(0, 100), step = 1, post = " tỷ"), icon_name = "coins"),
                  filter_field("Diện tích", sliderInput("area_range", NULL, min = 0, max = 1000, value = c(0, 1000), step = 10, post = " m²"), icon_name = "ruler-combined"),
                  filter_actions("reset_analysis_filters"),
                  uiOutput("filter_summary")), class = "filter-card")),
                column(9, app_panel("Diện tích vs Giá", "Mỗi điểm là một tin đăng, màu theo loại BĐS", chart_mode_control("area_price_tx"), plotlyOutput("area_price_plot", height = 430)))
              ),
              fluidRow(
                column(4, app_panel("Top quận/huyện theo giá/m²", "Đơn vị tự đổi theo bán hoặc cho thuê", chart_mode_control("price_m2_tx"), plotlyOutput("price_m2_plot", height = 310))),
                column(4, app_panel("Khoảng giá theo loại BĐS", "Điểm là giá trung vị, thanh ngang là vùng giá phổ biến", chart_mode_control("price_category_tx"), plotlyOutput("price_category_plot", height = 310))),
                column(4, app_panel("Phân phối giá", "Giá được chuẩn hóa để biểu đồ dễ quan sát hơn", chart_mode_control("log_price_tx"), plotlyOutput("log_price_plot", height = 310)))
              )
            )
          ),
          tabPanel(
            title = "predict", value = "predict",
            div(
              class = "page-wrap",
              h1(class = "page-title", "Dự đoán giá bất động sản"),
              div(class = "page-subtitle", "Form demo sử dụng model tốt nhất theo từng nhóm giao dịch."),
              fluidRow(
                column(4, app_panel("Thông tin bất động sản", "Nhập đặc trưng để mô hình dự đoán",
                  div(class = "predict-form",
                    filter_field("Quận/huyện", uiOutput("predict_district"), icon_name = "location-dot"),
                    filter_field("Loại BĐS", uiOutput("predict_category"), icon_name = "building"),
                    filter_field("Giao dịch", selectInput("predict_transaction", NULL, choices = c("Bán", "Cho thuê"), selected = "Bán", selectize = FALSE), icon_name = "tags", class = "wide"),
                    filter_field("Phường/xã", textInput("predict_ward", NULL, value = "Không rõ"), icon_name = "map-pin", class = "wide"),
                    filter_field("Diện tích", numericInput("predict_area", NULL, value = 75, min = 1, max = 5000), icon_name = "ruler-combined"),
                    filter_field("Số phòng", numericInput("predict_rooms", NULL, value = 2, min = 0, max = 20), icon_name = "bed"),
                    actionButton("predict_btn", "Tính lại dự đoán", icon = icon("calculator"), class = "btn-primary wide", width = "100%")
                  ), class = "filter-card")),
                column(8, app_panel("Kết quả dự đoán", textOutput("prediction_model_note", inline = TRUE),
                  div(class = "prediction-hero", div(class = "prediction-label", "Giá dự đoán"),
                      div(class = "prediction-value", textOutput("prediction_text", inline = TRUE)),
                      div(class = "prediction-note", textOutput("prediction_note", inline = TRUE))),
                  br(),
                  app_panel("Các yếu tố ảnh hưởng chính", "Feature importance từ Random Forest", plotlyOutput("importance_plot", height = 260), class = "nested-panel")))
              )
            )
          ),
          tabPanel(title = "clusters", value = "clusters", div(class = "page-wrap",
            h1(class = "page-title", "Phân cụm khu vực"),
            div(class = "page-subtitle", "K-Means theo giá/m², diện tích trung vị và số tin, tách riêng bán và cho thuê."),
            app_panel("K-Means clusters", "Bubble size thể hiện số tin, màu là cụm", chart_mode_control("cluster_tx"), plotlyOutput("cluster_plot", height = 500)))),
          tabPanel(title = "data", value = "data", div(class = "page-wrap",
            h1(class = "page-title", "Dữ liệu đã thu thập"),
            div(class = "page-subtitle", "Bảng dữ liệu sạch dùng cho EDA, ML và dashboard."),
            app_panel(
              "Bộ lọc dữ liệu",
              uiOutput("data_filter_summary"),
              div(
                class = "filter-toolbar",
                filter_field("Nguồn", uiOutput("data_source_filter"), icon_name = "database"),
                filter_field("Giao dịch", uiOutput("data_transaction_filter"), icon_name = "tags"),
                filter_field("Khu vực", uiOutput("data_district_filter"), icon_name = "location-dot"),
                filter_field("Loại BĐS", uiOutput("data_category_filter"), icon_name = "building"),
                filter_field("Khoảng giá", sliderInput("data_price_range", NULL, min = 0, max = 100, value = c(0, 100), step = 1, post = " tỷ"), icon_name = "coins"),
                filter_actions("reset_data_filters")
              ),
              class = "filter-card"
            ),
            app_panel("Bảng dữ liệu", "Có tìm kiếm nhanh trong bảng", DTOutput("data_table")))),
          tabPanel(title = "about", value = "about", div(class = "page-wrap",
            h1(class = "page-title", "Về đồ án"),
            div(class = "page-subtitle", "Pipeline phân tích và dự đoán giá bất động sản TP.HCM bằng R."),
            app_panel("Pipeline thực hiện", NULL, tags$ul(
              tags$li("Thu thập dữ liệu từ Chợ Tốt API và Alonhadat HTML, tách raw data theo nguồn rồi gộp về schema chung."),
              tags$li("Làm sạch dữ liệu, xử lý missing values, tạo log features, khoảng cách tới trung tâm và target encoding theo phường/xã."),
              tags$li("Phân tích khám phá bằng ggplot2 và leaflet: phân phối giá, giá/m², cơ cấu loại BĐS, phân bố địa lý."),
              tags$li("Huấn luyện Linear Regression, Random Forest, XGBoost, ensemble và K-Means clustering."),
              tags$li("Triển khai dashboard Shiny để demo trực tiếp kết quả phân tích, bản đồ và dự đoán."))),
            app_panel("Sơ đồ pipeline", "Xem chi tiết trong README.md và docs/diagrams", tags$pre("Chợ Tốt API + Alonhadat HTML → Raw theo nguồn → Combined CSV → Feature Engineering → EDA → ML Models → Shiny Dashboard"))))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$nav_overview, updateTabsetPanel(session, "tabs", selected = "overview"), ignoreInit = TRUE)
  observeEvent(input$nav_map, updateTabsetPanel(session, "tabs", selected = "map"), ignoreInit = TRUE)
  observeEvent(input$nav_analysis, updateTabsetPanel(session, "tabs", selected = "analysis"), ignoreInit = TRUE)
  observeEvent(input$nav_predict, updateTabsetPanel(session, "tabs", selected = "predict"), ignoreInit = TRUE)
  observeEvent(input$nav_clusters, updateTabsetPanel(session, "tabs", selected = "clusters"), ignoreInit = TRUE)
  observeEvent(input$nav_data, updateTabsetPanel(session, "tabs", selected = "data"), ignoreInit = TRUE)
  observeEvent(input$nav_about, updateTabsetPanel(session, "tabs", selected = "about"), ignoreInit = TRUE)

  data_tick <- reactiveTimer(15 * 60 * 1000)
  refresh_version <- reactiveVal(0L)

  listings <- reactive({
    data_tick()
    refresh_version()
    load_data()
  })

  metrics <- reactive({
    refresh_version()
    load_metrics()
  })

  registry <- reactive({
    refresh_version()
    load_registry()
  })

  output$refresh_status <- renderText({
    refresh_version()
    load_update_status()
  })

  observeEvent(input$refresh_data, {
    withProgress(message = "Đang cập nhật dữ liệu", value = 0.1, {
      incProgress(0.2, detail = "Thu thập tin mới và tạo lại features")
      result <- tryCatch(
        system2("Rscript", "scripts/auto_update.R", stdout = TRUE, stderr = TRUE),
        error = function(e) structure(conditionMessage(e), status = 1)
      )
      status <- attr(result, "status")
      if (is.null(status)) status <- 0

      incProgress(0.7, detail = "Nạp lại dashboard")
      refresh_version(refresh_version() + 1L)

      if (identical(status, 0)) {
        showNotification("Đã làm mới dữ liệu. Model sẽ tự retrain khi đủ điều kiện.", type = "message", duration = 6)
      } else {
        showNotification("Cập nhật dữ liệu chưa thành công. Xem data/auto_update_log.csv để biết lỗi.", type = "error", duration = 8)
      }
    })
  }, ignoreInit = TRUE)

  source_choices <- reactive(sort(unique(listings()$source)))
  transaction_choices <- reactive(sort(unique(listings()$transaction_type)))
  district_choices <- reactive(sort(unique(listings()$district_name)))
  category_choices <- reactive(sort(unique(listings()$category_name)))

  filtered <- reactive({
    df <- listings()
    if (is_selected_filter(input$sources)) {
      df <- df %>% filter(source %in% input$sources)
    }
    if (is_selected_filter(input$transactions)) {
      df <- df %>% filter(transaction_type %in% input$transactions)
    }
    if (is_selected_filter(input$districts)) {
      df <- df %>% filter(district_name %in% input$districts)
    }
    if (is_selected_filter(input$categories)) {
      df <- df %>% filter(category_name %in% input$categories)
    }
    price_range <- safe_range(input$price_range, c(0, 100))
    area_range <- safe_range(input$area_range, c(0, 1000))
    df %>%
      filter(
        price_b >= price_range[1], price_b <= price_range[2],
        area >= area_range[1], area <= area_range[2]
      )
  })

  map_filtered <- reactive({
    df <- listings()
    if (is_selected_filter(input$map_sources)) {
      df <- df %>% filter(source %in% input$map_sources)
    }
    if (is_selected_filter(input$map_transactions)) {
      df <- df %>% filter(transaction_type %in% input$map_transactions)
    }
    if (is_selected_filter(input$map_districts)) {
      df <- df %>% filter(district_name %in% input$map_districts)
    }
    if (is_selected_filter(input$map_categories)) {
      df <- df %>% filter(category_name %in% input$map_categories)
    }
    price_range <- safe_range(input$map_price_range, c(0, 100))
    area_range <- safe_range(input$map_area_range, c(0, 1000))
    df %>%
      filter(
        price_b >= price_range[1], price_b <= price_range[2],
        area >= area_range[1], area <= area_range[2],
        !is.na(lat), !is.na(lon)
      )
  })

  data_filtered <- reactive({
    df <- listings()
    if (is_selected_filter(input$data_sources)) {
      df <- df %>% filter(source %in% input$data_sources)
    }
    if (is_selected_filter(input$data_transactions)) {
      df <- df %>% filter(transaction_type %in% input$data_transactions)
    }
    if (is_selected_filter(input$data_districts)) {
      df <- df %>% filter(district_name %in% input$data_districts)
    }
    if (is_selected_filter(input$data_categories)) {
      df <- df %>% filter(category_name %in% input$data_categories)
    }
    price_range <- safe_range(input$data_price_range, c(0, 100))
    df %>%
      filter(price_b >= price_range[1], price_b <= price_range[2])
  })

  chart_transaction <- function(input_id) {
    value <- input[[input_id]]
    if (is.null(value) || length(value) == 0 || !(value %in% c("Bán", "Cho thuê"))) "Bán" else value[[1]]
  }

  overview_chart_data <- function(input_id) {
    tx <- chart_transaction(input_id)
    listings() %>% filter(transaction_type == tx)
  }

  analysis_chart_data <- function(input_id) {
    tx <- chart_transaction(input_id)
    df <- listings() %>% filter(transaction_type == tx)

    if (is_selected_filter(input$sources)) {
      df <- df %>% filter(source %in% input$sources)
    }
    if (is_selected_filter(input$districts)) {
      df <- df %>% filter(district_name %in% input$districts)
    }
    if (is_selected_filter(input$categories)) {
      df <- df %>% filter(category_name %in% input$categories)
    }

    price_range <- safe_range(input$price_range, c(0, 100))
    area_range <- safe_range(input$area_range, c(0, 1000))
    df %>%
      filter(
        price_b >= price_range[1], price_b <= price_range[2],
        area >= area_range[1], area <= area_range[2]
      )
  }

  output$kpi_cards <- renderUI({
    df <- listings()
    m <- metrics()
    div(
      class = "kpi-grid",
      kpi_card("Tin đăng đã thu thập", format(nrow(df), big.mark = ","), "sau làm sạch", "database", "default"),
      kpi_card("Giá trung vị", format_vnd(median(df$price, na.rm = TRUE)), "toàn TP.HCM", "coins", "warning"),
      kpi_card("Quận/huyện có dữ liệu", paste0(n_distinct(df$district_name), "/22+"), "độ phủ địa lý", "location-dot", "success"),
      kpi_card("Mô hình tốt nhất", best_model_name_only(m), "chọn theo MAPE/RMSE", "bullseye", "success", delta = best_model_mape_only(m), value_class = "text-mode")
    )
  })

  output$source_filter <- renderUI({
    filter_select("sources", source_choices(), "Tất cả nguồn")
  })

  output$district_filter <- renderUI({
    filter_select("districts", district_choices(), "Tất cả quận/huyện")
  })

  output$transaction_filter <- renderUI({
    filter_select("transactions", transaction_choices(), "Tất cả giao dịch")
  })

  output$category_filter <- renderUI({
    filter_select("categories", category_choices(), "Tất cả loại BĐS")
  })

  output$map_source_filter <- renderUI({
    filter_select("map_sources", source_choices(), "Tất cả nguồn")
  })

  output$map_district_filter <- renderUI({
    filter_select("map_districts", district_choices(), "Tất cả quận/huyện")
  })

  output$map_transaction_filter <- renderUI({
    filter_select("map_transactions", transaction_choices(), "Tất cả giao dịch")
  })

  output$map_category_filter <- renderUI({
    filter_select("map_categories", category_choices(), "Tất cả loại BĐS")
  })

  output$data_source_filter <- renderUI({
    filter_select("data_sources", source_choices(), "Tất cả nguồn")
  })

  output$data_district_filter <- renderUI({
    filter_select("data_districts", district_choices(), "Tất cả quận/huyện")
  })

  output$data_transaction_filter <- renderUI({
    filter_select("data_transactions", transaction_choices(), "Tất cả giao dịch")
  })

  output$data_category_filter <- renderUI({
    filter_select("data_categories", category_choices(), "Tất cả loại BĐS")
  })

  output$predict_district <- renderUI({
    selectInput("pred_district", NULL, choices = district_choices(), selectize = FALSE)
  })

  output$predict_category <- renderUI({
    is_rent_pred <- identical(input$predict_transaction, "Cho thuê")
    choices <- listings() %>%
      filter(is_rent == !!is_rent_pred) %>%
      pull(category_name) %>%
      unique() %>%
      sort()
    if (length(choices) == 0) choices <- category_choices()
    selectInput("pred_category", NULL, choices = choices, selectize = FALSE)
  })

  output$filter_summary <- renderUI({
    df <- filtered()
    div(
      class = "filter-summary",
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Số tin"),
          div(class = "filter-chip-value", format(nrow(df), big.mark = ","))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Giá trung vị"),
          div(class = "filter-chip-value", format_vnd(median(df$price, na.rm = TRUE))))
    )
  })

  output$map_filter_summary <- renderUI({
    df <- map_filtered()
    div(
      class = "filter-summary full",
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Marker hiển thị"),
          div(class = "filter-chip-value", format(nrow(df), big.mark = ","))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Nguồn"),
          div(class = "filter-chip-value", active_or_all(input$map_sources))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Khu vực"),
          div(class = "filter-chip-value", active_or_all(input$map_districts)))
    )
  })

  output$data_filter_summary <- renderUI({
    df <- data_filtered()
    div(
      class = "filter-summary full",
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Dòng dữ liệu"),
          div(class = "filter-chip-value", format(nrow(df), big.mark = ","))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Nguồn"),
          div(class = "filter-chip-value", active_or_all(input$data_sources))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Giá trung vị"),
          div(class = "filter-chip-value", format_vnd(median(df$price, na.rm = TRUE))))
    )
  })

  observeEvent(input$reset_analysis_filters, {
    updateSelectInput(session, "sources", selected = "__all__")
    updateSelectInput(session, "transactions", selected = "__all__")
    updateSelectInput(session, "districts", selected = "__all__")
    updateSelectInput(session, "categories", selected = "__all__")
    updateSliderInput(session, "price_range", value = c(0, 100))
    updateSliderInput(session, "area_range", value = c(0, 1000))
  }, ignoreInit = TRUE)

  observeEvent(input$reset_map_filters, {
    updateSelectInput(session, "map_sources", selected = "__all__")
    updateSelectInput(session, "map_transactions", selected = "__all__")
    updateSelectInput(session, "map_districts", selected = "__all__")
    updateSelectInput(session, "map_categories", selected = "__all__")
    updateSliderInput(session, "map_price_range", value = c(0, 100))
    updateSliderInput(session, "map_area_range", value = c(0, 1000))
  }, ignoreInit = TRUE)

  observeEvent(input$reset_data_filters, {
    updateSelectInput(session, "data_sources", selected = "__all__")
    updateSelectInput(session, "data_transactions", selected = "__all__")
    updateSelectInput(session, "data_districts", selected = "__all__")
    updateSelectInput(session, "data_categories", selected = "__all__")
    updateSliderInput(session, "data_price_range", value = c(0, 100))
  }, ignoreInit = TRUE)

  output$district_plot <- renderPlotly({
    tx <- chart_transaction("district_plot_tx")
    df <- overview_chart_data("district_plot_tx")
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", tx, "phù hợp.")))
    p <- df %>%
      count(district_name, sort = TRUE) %>%
      slice_head(n = 12) %>%
      mutate(
        tooltip = paste0("Giao dịch: ", tx, "<br>Quận/huyện: ", district_name, "<br>Số tin: ", format(n, big.mark = "."))
      ) %>%
      ggplot(aes(x = reorder(district_name, n), y = n, text = tooltip)) +
      geom_col(fill = "#0072bc", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "Số tin") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = "text")
  })

  output$category_plot <- renderPlotly({
    tx <- chart_transaction("category_plot_tx")
    df <- overview_chart_data("category_plot_tx")
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", tx, "phù hợp.")))
    p <- df %>%
      count(category_name, sort = TRUE) %>%
      mutate(
        tooltip = paste0("Giao dịch: ", tx, "<br>Loại BĐS: ", category_name, "<br>Số tin: ", format(n, big.mark = "."))
      ) %>%
      ggplot(aes(x = reorder(category_name, n), y = n, fill = category_name, text = tooltip)) +
      geom_col(width = 0.72) +
      scale_fill_manual(values = chart_palette) +
      coord_flip() +
      guides(fill = "none") +
      labs(x = NULL, y = "Số tin") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = "text")
  })

  output$metrics_table <- renderTable({
    m <- metrics()
    if (nrow(m) == 0) return(data.frame(Ghi_chu = "Chưa có models/model_metrics.csv"))
    m %>%
      mutate(
        rmse_vnd = format_vnd_full(rmse_vnd),
        mae_vnd = format_vnd_full(mae_vnd),
        mape = paste0(round(mape * 100, 1), "%"),
        r2 = format_metric(r2)
      ) %>%
      rename(
        `Nhóm dữ liệu` = segment,
        `Mô hình` = model,
        RMSE = rmse_vnd,
        MAE = mae_vnd,
        MAPE = mape,
        `R²` = r2
      )
  })

  output$listing_map <- renderLeaflet({
    df <- map_filtered()
    validate(need(nrow(df) > 0, "Không có điểm dữ liệu phù hợp bộ lọc."))

    source_links <- listing_url(df$ad_url)
    source_link_html <- ifelse(
      !is.na(source_links) & source_links != "",
      paste0(
        "<a href='", htmltools::htmlEscape(source_links), "' target='_blank' rel='noopener noreferrer' ",
        "style='display:inline-flex;align-items:center;justify-content:center;margin-top:10px;",
        "padding:7px 10px;border-radius:6px;background:#0072bc;color:#ffffff;",
        "font-weight:700;text-decoration:none'>Xem tin gốc</a>"
      ),
      "<div style='margin-top:10px;color:#94a3b8;font-size:12px'>Tin này chưa có link gốc</div>"
    )

    popup <- paste0(
      "<div style='font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Arial,sans-serif;font-size:12px;min-width:220px'>",
      "<div style='font-weight:700;color:#0072bc;margin-bottom:4px'>", htmltools::htmlEscape(df$title), "</div>",
      "<div style='color:#64748b'>", htmltools::htmlEscape(df$district_name), " · ", htmltools::htmlEscape(df$ward), "</div>",
      "<div style='margin-top:7px;display:grid;grid-template-columns:auto 1fr;gap:3px 10px'>",
      "<span style='color:#64748b'>Giá</span><b>", format_vnd_full(df$price), "</b>",
      "<span style='color:#64748b'>Diện tích</span><b>", round(df$area, 1), " m²</b>",
      "<span style='color:#64748b'>Giá/m²</span><b>", format_vnd_full(df$price_per_m2), "/m²</b>",
      "<span style='color:#64748b'>Loại</span><b>", htmltools::htmlEscape(df$category_name), "</b>",
      "</div>",
      source_link_html,
      "</div>"
    )

    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 106.70, lat = 10.78, zoom = 11) %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = 5, stroke = TRUE, weight = 1, color = "#ffffff",
        fillColor = ~price_color(price), fillOpacity = 0.82,
        popup = popup,
        clusterOptions = markerClusterOptions()
      )
  })

  output$area_price_plot <- renderPlotly({
    tx <- chart_transaction("area_price_tx")
    price_info <- price_display_info(tx)
    df <- analysis_chart_data("area_price_tx") %>%
      mutate(display_price = .data[[price_info$value_col]]) %>%
      filter(!is.na(area), area > 0, !is.na(display_price), display_price > 0)
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", tx, "phù hợp bộ lọc.")))
    price_cutoff <- quantile(df$display_price, 0.98, na.rm = TRUE)
    p <- df %>%
      filter(display_price <= price_cutoff) %>%
      plot_sample(max_n = 1600) %>%
      mutate(
        tooltip = paste0(
          "Giao dịch: ", tx,
          "<br>Loại BĐS: ", category_name,
          "<br>Quận/huyện: ", district_name,
          "<br>Diện tích: ", format_number_vi(area, 1), " m²",
          "<br>Giá: ", format_number_vi(display_price, price_info$digits), " ", price_info$unit
        )
      ) %>%
      ggplot(aes(x = area, y = display_price, color = category_name, text = tooltip)) +
      geom_point(alpha = 0.55, size = 1.7) +
      scale_color_manual(values = chart_palette) +
      labs(x = "Diện tích (m²)", y = price_info$axis, color = "Loại") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = "text") %>% toWebGL()
  })

  output$price_m2_plot <- renderPlotly({
    tx <- chart_transaction("price_m2_tx")
    m2_info <- price_m2_display_info(tx)
    df <- analysis_chart_data("price_m2_tx")
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", tx, "phù hợp bộ lọc.")))
    p <- df %>%
      filter(!is.na(price_per_m2), price_per_m2 > 0) %>%
      group_by(district_name) %>%
      summarise(median_price_m2 = median(price_per_m2, na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n >= 3) %>%
      slice_max(median_price_m2, n = 10) %>%
      mutate(display_price_m2 = median_price_m2 / m2_info$scale) %>%
      mutate(
        tooltip = paste0(
          "Giao dịch: ", tx,
          "<br>Quận/huyện: ", district_name,
          "<br>Giá trung vị/m²: ", format_number_vi(display_price_m2, m2_info$digits), " ", m2_info$unit,
          "<br>Số tin: ", format(n, big.mark = ".")
        )
      ) %>%
      ggplot(aes(x = reorder(district_name, display_price_m2), y = display_price_m2, text = tooltip)) +
      geom_col(fill = "#059669", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = m2_info$axis) +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = "text")
  })

  output$price_category_plot <- renderPlotly({
    selected_transaction <- chart_transaction("price_category_tx")
    price_info <- price_display_info(selected_transaction)
    df <- analysis_chart_data("price_category_tx") %>%
      mutate(display_price = .data[[price_info$value_col]]) %>%
      filter(!is.na(display_price), display_price > 0)
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", selected_transaction, "phù hợp bộ lọc.")))
    price_cutoff <- quantile(df$display_price, 0.98, na.rm = TRUE)

    summary_df <- df %>%
      filter(display_price <= price_cutoff) %>%
      filter(!is.na(category_name), category_name != "") %>%
      group_by(category_name) %>%
      summarise(
        q1_price = quantile(display_price, 0.25, na.rm = TRUE),
        median_price = median(display_price, na.rm = TRUE),
        q3_price = quantile(display_price, 0.75, na.rm = TRUE),
        listing_count = n(),
        .groups = "drop"
      ) %>%
      filter(listing_count >= 3) %>%
      arrange(median_price) %>%
      mutate(
        category_label = factor(category_name, levels = category_name),
        tooltip = paste0(
          "Giao dịch: ", selected_transaction,
          "<br>Loại BĐS: ", category_name,
          "<br>Giá trung vị: ", format_number_vi(median_price, price_info$digits), " ", price_info$unit,
          "<br>Vùng phổ biến: ", format_number_vi(q1_price, price_info$digits), " - ", format_number_vi(q3_price, price_info$digits), " ", price_info$unit,
          "<br>Số tin: ", format(listing_count, big.mark = ".")
        )
      )
    validate(need(nrow(summary_df) > 0, "Không có dữ liệu phù hợp để vẽ biểu đồ."))

    plot_ly(
      summary_df,
      x = ~median_price,
      y = ~category_label,
      type = "scatter",
      mode = "markers",
      text = ~tooltip,
      hovertemplate = "%{text}<extra></extra>",
      marker = list(size = 10, color = "#0072bc", line = list(color = "#ffffff", width = 1.5)),
      error_x = list(
        type = "data",
        symmetric = FALSE,
        array = ~q3_price - median_price,
        arrayminus = ~median_price - q1_price,
        color = "#0072bc",
        thickness = 2,
        width = 4
      )
    ) %>%
      layout(
        showlegend = FALSE,
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#d7e6f5", font = list(color = "#1f2937")),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        annotations = list(list(
          x = 0,
          y = 1.12,
          xref = "paper",
          yref = "paper",
          text = paste0("Đang hiển thị giao dịch: ", selected_transaction),
          showarrow = FALSE,
          xanchor = "left",
          font = list(size = 12, color = "#64748b")
        )),
        xaxis = list(title = price_info$axis, gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb"),
        yaxis = list(title = "", automargin = TRUE, categoryorder = "array", categoryarray = levels(summary_df$category_label))
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$log_price_plot <- renderPlotly({
    tx <- chart_transaction("log_price_tx")
    plot_df <- analysis_chart_data("log_price_tx") %>%
      filter(!is.na(price), price > 0) %>%
      mutate(price_index = log1p(price))
    validate(need(nrow(plot_df) > 0, paste("Không có dữ liệu", tx, "phù hợp bộ lọc.")))

    plot_ly(
      plot_df,
      x = ~price_index,
      type = "histogram",
      nbinsx = 28,
      marker = list(color = "#d97706", line = list(color = "#ffffff", width = 0.5)),
      hovertemplate = paste0("Giao dịch: ", tx, "<br>Mức giá chuẩn hóa: %{x:.2f}<br>Số tin: %{y}<extra></extra>")
    ) %>%
      layout(
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#d7e6f5", font = list(color = "#1f2937")),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        bargap = 0.04,
        xaxis = list(title = "Mức giá chuẩn hóa", gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb"),
        yaxis = list(title = "Số tin", gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  prediction <- eventReactive(input$predict_btn, {
    req(input$pred_district, input$pred_category, input$predict_transaction, input$predict_area, input$predict_rooms)
    area_val <- as.numeric(input$predict_area)
    if (is.na(area_val) || area_val <= 0) return(NA_real_)
    df <- listings()
    if (nrow(df) == 0) return(NA_real_)

    input_row <- build_prediction_row(
      df = df,
      district = input$pred_district,
      category = input$pred_category,
      ward = input$predict_ward,
      area = input$predict_area,
      rooms = input$predict_rooms,
      transaction_type = input$predict_transaction
    )

    predict_price(input_row, input_row$is_rent[[1]])
  }, ignoreInit = TRUE)

  output$prediction_text <- renderText({
    pred <- prediction()
    if (is.na(pred)) "Chưa dự đoán được" else format_vnd_full(pred)
  })

  output$prediction_note <- renderText({
    pred <- prediction()
    if (is.na(pred)) {
      "Hãy chọn quận/huyện và loại bất động sản có trong dữ liệu train."
    } else {
      paste("Giao dịch:", input$predict_transaction, "· Loại:", input$pred_category, "· Khu vực:", input$pred_district, "· Diện tích:", input$predict_area, "m²")
    }
  })

  output$prediction_model_note <- renderText({
    is_rent_pred <- identical(input$predict_transaction, "Cho thuê")
    paste0("Mô hình: ", prediction_model_label(is_rent_pred), " · giá trị mang tính tham khảo")
  })

  output$importance_plot <- renderPlotly({
    is_rent_pred <- identical(input$predict_transaction, "Cho thuê")
    path <- if (is_rent_pred) {
      if (file.exists("models/rf_importance_rent.csv")) "models/rf_importance_rent.csv" else "models/rf_importance_sale.csv"
    } else {
      if (file.exists("models/rf_importance_sale.csv")) "models/rf_importance_sale.csv" else "models/rf_importance_rent.csv"
    }
    validate(need(file.exists(path), "Chưa có feature importance."))
    p <- read_csv(path, show_col_types = FALSE) %>%
      slice_head(n = 10) %>%
      mutate(
        feature_label = feature_label_vi(feature),
        tooltip = paste0("Yếu tố: ", feature_label, "<br>Mức ảnh hưởng: ", format_number_vi(IncNodePurity, 1))
      ) %>%
      ggplot(aes(x = reorder(feature_label, IncNodePurity), y = IncNodePurity, text = tooltip)) +
      geom_col(fill = "#0072bc", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "Mức ảnh hưởng") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = "text")
  })

  output$cluster_plot <- renderPlotly({
    validate(need(file.exists(CLUSTER_PATH), "Chưa có kmeans_area_price.csv. Hãy chạy train_models.R."))
    tx <- chart_transaction("cluster_tx")
    m2_info <- price_m2_display_info(tx)
    cluster_df <- read_csv(CLUSTER_PATH, show_col_types = FALSE)
    if (!"transaction_type" %in% names(cluster_df)) {
      cluster_df$transaction_type <- "Bán"
    }
    cluster_df <- cluster_df %>% filter(transaction_type == tx)
    validate(need(nrow(cluster_df) > 0, paste("Chưa có dữ liệu phân cụm cho giao dịch", tx)))

    p <- cluster_df %>%
      mutate(
        cluster = as.factor(cluster),
        display_price_m2 = median_price_per_m2 / m2_info$scale,
        tooltip = paste0(
          "Giao dịch: ", transaction_type,
          "<br>Quận/huyện: ", district_name,
          "<br>Loại BĐS: ", category_name,
          "<br>Cụm: ", cluster,
          "<br>Diện tích trung vị: ", format_number_vi(median_area, 1), " m²",
          "<br>Giá/m² trung vị: ", format_number_vi(display_price_m2, m2_info$digits), " ", m2_info$unit,
          "<br>Số tin: ", format(listing_count, big.mark = ".")
        )
      ) %>%
      ggplot(aes(x = median_area, y = display_price_m2, color = cluster, size = listing_count, text = tooltip)) +
      geom_point(alpha = 0.78) +
      scale_color_manual(values = chart_palette) +
      labs(x = "Diện tích trung vị (m²)", y = paste0("Giá/m² trung vị (", m2_info$unit, ")"), color = "Cụm", size = "Số tin") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = "text")
  })

  output$data_table <- renderDT({
    data_filtered() %>%
      transmute(
        `Nguồn` = source,
        `Giao dịch` = transaction_type,
        `Tiêu đề` = title,
        `Quận/huyện` = district_name,
        `Phường/xã` = ward,
        `Loại BĐS` = category_name,
        `Giá` = format_vnd_full(price),
        `Diện tích` = paste0(round(area, 1), " m²"),
        `Giá/m²` = format_vnd_full(price_per_m2),
        `Link` = ifelse(
          !is.na(listing_url(ad_url)),
          paste0('<a href="', htmltools::htmlEscape(listing_url(ad_url)), '" target="_blank" rel="noopener">Xem tin</a>'),
          ""
        )
      ) %>%
      datatable(
        rownames = FALSE,
        filter = "none",
        escape = 1:9,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          language = list(search = "Tìm kiếm:", lengthMenu = "Hiển thị _MENU_ dòng")
        )
      )
  })
}

shinyApp(ui, server)
