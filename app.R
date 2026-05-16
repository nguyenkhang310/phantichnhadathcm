# ============================================================
# SHINY APP - BĐS TP.HCM
# Giao diện dashboard tùy biến, backend giữ pipeline R.
# Chạy: Rscript -e 'shiny::runApp(".", host="127.0.0.1", port=3838)'
# ============================================================

if (dir.exists("R_libs")) .libPaths(c(normalizePath("R_libs"), .libPaths()))

required_packages <- c(
  "shiny", "dplyr", "readr", "lubridate",
  "ggplot2", "plotly", "DT", "randomForest", "leaflet"
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

DATA_PATH <- "data/hcmc_bds_featured.csv"
RAW_PATH <- "data/hcmc_bds_raw.csv"
METRICS_PATH <- "models/model_metrics.csv"
SALE_MODEL_PATH <- "models/price_models_sale.rds"
RENT_MODEL_PATH <- "models/price_models_rent.rds"
CLUSTER_PATH <- "models/kmeans_area_price.csv"

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

format_number_vi <- function(x, digits = 1) {
  format(round(x, digits), big.mark = ".", decimal.mark = ",", nsmall = digits, trim = TRUE)
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

predict_price <- function(input_row, is_rent) {
  model_path <- if (is_rent) RENT_MODEL_PATH else SALE_MODEL_PATH
  if (!file.exists(model_path)) return(NA_real_)

  bundle <- readRDS(model_path)
  model <- bundle$random_forest
  if (is.null(model)) return(NA_real_)

  for (col in names(model$forest$xlevels)) {
    if (col %in% names(input_row)) {
      levels_for_col <- model$forest$xlevels[[col]]
      if (is.character(levels_for_col) && length(levels_for_col) > 0) {
        input_row[[col]] <- factor(as.character(input_row[[col]]), levels = levels_for_col)
      }
    }
  }

  pred <- tryCatch(predict(model, newdata = input_row), error = function(e) NA_real_)
  expm1(as.numeric(pred))
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

kpi_card <- function(label, value, hint = NULL, icon = "chart-line", tone = "default", delta = NULL) {
  div(
    class = paste("kpi-card", paste0("tone-", tone)),
    div(
      class = "kpi-top",
      div(class = "kpi-label", label),
      div(class = "kpi-icon", icon(icon))
    ),
    div(class = "kpi-value", value),
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

chart_palette <- c("#0072bc", "#059669", "#d97706", "#ed1c24", "#64748b")

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
  --radius: 8px;
  --bg: #f6f8fb;
  --fg: #1f2937;
  --muted: #64748b;
  --card: #ffffff;
  --border: #e5edf5;
  --primary: #0072bc;
  --primary-dark: #005a94;
  --sidebar: #ffffff;
  --sidebar-accent: #f1f5f9;
  --sidebar-hover: #f8fafc;
  --success: #059669;
  --warning: #d97706;
  --danger: #ed1c24;
  --chart1: #0072bc;
  --chart2: #059669;
  --chart3: #d97706;
  --chart4: #ed1c24;
  --chart5: #64748b;
}
body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Inter, Arial, sans-serif;
}
html, body, .container-fluid {
  min-height: 100%;
  margin: 0;
  padding: 0;
  background: var(--bg);
  color: var(--fg);
}
body {
  font-family: Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Arial, sans-serif;
  font-feature-settings: 'cv11', 'ss01';
}
.app-shell {
  display: flex;
  min-height: 100vh;
  width: 100%;
  background: var(--bg);
}
.app-sidebar {
  position: sticky;
  top: 0;
  width: 248px;
  min-width: 248px;
  height: 100vh;
  display: flex;
  flex-direction: column;
  background: var(--sidebar);
  color: var(--fg);
  border-right: 1px solid var(--border);
  box-shadow: 1px 0 10px rgba(0,0,0,0.02);
}
.app-brand {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 24px 16px;
  border-bottom: 1px solid var(--border);
  text-align: center;
  gap: 12px;
}
.brand-logo {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 112px;
  height: auto;
  background: transparent;
  transition: transform 0.2s ease;
}
.brand-logo:hover {
  transform: scale(1.04);
}
.brand-logo img {
  width: 100%;
  height: auto;
  object-fit: contain;
  filter: drop-shadow(0 4px 6px rgba(0, 114, 188, 0.08));
}
.brand-text {
  display: flex;
  flex-direction: column;
  gap: 3px;
}
.brand-title {
  color: var(--primary-dark);
  font-size: 13px;
  font-weight: 800;
  text-transform: uppercase;
  letter-spacing: 0.05em;
}
.brand-subtitle {
  color: #64748b;
  font-size: 11px;
  font-weight: 600;
}
.nav-section-label {
  padding: 24px 20px 8px;
  font-size: 11px;
  font-weight: 700;
  letter-spacing: .08em;
  color: #94a3b8;
}
.app-nav-link {
  display: flex;
  align-items: center;
  gap: 12px;
  min-height: 42px;
  margin: 4px 12px;
  padding: 0 14px;
  color: #475569;
  text-decoration: none !important;
  border-radius: 8px;
  font-size: 14px;
  font-weight: 500;
  transition: all .15s ease;
}
.app-nav-link:hover,
.app-nav-link:focus {
  background: var(--sidebar-hover);
  color: var(--primary);
}
.app-nav-link.active {
  background: rgba(0, 114, 188, 0.08);
  color: var(--primary);
  font-weight: 600;
}
.app-nav-link i {
  width: 18px;
  text-align: center;
  font-size: 15px;
  color: #94a3b8;
  transition: color .15s ease;
}
.app-nav-link:hover i, .app-nav-link.active i {
  color: var(--primary);
}
.app-sidebar-footer {
  margin-top: auto;
  padding: 18px;
  color: #94a3b8;
  font-size: 11px;
  line-height: 1.55;
  border-top: 1px solid var(--border);
  text-align: center;
}
.app-main {
  min-width: 0;
  flex: 1;
  display: flex;
  flex-direction: column;
}
.app-topbar {
  position: sticky;
  top: 0;
  z-index: 20;
  height: 64px;
  display: flex;
  align-items: center;
  padding: 0 28px;
  background: rgba(255,255,255,.8);
  backdrop-filter: blur(16px);
  -webkit-backdrop-filter: blur(16px);
  border-bottom: 1px solid var(--border);
  box-shadow: 0 1px 2px rgba(0,0,0,0.01);
}
.topbar-title-wrap {
  min-width: 0;
  display: flex;
  flex-direction: column;
  justify-content: center;
}
.topbar-title {
  color: var(--fg);
  font-size: 16px;
  font-weight: 700;
  line-height: 1.2;
  letter-spacing: -0.01em;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
.topbar-subtitle {
  margin-top: 3px;
  color: #64748b;
  font-size: 12px;
  font-weight: 500;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
.status-badge {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  padding: 6px 12px;
  border-radius: 999px;
  background: #f0fdf4;
  border: 1px solid #bbf7d0;
  color: #166534;
  font-size: 12px;
  font-weight: 600;
  white-space: nowrap;
}
.status-dot {
  width: 6px;
  height: 6px;
  border-radius: 50%;
  background: #22c55e;
  box-shadow: 0 0 0 2px rgba(34,197,94,0.2);
  animation: pulse 2s infinite;
}
@keyframes pulse {
  0% { box-shadow: 0 0 0 0 rgba(34,197,94,0.4); }
  70% { box-shadow: 0 0 0 4px rgba(34,197,94,0); }
  100% { box-shadow: 0 0 0 0 rgba(34,197,94,0); }
}
.app-content {
  flex: 1;
  min-width: 0;
}
.app-content > .tabbable > .nav {
  display: none;
}
.app-content .tab-content {
  border: 0;
  padding: 0;
}
.app-content .tab-pane {
  padding: 0;
}
.page-wrap { padding: 18px 22px 28px; }
.page-title { font-size: 20px; font-weight: 750; color: var(--fg); margin: 0; }
.page-subtitle { color: var(--muted); font-size: 13px; margin: 4px 0 16px; }
.kpi-grid { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 12px; margin-bottom: 16px; }
.kpi-card {
  background: var(--card); border: 1px solid var(--border); border-radius: var(--radius);
  padding: 16px; box-shadow: 0 10px 26px rgba(15, 56, 98, .08), 0 1px 2px rgba(15, 23, 42, .04);
  transition: transform .18s ease, box-shadow .18s ease, border-color .18s ease;
}
.kpi-card:hover {
  transform: translateY(-2px);
  box-shadow: 0 18px 34px rgba(15, 56, 98, .13), 0 2px 6px rgba(15, 23, 42, .05);
  border-color: rgba(0,114,188,.22);
}
.kpi-top { display: flex; justify-content: space-between; gap: 10px; }
.kpi-label { font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: .05em; color: var(--muted); }
.kpi-icon {
  width: 34px; height: 34px; display: flex; align-items: center; justify-content: center;
  border-radius: 7px; background: rgba(0,114,188,.10); color: var(--primary);
}
.tone-success .kpi-icon { background: rgba(5,150,105,.12); color: var(--success); }
.tone-warning .kpi-icon { background: rgba(217,119,6,.12); color: var(--warning); }
.tone-danger .kpi-icon { background: rgba(237,28,36,.10); color: var(--danger); }
.kpi-value { margin-top: 8px; font-size: 28px; line-height: 1.1; font-weight: 760; color: var(--fg); font-variant-numeric: tabular-nums; }
.kpi-foot { margin-top: 8px; display: flex; align-items: center; gap: 8px; min-height: 18px; font-size: 12px; color: var(--muted); }
.kpi-delta { color: var(--success); background: rgba(5,150,105,.10); padding: 2px 6px; border-radius: 4px; font-weight: 700; }
.app-panel {
  background: var(--card); border: 1px solid var(--border); border-radius: var(--radius);
  box-shadow: 0 12px 30px rgba(15, 56, 98, .08), 0 1px 2px rgba(15, 23, 42, .04); margin-bottom: 16px; overflow: hidden;
  transition: box-shadow .18s ease, border-color .18s ease;
}
.app-panel:hover {
  box-shadow: 0 14px 32px rgba(15, 56, 98, .10), 0 2px 6px rgba(15, 23, 42, .04);
  border-color: rgba(0,114,188,.18);
}
.app-panel.nested-panel {
  box-shadow: none;
  border: 1px solid var(--border);
  margin-bottom: 0;
}
.app-panel.nested-panel:hover {
  transform: none;
  box-shadow: none;
}
.app-panel.filter-card {
  position: relative;
  z-index: 80;
  overflow: visible;
}
.app-panel.filter-card:hover {
  transform: none;
}
.app-panel.filter-card .panel-body {
  overflow: visible;
}
.app-panel.filter-card .panel-head {
  border-radius: var(--radius) var(--radius) 0 0;
}
.panel-head { padding: 13px 16px; border-bottom: 1px solid var(--border); display: flex; justify-content: space-between; gap: 12px; }
.panel-title { font-size: 14px; font-weight: 740; color: var(--fg); }
.panel-subtitle { font-size: 12px; color: var(--muted); margin-top: 2px; }
.panel-body { padding: 16px; }
.filter-panel {
  display: flex;
  flex-direction: column;
  gap: 14px;
}
.filter-toolbar {
  display: grid;
  grid-template-columns: 1fr 1.2fr 1.2fr .95fr .95fr auto;
  gap: 12px;
  align-items: end;
}
.filter-grid {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 12px;
}
.filter-field {
  min-width: 0;
}
.filter-field .form-group {
  margin-bottom: 0;
}
.filter-field > .form-group > label,
.filter-field label.control-label,
.predict-form label.control-label {
  display: none;
}
.filter-field-label {
  display: flex;
  align-items: center;
  gap: 7px;
  min-height: 18px;
  margin-bottom: 7px;
  color: #475569;
  font-size: 11px;
  font-weight: 760;
  letter-spacing: .04em;
  text-transform: uppercase;
}
.filter-field-label i {
  width: 14px;
  color: var(--primary);
  text-align: center;
}
.filter-summary {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 8px;
  border: 1px solid var(--border);
  background: #f8fafc;
  border-radius: 8px;
  padding: 10px;
}
.filter-summary.full {
  grid-template-columns: repeat(3, minmax(0, 1fr));
}
.filter-chip {
  min-width: 0;
  padding: 8px 9px;
  border: 1px solid #dbe7f2;
  border-radius: 7px;
  background: #ffffff;
}
.filter-chip-label {
  color: var(--muted);
  font-size: 10px;
  font-weight: 700;
  text-transform: uppercase;
}
.filter-chip-value {
  margin-top: 2px;
  color: var(--fg);
  font-size: 13px;
  font-weight: 760;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
.filter-actions {
  display: flex;
  justify-content: flex-end;
  align-items: flex-end;
}
.filter-toolbar > .filter-actions {
  grid-column: -2 / -1;
}
.btn-filter-reset {
  min-height: 38px;
  border: 1px solid #dbe7f2;
  border-radius: 7px;
  background: #ffffff;
  color: #475569;
  font-size: 12px;
  font-weight: 720;
}
.btn-filter-reset:hover,
.btn-filter-reset:focus {
  border-color: rgba(0,114,188,.35);
  color: var(--primary);
  background: #f8fbff;
}
.form-control,
.selectize-input {
  min-height: 40px;
  border: 1px solid #d7e4ef;
  border-radius: 7px;
  box-shadow: none;
  color: var(--fg);
  font-size: 13px;
  line-height: 20px;
}
.filter-field select.form-control,
.predict-form select.form-control {
  appearance: none;
  -webkit-appearance: none;
  cursor: pointer;
  padding: 8px 34px 8px 11px;
  background-color: #ffffff;
  background-image:
    linear-gradient(45deg, transparent 50%, #64748b 50%),
    linear-gradient(135deg, #64748b 50%, transparent 50%);
  background-position:
    calc(100% - 17px) 17px,
    calc(100% - 12px) 17px;
  background-size: 5px 5px, 5px 5px;
  background-repeat: no-repeat;
  transition: border-color .15s ease, box-shadow .15s ease, background-color .15s ease;
}
.filter-field select.form-control:hover,
.predict-form select.form-control:hover {
  border-color: #b8cbe0;
  background-color: #fbfdff;
}
.filter-field select.form-control:focus,
.predict-form select.form-control:focus,
.filter-field .form-control:focus,
.predict-form .form-control:focus {
  border-color: rgba(0,114,188,.55);
  box-shadow: 0 0 0 3px rgba(0,114,188,.10);
  outline: none;
}
.selectize-input {
  display: flex !important;
  align-items: center;
  gap: 4px;
  position: relative;
  padding: 7px 34px 7px 10px;
  transition: border-color .15s ease, box-shadow .15s ease, background .15s ease;
}
.selectize-control {
  position: relative;
}
.selectize-control.multi .selectize-input:after,
.selectize-control.single .selectize-input:after {
  content: '';
  position: absolute;
  right: 12px;
  top: 50%;
  width: 0;
  height: 0;
  margin-top: -2px;
  border-left: 5px solid transparent;
  border-right: 5px solid transparent;
  border-top: 6px solid #64748b;
  pointer-events: none;
  transition: transform .16s ease, border-top-color .16s ease;
}
.selectize-control.dropdown-active .selectize-input:after {
  transform: rotate(180deg);
  border-top-color: var(--primary);
}
.selectize-input.focus {
  border-color: rgba(0,114,188,.55);
  box-shadow: 0 0 0 3px rgba(0,114,188,.10);
  background: #ffffff;
}
.selectize-input input {
  font-size: 13px !important;
  line-height: 20px !important;
  color: var(--fg) !important;
}
.selectize-input input::placeholder {
  color: #94a3b8;
  opacity: 1;
}
.selectize-control.multi .selectize-input > div {
  display: inline-flex;
  align-items: center;
  max-width: 160px;
  min-height: 24px;
  padding: 3px 7px;
  border: 1px solid rgba(0,114,188,.18);
  border-radius: 999px;
  background: rgba(0,114,188,.08);
  color: var(--primary-dark);
  font-size: 12px;
  font-weight: 650;
  overflow: hidden;
  text-overflow: ellipsis;
}
.selectize-dropdown {
  z-index: 10000 !important;
  border: 1px solid #d7e4ef;
  border-radius: 8px;
  box-shadow: 0 12px 30px rgba(15, 56, 98, .14);
  overflow: hidden;
  margin-top: 6px;
  color: var(--fg);
  font-size: 13px;
}
.selectize-dropdown-content {
  max-height: 260px;
}
.selectize-dropdown .option {
  padding: 8px 10px;
  line-height: 1.35;
  cursor: pointer;
}
.selectize-dropdown .active {
  background: rgba(0,114,188,.08);
  color: var(--primary-dark);
}
.selectize-dropdown .selected {
  background: #f8fafc;
  color: #64748b;
}
.irs--shiny {
  height: 44px;
}
.irs--shiny .irs-line {
  height: 6px;
  border: 0;
  background: #e7eef6;
}
.irs--shiny .irs-bar {
  height: 6px;
  background: var(--primary);
  border-color: var(--primary);
}
.irs--shiny .irs-handle {
  top: 17px;
  width: 16px;
  height: 16px;
  border: 3px solid #ffffff;
  background: var(--primary);
  box-shadow: 0 1px 5px rgba(15,23,42,.25);
}
.irs--shiny .irs-from,
.irs--shiny .irs-to,
.irs--shiny .irs-single {
  background: #0f72b8;
  border-color: #0f72b8;
  border-radius: 5px;
  font-size: 11px;
  font-weight: 720;
}
.btn-primary { background: var(--primary); border-color: var(--primary); border-radius: 7px; font-weight: 650; }
.predict-form {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 12px;
}
.predict-form .wide {
  grid-column: 1 / -1;
}
.predict-form .action-button {
  min-height: 40px;
}
.prediction-hero {
  border-radius: var(--radius); border: 1px solid rgba(0,114,188,.16);
  background: linear-gradient(135deg, rgba(0,114,188,.10), rgba(255,255,255,.95));
  padding: 22px;
}
.prediction-label { font-size: 12px; text-transform: uppercase; letter-spacing: .06em; color: var(--muted); font-weight: 700; }
.prediction-value { margin-top: 6px; color: var(--primary); font-size: 38px; line-height: 1.1; font-weight: 780; }
.prediction-note { margin-top: 8px; color: var(--muted); font-size: 13px; }
.map-shell { position: relative; overflow: hidden; border-radius: var(--radius); border: 1px solid var(--border); }
.map-legend {
  position: absolute; z-index: 500; bottom: 14px; left: 14px; background: rgba(255,255,255,.95);
  border: 1px solid var(--border); border-radius: 6px; padding: 9px 11px; font-size: 12px; box-shadow: 0 4px 14px rgba(15,23,42,.12);
}
.dot { display: inline-block; width: 10px; height: 10px; border-radius: 999px; margin-right: 4px; }
.dot-low { background: var(--success); } .dot-mid { background: var(--warning); } .dot-high { background: var(--danger); }
.dataTables_wrapper { font-size: 13px; }
.table > thead > tr > th { background: #f8fafc; color: #334155; border-bottom: 1px solid var(--border); }
@media (max-width: 1100px) { .kpi-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
@media (max-width: 1200px) {
  .filter-toolbar { grid-template-columns: repeat(2, minmax(0, 1fr)); }
  .filter-actions { justify-content: flex-start; }
}
@media (max-width: 900px) {
  .app-shell { flex-direction: column; }
  .app-sidebar {
    position: relative;
    width: 100%;
    min-width: 0;
    height: auto;
  }
  .app-brand {
    flex-direction: row;
    padding: 12px 16px;
    justify-content: flex-start;
    text-align: left;
    border-bottom: 1px solid var(--border);
  }
  .brand-logo { width: 52px; }
  .brand-text { align-items: flex-start; }
  .nav-section-label, .app-sidebar-footer { display: none; }
  .app-sidebar nav { display: flex; overflow-x: auto; -webkit-overflow-scrolling: touch; padding: 12px; gap: 8px; }
  .app-sidebar .app-nav-link { display: inline-flex; white-space: nowrap; flex-shrink: 0; }
  .app-nav-link {
    min-height: 40px;
    margin: 0;
    padding: 0 16px;
    border-radius: 20px;
    background: var(--sidebar-hover);
  }
  .app-nav-link.active { background: rgba(0,114,188,.08); color: var(--primary); }
  .app-topbar { position: relative; }
}
@media (max-width: 640px) {
  .kpi-grid { grid-template-columns: 1fr; }
  .filter-toolbar, .filter-grid, .filter-summary.full, .predict-form { grid-template-columns: 1fr; }
  .page-wrap { padding: 14px; }
  .prediction-value { font-size: 30px; }
  .topbar-subtitle { display: none; }
  .app-topbar { padding: 0 16px; height: 56px; }
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
          span(class = "status-badge", span(class = "status-dot"), "Dữ liệu đã cập nhật")
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
                column(8, app_panel("Số tin theo quận/huyện", "Top khu vực có nhiều tin đăng nhất", plotlyOutput("district_plot", height = 330))),
                column(4, app_panel("Cơ cấu loại bất động sản", "Tỉ trọng theo số lượng tin", plotlyOutput("category_plot", height = 330)))
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
                column(9, app_panel("Diện tích vs Giá", "Mỗi điểm là một tin đăng, màu theo loại BĐS", plotlyOutput("area_price_plot", height = 430)))
              ),
              fluidRow(
                column(4, app_panel("Top quận/huyện theo giá/m²", "Triệu đồng/m² (median)", plotlyOutput("price_m2_plot", height = 310))),
                column(4, app_panel("Khoảng giá theo loại BĐS", "Boxplot sau khi loại bớt outlier cực trị", plotlyOutput("price_category_plot", height = 310))),
                column(4, app_panel("Phân phối log(giá)", "Kiểm tra độ lệch phải của giá", plotlyOutput("log_price_plot", height = 310)))
              )
            )
          ),
          tabPanel(
            title = "predict", value = "predict",
            div(
              class = "page-wrap",
              h1(class = "page-title", "Dự đoán giá bất động sản"),
              div(class = "page-subtitle", "Form demo sử dụng Random Forest đã train từ dữ liệu hiện có."),
              fluidRow(
                column(4, app_panel("Thông tin bất động sản", "Nhập đặc trưng để mô hình dự đoán",
                  div(class = "predict-form",
                    filter_field("Quận/huyện", uiOutput("predict_district"), icon_name = "location-dot"),
                    filter_field("Loại BĐS", uiOutput("predict_category"), icon_name = "building"),
                    filter_field("Giao dịch", selectInput("predict_transaction", NULL, choices = c("Bán", "Cho thuê"), selected = "Bán", selectize = FALSE), icon_name = "tags", class = "wide"),
                    filter_field("Phường/xã", textInput("predict_ward", NULL, value = "Không rõ"), icon_name = "map-pin", class = "wide"),
                    filter_field("Diện tích", numericInput("predict_area", NULL, value = 75, min = 5, max = 5000), icon_name = "ruler-combined"),
                    filter_field("Số phòng", numericInput("predict_rooms", NULL, value = 2, min = 0, max = 20), icon_name = "bed"),
                    actionButton("predict_btn", "Tính lại dự đoán", icon = icon("calculator"), class = "btn-primary wide", width = "100%")
                  ), class = "filter-card")),
                column(8, app_panel("Kết quả dự đoán", "Mô hình: Random Forest · giá trị mang tính tham khảo",
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
            div(class = "page-subtitle", "K-Means theo giá/m², diện tích trung vị và số tin."),
            app_panel("K-Means clusters", "Bubble size thể hiện số tin, màu là cụm", plotlyOutput("cluster_plot", height = 500)))),
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

  listings <- reactive({
    data_tick()
    load_data()
  })

  metrics <- reactive(load_metrics())

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

  output$kpi_cards <- renderUI({
    df <- listings()
    m <- metrics()
    div(
      class = "kpi-grid",
      kpi_card("Tin đăng đã thu thập", format(nrow(df), big.mark = ","), "sau làm sạch", "database", "default"),
      kpi_card("Giá trung vị", format_vnd(median(df$price, na.rm = TRUE)), "toàn TP.HCM", "coins", "warning"),
      kpi_card("Quận/huyện có dữ liệu", paste0(n_distinct(df$district_name), "/22+"), "độ phủ địa lý", "location-dot", "success"),
      kpi_card("Mô hình tốt nhất", best_model_label(m), "nhóm sale đáng tin hơn", "bullseye", "success")
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
    selectInput("pred_category", NULL, choices = category_choices(), selectize = FALSE)
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
    req(nrow(listings()) > 0)
    p <- listings() %>%
      count(district_name, sort = TRUE) %>%
      slice_head(n = 12) %>%
      mutate(
        tooltip = paste0("Quận/huyện: ", district_name, "<br>Số tin: ", format(n, big.mark = "."))
      ) %>%
      ggplot(aes(x = reorder(district_name, n), y = n, text = tooltip)) +
      geom_col(fill = "#0072bc", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "Số tin") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = "text")
  })

  output$category_plot <- renderPlotly({
    req(nrow(listings()) > 0)
    p <- listings() %>%
      count(category_name, sort = TRUE) %>%
      mutate(
        tooltip = paste0("Loại BĐS: ", category_name, "<br>Số tin: ", format(n, big.mark = "."))
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

    popup <- paste0(
      "<div style='font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Arial,sans-serif;font-size:12px;min-width:220px'>",
      "<div style='font-weight:700;color:#0072bc;margin-bottom:4px'>", htmltools::htmlEscape(df$title), "</div>",
      "<div style='color:#64748b'>", htmltools::htmlEscape(df$district_name), " · ", htmltools::htmlEscape(df$ward), "</div>",
      "<div style='margin-top:7px;display:grid;grid-template-columns:auto 1fr;gap:3px 10px'>",
      "<span style='color:#64748b'>Giá</span><b>", format_vnd_full(df$price), "</b>",
      "<span style='color:#64748b'>Diện tích</span><b>", round(df$area, 1), " m²</b>",
      "<span style='color:#64748b'>Giá/m²</span><b>", format_vnd_full(df$price_per_m2), "/m²</b>",
      "<span style='color:#64748b'>Loại</span><b>", htmltools::htmlEscape(df$category_name), "</b>",
      "</div></div>"
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
    req(nrow(filtered()) > 0)
    p <- filtered() %>%
      filter(!is.na(area), area > 0, price_m <= quantile(price_m, 0.98, na.rm = TRUE)) %>%
      plot_sample(max_n = 1600) %>%
      mutate(
        tooltip = paste0(
          "Loại BĐS: ", category_name,
          "<br>Quận/huyện: ", district_name,
          "<br>Diện tích: ", format_number_vi(area, 1), " m²",
          "<br>Giá: ", format_number_vi(price_b, 2), " tỷ VND"
        )
      ) %>%
      ggplot(aes(x = area, y = price_b, color = category_name, text = tooltip)) +
      geom_point(alpha = 0.55, size = 1.7) +
      scale_color_manual(values = chart_palette) +
      labs(x = "Diện tích (m²)", y = "Giá (tỷ VND)", color = "Loại") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = "text") %>% toWebGL()
  })

  output$price_m2_plot <- renderPlotly({
    req(nrow(filtered()) > 0)
    p <- filtered() %>%
      filter(!is.na(price_per_m2), price_per_m2 > 0) %>%
      group_by(district_name) %>%
      summarise(median_price_m2 = median(price_per_m2, na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n >= 3) %>%
      slice_max(median_price_m2, n = 10) %>%
      mutate(
        tooltip = paste0(
          "Quận/huyện: ", district_name,
          "<br>Giá trung vị/m²: ", format_number_vi(median_price_m2 / 1e6, 1), " triệu VND",
          "<br>Số tin: ", format(n, big.mark = ".")
        )
      ) %>%
      ggplot(aes(x = reorder(district_name, median_price_m2), y = median_price_m2 / 1e6, text = tooltip)) +
      geom_col(fill = "#059669", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "Triệu VND/m²") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = "text")
  })

  output$price_category_plot <- renderPlotly({
    req(nrow(filtered()) > 0)
    p <- filtered() %>%
      filter(!is.na(price_b), price_b <= quantile(price_b, 0.98, na.rm = TRUE)) %>%
      mutate(
        tooltip = paste0("Loại BĐS: ", category_name, "<br>Giá: ", format_number_vi(price_b, 2), " tỷ VND")
      ) %>%
      ggplot(aes(x = category_name, y = price_b, fill = category_name, text = tooltip)) +
      geom_boxplot(outlier.alpha = 0.18) +
      scale_fill_manual(values = chart_palette) +
      coord_flip() +
      guides(fill = "none") +
      labs(x = NULL, y = "Giá (tỷ VND)") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = "text")
  })

  output$log_price_plot <- renderPlotly({
    req(nrow(filtered()) > 0)
    p <- filtered() %>%
      ggplot(aes(x = log1p(price))) +
      geom_histogram(bins = 28, fill = "#d97706", color = "white", linewidth = 0.2) +
      labs(x = "log(1 + giá)", y = "Số tin") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = c("x", "count")) %>%
      layout(
        xaxis = list(hoverformat = ".2f"),
        hovertemplate = "Log(1 + giá): %{x:.2f}<br>Số tin: %{y}<extra></extra>"
      )
  })

  prediction <- eventReactive(input$predict_btn, {
    req(input$pred_district, input$pred_category, input$predict_transaction)
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
        tooltip = paste0("Yếu tố: ", feature, "<br>Mức ảnh hưởng: ", format_number_vi(IncNodePurity, 1))
      ) %>%
      ggplot(aes(x = reorder(feature, IncNodePurity), y = IncNodePurity, text = tooltip)) +
      geom_col(fill = "#0072bc", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "IncNodePurity") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = "text")
  })

  output$cluster_plot <- renderPlotly({
    validate(need(file.exists(CLUSTER_PATH), "Chưa có kmeans_area_price.csv. Hãy chạy train_models.R."))
    p <- read_csv(CLUSTER_PATH, show_col_types = FALSE) %>%
      mutate(
        cluster = as.factor(cluster),
        tooltip = paste0(
          "Quận/huyện: ", district_name,
          "<br>Loại BĐS: ", category_name,
          "<br>Cụm: ", cluster,
          "<br>Diện tích trung vị: ", format_number_vi(median_area, 1), " m²",
          "<br>Giá/m² trung vị: ", format_number_vi(median_price_per_m2 / 1e6, 1), " triệu VND",
          "<br>Số tin: ", format(listing_count, big.mark = ".")
        )
      ) %>%
      ggplot(aes(x = median_area, y = median_price_per_m2 / 1e6, color = cluster, size = listing_count, text = tooltip)) +
      geom_point(alpha = 0.78) +
      scale_color_manual(values = chart_palette) +
      labs(x = "Diện tích trung vị (m²)", y = "Giá/m² trung vị (triệu VND)", color = "Cụm", size = "Số tin") +
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
          !is.na(ad_url) & ad_url != "",
          {
            fixed_url <- ifelse(grepl("\\.htm$", ad_url), ad_url, paste0(ad_url, ".htm"))
            paste0('<a href="', htmltools::htmlEscape(fixed_url), '" target="_blank" rel="noopener">Xem tin</a>')
          },
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
