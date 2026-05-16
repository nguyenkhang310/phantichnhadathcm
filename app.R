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
      district_name = if_else(is.na(district_name) | district_name == "", "Không rõ", as.character(district_name)),
      category_name = if_else(is.na(category_name) | category_name == "", "Không rõ", as.character(category_name)),
      ward = if_else(is.na(ward) | ward == "", "Không rõ", as.character(ward)),
      price = as.numeric(price),
      area = as.numeric(area),
      rooms = as.numeric(rooms),
      price_m = price / 1e6,
      price_b = price / 1e9,
      price_per_m2 = if_else(!is.na(area) & area > 0, price / area, NA_real_),
      posted_at = suppressWarnings(as_datetime(posted_at)),
      is_rent = as.logical(is_rent)
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

format_metric <- function(x) {
  vapply(x, function(value) {
    if (is.na(value) || !is.finite(value)) return("NA")
    format(round(value, 3), nsmall = 3)
  }, character(1))
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
  width: 96px;
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
  transition: transform .18s ease, box-shadow .18s ease, border-color .18s ease;
}
.app-panel:hover {
  transform: translateY(-1px);
  box-shadow: 0 20px 40px rgba(15, 56, 98, .12), 0 2px 8px rgba(15, 23, 42, .05);
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
.panel-head { padding: 13px 16px; border-bottom: 1px solid var(--border); display: flex; justify-content: space-between; gap: 12px; }
.panel-title { font-size: 14px; font-weight: 740; color: var(--fg); }
.panel-subtitle { font-size: 12px; color: var(--muted); margin-top: 2px; }
.panel-body { padding: 16px; }
.filter-panel .form-group { margin-bottom: 12px; }
.form-control, .selectize-input, .irs--shiny .irs-bar, .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { border-radius: 6px; }
.irs--shiny .irs-bar, .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: var(--primary); border-color: var(--primary); }
.btn-primary { background: var(--primary); border-color: var(--primary); border-radius: 6px; font-weight: 650; }
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
  .brand-logo { width: 44px; }
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
      div(class = "app-sidebar-footer", "Nguồn dữ liệu: Chợ Tốt API", br(), "© 2026 HCMUTE")
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
          span(class = "status-badge", span(class = "status-dot"), "Live Data")
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
              div(class = "page-subtitle", "Snapshot dữ liệu thu thập từ Chợ Tốt cho TP.HCM, cập nhật theo pipeline R."),
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
                textOutput("map_count", inline = TRUE),
                fluidRow(
                  column(3, uiOutput("map_district_filter")),
                  column(3, uiOutput("map_category_filter")),
                  column(3, sliderInput("map_price_range", "Khoảng giá (tỷ VND)", min = 0, max = 100, value = c(0, 100), step = 1)),
                  column(3, sliderInput("map_area_range", "Diện tích (m²)", min = 0, max = 1000, value = c(0, 1000), step = 10))
                )
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
                  uiOutput("district_filter"), uiOutput("category_filter"),
                  sliderInput("price_range", "Khoảng giá (tỷ VND)", min = 0, max = 100, value = c(0, 100), step = 1),
                  sliderInput("area_range", "Diện tích (m²)", min = 0, max = 1000, value = c(0, 1000), step = 10),
                  uiOutput("filter_summary")))),
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
                  uiOutput("predict_district"), uiOutput("predict_category"),
                  textInput("predict_ward", "Phường/xã", value = "Không rõ"),
                  numericInput("predict_area", "Diện tích (m²)", value = 75, min = 5, max = 5000),
                  numericInput("predict_rooms", "Số phòng", value = 2, min = 0, max = 20),
                  actionButton("predict_btn", "Tính lại dự đoán", icon = icon("calculator"), class = "btn-primary", width = "100%"))),
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
            app_panel("Bảng dữ liệu", "Có tìm kiếm và lọc nhanh theo từng cột", DTOutput("data_table")))),
          tabPanel(title = "about", value = "about", div(class = "page-wrap",
            h1(class = "page-title", "Về đồ án"),
            div(class = "page-subtitle", "Pipeline phân tích và dự đoán giá bất động sản TP.HCM bằng R."),
            app_panel("Pipeline thực hiện", NULL, tags$ul(
              tags$li("Thu thập dữ liệu từ API công khai của Chợ Tốt, lưu SQLite/CSV và chống trùng bằng ad_id."),
              tags$li("Làm sạch dữ liệu, xử lý missing values, tạo log features, khoảng cách tới trung tâm và target encoding theo phường/xã."),
              tags$li("Phân tích khám phá bằng ggplot2 và leaflet: phân phối giá, giá/m², cơ cấu loại BĐS, phân bố địa lý."),
              tags$li("Huấn luyện Linear Regression, Random Forest, XGBoost, ensemble và K-Means clustering."),
              tags$li("Triển khai dashboard Shiny để demo trực tiếp kết quả phân tích, bản đồ và dự đoán."))),
            app_panel("Sơ đồ pipeline", "Xem chi tiết trong README.md và docs/diagrams", tags$pre("Chợ Tốt API → SQLite/CSV → Feature Engineering → EDA → ML Models → Shiny Dashboard"))))
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

  filtered <- reactive({
    df <- listings()
    if (!is.null(input$districts) && length(input$districts) > 0) {
      df <- df %>% filter(district_name %in% input$districts)
    }
    if (!is.null(input$categories) && length(input$categories) > 0) {
      df <- df %>% filter(category_name %in% input$categories)
    }
    df %>%
      filter(
        price_b >= input$price_range[1], price_b <= input$price_range[2],
        area >= input$area_range[1], area <= input$area_range[2]
      )
  })

  map_filtered <- reactive({
    df <- listings()
    if (!is.null(input$map_districts) && length(input$map_districts) > 0) {
      df <- df %>% filter(district_name %in% input$map_districts)
    }
    if (!is.null(input$map_categories) && length(input$map_categories) > 0) {
      df <- df %>% filter(category_name %in% input$map_categories)
    }
    df %>%
      filter(
        price_b >= input$map_price_range[1], price_b <= input$map_price_range[2],
        area >= input$map_area_range[1], area <= input$map_area_range[2],
        !is.na(lat), !is.na(lon)
      )
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

  output$district_filter <- renderUI({
    choices <- sort(unique(listings()$district_name))
    selectizeInput("districts", "Quận/huyện", choices = choices, selected = choices, multiple = TRUE)
  })

  output$category_filter <- renderUI({
    choices <- sort(unique(listings()$category_name))
    selectizeInput("categories", "Loại bất động sản", choices = choices, selected = choices, multiple = TRUE)
  })

  output$map_district_filter <- renderUI({
    choices <- sort(unique(listings()$district_name))
    selectizeInput("map_districts", "Quận/huyện", choices = choices, selected = choices, multiple = TRUE)
  })

  output$map_category_filter <- renderUI({
    choices <- sort(unique(listings()$category_name))
    selectizeInput("map_categories", "Loại bất động sản", choices = choices, selected = choices, multiple = TRUE)
  })

  output$predict_district <- renderUI({
    selectInput("pred_district", "Quận/huyện", choices = sort(unique(listings()$district_name)))
  })

  output$predict_category <- renderUI({
    selectInput("pred_category", "Loại bất động sản", choices = sort(unique(listings()$category_name)))
  })

  output$filter_summary <- renderUI({
    df <- filtered()
    tags$div(
      style = "border:1px solid var(--border);background:#f8fafc;border-radius:6px;padding:10px;font-size:12px;",
      tags$div(tags$b("Số tin sau lọc: "), format(nrow(df), big.mark = ",")),
      tags$div(tags$b("Giá trung vị: "), format_vnd_full(median(df$price, na.rm = TRUE)))
    )
  })

  output$map_count <- renderText({
    paste0(format(nrow(map_filtered()), big.mark = ","), " tin đăng hiển thị")
  })

  output$district_plot <- renderPlotly({
    req(nrow(listings()) > 0)
    p <- listings() %>%
      count(district_name, sort = TRUE) %>%
      slice_head(n = 12) %>%
      ggplot(aes(x = reorder(district_name, n), y = n)) +
      geom_col(fill = "#0072bc", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "Số tin") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = c("x", "y"))
  })

  output$category_plot <- renderPlotly({
    req(nrow(listings()) > 0)
    p <- listings() %>%
      count(category_name, sort = TRUE) %>%
      ggplot(aes(x = reorder(category_name, n), y = n, fill = category_name)) +
      geom_col(width = 0.72) +
      scale_fill_manual(values = chart_palette) +
      coord_flip() +
      guides(fill = "none") +
      labs(x = NULL, y = "Số tin") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = c("x", "y"))
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
      ggplot(aes(x = area, y = price_b, color = category_name)) +
      geom_point(alpha = 0.55, size = 1.7) +
      scale_color_manual(values = chart_palette) +
      labs(x = "Diện tích (m²)", y = "Giá (tỷ VND)", color = "Loại") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = c("x", "y", "colour"))
  })

  output$price_m2_plot <- renderPlotly({
    req(nrow(filtered()) > 0)
    p <- filtered() %>%
      filter(!is.na(price_per_m2), price_per_m2 > 0) %>%
      group_by(district_name) %>%
      summarise(median_price_m2 = median(price_per_m2, na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n >= 3) %>%
      slice_max(median_price_m2, n = 10) %>%
      ggplot(aes(x = reorder(district_name, median_price_m2), y = median_price_m2 / 1e6)) +
      geom_col(fill = "#059669", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "Triệu VND/m²") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = c("x", "y"))
  })

  output$price_category_plot <- renderPlotly({
    req(nrow(filtered()) > 0)
    p <- filtered() %>%
      filter(!is.na(price_b), price_b <= quantile(price_b, 0.98, na.rm = TRUE)) %>%
      ggplot(aes(x = category_name, y = price_b, fill = category_name)) +
      geom_boxplot(outlier.alpha = 0.18) +
      scale_fill_manual(values = chart_palette) +
      coord_flip() +
      guides(fill = "none") +
      labs(x = NULL, y = "Giá (tỷ VND)") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = c("x", "y"))
  })

  output$log_price_plot <- renderPlotly({
    req(nrow(filtered()) > 0)
    p <- filtered() %>%
      ggplot(aes(x = log1p(price))) +
      geom_histogram(bins = 28, fill = "#d97706", color = "white", linewidth = 0.2) +
      labs(x = "log(1 + giá)", y = "Số tin") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = c("x", "y"))
  })

  prediction <- eventReactive(input$predict_btn, {
    req(input$pred_district, input$pred_category)
    sample_row <- listings() %>% slice(1)
    if (nrow(sample_row) == 0) return(NA_real_)

    input_row <- sample_row
    input_row$district_name <- input$pred_district
    input_row$category_name <- input$pred_category
    input_row$ward <- input$predict_ward
    input_row$area <- input$predict_area
    input_row$rooms <- input$predict_rooms
    input_row$posted_hour <- hour(Sys.time())
    input_row$posted_wday <- wday(Sys.time(), label = TRUE)
    input_row$is_rent <- input$pred_category %in% c("Phòng trọ", "Văn phòng, Mặt bằng kinh doanh", "Văn phòng/Mặt bằng")

    predict_price(input_row, input_row$is_rent)
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
      paste("Loại:", input$pred_category, "· Khu vực:", input$pred_district, "· Diện tích:", input$predict_area, "m²")
    }
  })

  output$importance_plot <- renderPlotly({
    is_rent_pred <- isTRUE(input$pred_category %in% c("Phòng trọ", "Văn phòng, Mặt bằng kinh doanh", "Văn phòng/Mặt bằng"))
    path <- if (is_rent_pred) {
      if (file.exists("models/rf_importance_rent.csv")) "models/rf_importance_rent.csv" else "models/rf_importance_sale.csv"
    } else {
      if (file.exists("models/rf_importance_sale.csv")) "models/rf_importance_sale.csv" else "models/rf_importance_rent.csv"
    }
    validate(need(file.exists(path), "Chưa có feature importance."))
    p <- read_csv(path, show_col_types = FALSE) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(feature, IncNodePurity), y = IncNodePurity)) +
      geom_col(fill = "#0072bc", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "IncNodePurity") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = c("x", "y"))
  })

  output$cluster_plot <- renderPlotly({
    validate(need(file.exists(CLUSTER_PATH), "Chưa có kmeans_area_price.csv. Hãy chạy train_models.R."))
    p <- read_csv(CLUSTER_PATH, show_col_types = FALSE) %>%
      mutate(cluster = as.factor(cluster)) %>%
      ggplot(aes(x = median_area, y = median_price_per_m2 / 1e6, color = cluster, size = listing_count)) +
      geom_point(alpha = 0.78) +
      scale_color_manual(values = chart_palette) +
      labs(x = "Diện tích trung vị (m²)", y = "Giá/m² trung vị (triệu VND)", color = "Cụm", size = "Số tin") +
      theme_minimal(base_size = 12)
    interactive_chart(p, tooltip = c("x", "y", "colour", "size"))
  })

  output$data_table <- renderDT({
    listings() %>%
      transmute(
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
        filter = "top",
        escape = c(0, 1, 2, 3, 4, 5, 6),
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          language = list(search = "Tìm kiếm:", lengthMenu = "Hiển thị _MENU_ dòng")
        )
      )
  })
}

shinyApp(ui, server)
