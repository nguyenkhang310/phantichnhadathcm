# ============================================================
# GIAO DIỆN ỨNG DỤNG
# Khai báo layout, sidebar, các tab và input/output placeholder của Shiny.
# ============================================================

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
    tags$link(rel = "stylesheet", href = "giao_dien.css?v=simulation-toolbar-center-20260619"),
    tags$script(src = "tuong_tac.js")
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
      tags$nav(
        class = "app-sidebar-nav",
        nav_link("overview", "Tổng quan", "chart-line"),
        nav_link("map", "Bản đồ dữ liệu", "map-location-dot"),
        nav_link("analysis", "Phân tích giá", "chart-column"),
        nav_link("statistics", "Suy luận thống kê", "square-root-variable"),
        nav_link("predict", "Dự đoán giá", "calculator"),
        nav_link("diagnostics", "Đánh giá model", "clipboard-check"),
        nav_link("clusters", "Phân cụm khu vực", "layer-group"),
        nav_link("data", "Dữ liệu", "table"),
        nav_link("assistant", "Trợ lý BĐS", "comments")
      ),
      div(class = "app-sidebar-footer", "Nhóm 21 Lập Trình R", br(), "© 2026 HCMUTE")
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
          div(
            class = "report-toolbar",
            # --- Chọn khu vực ---
            div(
              class = "report-search",
              div(class = "report-search-icon", icon("location-dot")),
              uiOutput("report_district_picker")
            ),
            # --- Thống kê nhanh ---
            tags$span(class = "report-divider"),
            uiOutput("report_quick_insight"),
            # --- Xuất PDF ---
            tags$span(class = "report-divider"),
            downloadLink(
              "download_district_report",
              label = tagList(icon("file-arrow-down"), span("Xuất PDF")),
              class = "report-download-button",
              title = "Xuất báo cáo PDF cho khu vực đang chọn"
            )
          )
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
                column(8, app_panel("Số tin theo khu vực cũ", "Top khu vực có nhiều tin đăng nhất", chart_mode_control("district_plot_tx"), plotlyOutput("district_plot", height = 330))),
                column(4, app_panel("Cơ cấu loại bất động sản", "Tỉ trọng theo số lượng tin", chart_mode_control("category_plot_tx"), plotlyOutput("category_plot", height = 330)))
              ),
              app_panel(
                "Hiệu năng mô hình",
                "Train/Test là tập validate 80/20; model cuối đã refit trên toàn bộ dữ liệu sạch.",
                tableOutput("metrics_table")
              )
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
                  filter_field("Khoảng giá", sliderInput("map_price_range", NULL, min = 0, max = 500, value = c(0, 500), step = 1, post = " tỷ", ticks = FALSE), icon_name = "coins"),
                  filter_field("Diện tích", sliderInput("map_area_range", NULL, min = 0, max = 5000, value = c(0, 5000), step = 10, post = " m²", ticks = FALSE), icon_name = "ruler-combined"),
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
                    span(class = "dot dot-high", style = "margin-left:8px;"), "Cao",
                    div(style = "margin-top:8px;color:#64748b;font-size:12px;", "Marker mờ: app tự ước lượng vị trí theo khu vực"))
              )
            )
          ),
          # ======================================================
          # UI - PHAN TICH GIA / EDA
          # Cac panel trong tab nay tra loi cau hoi thi truong:
          # phan phoi gia, quan he dien tich-gia, gia/m2 theo khu vuc,
          # heatmap khu vuc x loai BDS, xu huong thoi gian va tuong quan.
          # ======================================================
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
                  filter_field("Khoảng giá", sliderInput("price_range", NULL, min = 0, max = 500, value = c(0, 500), step = 1, post = " tỷ", ticks = FALSE), icon_name = "coins"),
                  filter_field("Diện tích", sliderInput("area_range", NULL, min = 0, max = 5000, value = c(0, 5000), step = 10, post = " m²", ticks = FALSE), icon_name = "ruler-combined"),
                  filter_actions("reset_analysis_filters"),
                  uiOutput("filter_summary")), class = "filter-card")),
                column(9, app_panel("Diện tích vs Giá", "Mỗi điểm là một tin đăng, màu theo loại BĐS", chart_mode_control("area_price_tx"), plotlyOutput("area_price_plot", height = 430)))
              ),
              fluidRow(
                column(4, app_panel("Top khu vực cũ theo giá/m²", "Đơn vị tự đổi theo bán hoặc cho thuê", chart_mode_control("price_m2_tx"), plotlyOutput("price_m2_plot", height = 310))),
                column(4, app_panel("Khoảng giá theo loại BĐS", "Điểm là giá trung vị, thanh ngang là vùng giá phổ biến", chart_mode_control("price_category_tx"), plotlyOutput("price_category_plot", height = 310))),
                column(4, app_panel("Phân phối giá", "Giá được chuẩn hóa để biểu đồ dễ quan sát hơn", chart_mode_control("log_price_tx"), plotlyOutput("log_price_plot", height = 310)))
              ),
              fluidRow(
                column(6, app_panel("Heatmap khu vực x loại BĐS", "Màu thể hiện giá/m² trung vị", chart_mode_control("district_category_heatmap_tx"), plotlyOutput("district_category_heatmap", height = 390))),
                column(6, app_panel("Radar nguồn dữ liệu", "Tỷ trọng nguồn trong nhóm giao dịch đang chọn", chart_mode_control("source_radar_tx"), plotlyOutput("source_sunburst_plot", height = 390)))
              ),
              fluidRow(
                column(6, app_panel("Xu hướng theo thời gian", "Loại các ngày đăng trong tương lai để tránh nhiễu", chart_mode_control("time_trend_tx"), plotlyOutput("time_trend_plot", height = 340))),
                column(6, app_panel("Tương quan biến số", "Correlation trên các biến số chính", chart_mode_control("correlation_tx"), plotlyOutput("correlation_plot", height = 340)))
              ),
              app_panel(
                "ECDF giá/m²",
                "So đường phân phối tích lũy để nhìn percentile và độ lệch giữa nhóm",
                chart_mode_control("ecdf_tx"),
                plotlyOutput("price_ecdf_plot", height = 340)
              )
            )
          ),
          # ======================================================
          # UI - SUY LUAN THONG KE
          # Tab nay ap dung truc tiep ly thuyet xac suat thong ke:
          # xac suat co dieu kien, CLT, bootstrap CI va kiem dinh gia thuyet.
          # ======================================================
          tabPanel(
            title = "statistics", value = "statistics",
            div(
              class = "page-wrap",
              h1(class = "page-title", "Suy luận thống kê"),
              div(class = "page-subtitle", "Áp dụng xác suất, phân phối mẫu, kiểm định giả thuyết, CLT và bootstrap trực tiếp trên dữ liệu BĐS TP.HCM."),
              uiOutput("stat_kpi_cards"),
              app_panel(
                "Thiết lập phân tích",
                NULL,
                div(
                  class = "filter-toolbar stat-toolbar",
                  filter_field("Nguồn", uiOutput("stat_source_filter"), icon_name = "database"),
                  filter_field("Giao dịch", selectInput("stat_transaction", NULL, choices = c("Bán", "Cho thuê"), selected = "Bán", selectize = FALSE), icon_name = "tags"),
                  filter_field("Loại BĐS", uiOutput("stat_category_filter"), icon_name = "building"),
                  filter_field("Khu vực A", uiOutput("stat_district_a_filter"), icon_name = "location-dot"),
                  filter_field("Khu vực B", uiOutput("stat_district_b_filter"), icon_name = "code-compare")
                ),
                div(
                  class = "filter-toolbar stat-toolbar compact",
                  filter_field("Mức tin cậy", selectInput("stat_confidence", NULL, choices = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99), selected = 0.95, selectize = FALSE), icon_name = "shield-halved")
                ),
                uiOutput("stat_filter_summary"),
                class = "filter-card"
              ),
              fluidRow(
                column(6, app_panel("Xác suất có điều kiện", "P(loại BĐS | khu vực) theo số tin", plotlyOutput("probability_heatmap", height = 390))),
                column(6, app_panel("Phân phối giá/m²", "ECDF giữa hai khu vực được chọn", plotlyOutput("stat_distribution_plot", height = 390)))
              ),
              app_panel(
                "Thiết lập mô phỏng",
                "Cỡ mẫu dùng cho CLT; số lần lặp dùng chung cho CLT và Bootstrap CI bên dưới",
                div(
                  class = "filter-toolbar stat-toolbar simulation-toolbar",
                  filter_field("Cỡ mẫu CLT", sliderInput("stat_sample_size", NULL, min = 10, max = 300, value = 50, step = 10, ticks = FALSE), icon_name = "dice"),
                  filter_field("Số lần lặp", sliderInput("stat_reps", NULL, min = 200, max = 1500, value = 600, step = 100, ticks = FALSE), icon_name = "rotate")
                ),
                class = "filter-card"
              ),
              fluidRow(
                column(6, app_panel(
                  "CLT simulation",
                  "Phân phối trung bình mẫu khi lấy mẫu có hoàn lại",
                  plotlyOutput("clt_plot", height = 360)
                )),
                column(6, app_panel("Bootstrap CI", "Khoảng tin cậy bootstrap cho trung vị giá/m² khu vực A", plotlyOutput("bootstrap_plot", height = 360)))
              ),
              fluidRow(
                column(7, app_panel("Kiểm định giả thuyết", "H0: giá/m² trung bình log-scale của hai khu vực bằng nhau", tableOutput("hypothesis_table"))),
                column(5, app_panel("Bảng xác suất thực nghiệm", "Các xác suất nổi bật trong dữ liệu đã lọc", tableOutput("empirical_probability_table")))
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
                    filter_field("Khu vực cũ", uiOutput("predict_district"), icon_name = "location-dot"),
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
                  uiOutput("prediction_market_band"),
                  br(),
                  app_panel("Các yếu tố ảnh hưởng chính", "Feature importance từ Random Forest", plotlyOutput("importance_plot", height = 260), class = "nested-panel")))
              )
            )
          ),
          tabPanel(
            title = "diagnostics", value = "diagnostics",
            div(
              class = "page-wrap",
              h1(class = "page-title", "Đánh giá model"),
              div(class = "page-subtitle", "Đọc model như một hệ thống dự báo: chỉ số tổng quan, sai số, residual và các nhóm dễ dự đoán sai."),
              app_panel(
                "Bộ lọc model",
                NULL,
                div(class = "filter-toolbar stat-toolbar compact",
                  filter_field("Giao dịch", chart_mode_control("diagnostic_tx"), icon_name = "tags")
                ),
                class = "filter-card"
              ),
              uiOutput("model_card_ui"),
              fluidRow(
                column(6, app_panel("Actual vs Predicted", "Đường chéo là dự đoán hoàn hảo; hover để xem loại BĐS", plotlyOutput("diagnostic_scatter_plot", height = 460))),
                column(6, app_panel("Residual distribution", "Sai số log(actual) - log(predicted)", plotlyOutput("diagnostic_residual_plot", height = 460)))
              ),
              fluidRow(
                column(6, app_panel("Sai số theo khu vực", "Top nhóm có MAPE cao trong mẫu chẩn đoán", plotlyOutput("diagnostic_error_group_plot", height = 430))),
                column(6, app_panel("So sánh chỉ số model", "RMSE, MAE, MAPE và R² theo từng thuật toán", plotlyOutput("metrics_compare_plot", height = 430)))
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
                filter_field("Khoảng giá", sliderInput("data_price_range", NULL, min = 0, max = 500, value = c(0, 500), step = 1, post = " tỷ", ticks = FALSE), icon_name = "coins"),
                filter_actions("reset_data_filters")
              ),
              class = "filter-card"
            ),
            uiOutput("data_quality_cards"),
            fluidRow(
              column(6, app_panel("Kiểm tra chất lượng dữ liệu", "Các cảnh báo không bị xóa tự động, chỉ dùng để đọc kết quả cẩn thận", tableOutput("data_quality_table"))),
              column(6, app_panel("Độ phủ nguồn dữ liệu", "Số dòng và tỷ lệ tọa độ gốc theo nguồn", plotlyOutput("data_quality_plot", height = 340)))
            ),
            app_panel("Bảng dữ liệu", "Có tìm kiếm nhanh trong bảng", DTOutput("data_table")))),
          tabPanel(title = "assistant", value = "assistant", div(class = "gemini-page-wrap",
            div(
              class = "gemini-wrapper",
              
              # Khung chat va loi chao
              div(
                class = "gemini-chat-container",
                uiOutput("gemini_chat_view")
              ),
              
              # Thanh nhap noi o cuoi man hinh
              div(
                class = "gemini-input-container-wrap",
                div(
                  class = "gemini-input-bar",
                  # Nut cong trang tri
                  actionLink("assistant_add", label = icon("plus"), class = "gemini-input-btn"),
                  
                  # O nhap cau hoi
                  textAreaInput(
                    "assistant_question",
                    NULL,
                    value = "",
                    rows = 1,
                    placeholder = "Hỏi trợ lý BDS..."
                  ),
                  
                  # Cum dieu khien ben phai
                  div(
                    class = "gemini-input-right",
                    # Nhan che do xu ly local
                    div(
                      class = "gemini-model-badge",
                      span("R-Tools"),
                      icon("chevron-down")
                    ),
                    # Nut micro
                    actionLink("assistant_mic", label = icon("microphone"), class = "gemini-input-btn"),
                    # Nut gui cau hoi
                    actionButton(
                      "assistant_send",
                      label = icon("arrow-up"),
                      class = "gemini-btn-send"
                    )
                  )
                )
              ),
              
              # Ghi chu nho va nut xoa hoi thoai
              div(
                class = "gemini-footer-note",
                span("BDS dùng tool dữ liệu local, model dự đoán và memory hội thoại. Không dùng API ngoài."),
                actionLink("assistant_clear", label = tagList(icon("trash-can"), "Xóa hội thoại"), class = "gemini-clear-link")
              )
            )
          ))
        )
      )
    )
  )
)
