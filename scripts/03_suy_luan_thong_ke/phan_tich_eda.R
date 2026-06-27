source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()

required_packages <- c("dplyr", "readr", "lubridate", "ggplot2", "tidyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Thieu package: ", paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(tidyr)
})

load_eda_data <- function() {
  if (file.exists(PATHS$featured_csv)) return(read_csv(PATHS$featured_csv, show_col_types = FALSE))
  if (file.exists(PATHS$combined_raw_csv)) return(read_csv(PATHS$combined_raw_csv, show_col_types = FALSE))
  stop("Chua co data. Hay chay pipeline xu ly du lieu truoc.")
}

format_count_axis <- function(x) {
  format(round(x, 0), big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

filter_quantile <- function(data, column, low = 0.01, high = 0.99) {
  data %>%
    group_by(transaction_type) %>%
    mutate(
      q_low = quantile(.data[[column]], low, na.rm = TRUE),
      q_high = quantile(.data[[column]], high, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(.data[[column]] >= q_low, .data[[column]] <= q_high) %>%
    select(-q_low, -q_high)
}

save_plot <- function(plot, filename, width = 10, height = 6) {
  ggsave(file.path(PATHS$plot_dir, filename), plot, width = width, height = height, dpi = 160)
}

run_eda <- function() {
  dir.create(PATHS$plot_dir, showWarnings = FALSE, recursive = TRUE)

  df <- load_eda_data() %>%
    mutate(
      price = as.numeric(price),
      area = as.numeric(area),
      price_per_m2 = if_else(!is.na(price_per_m2), as.numeric(price_per_m2), if_else(area > 0, price / area, NA_real_)),
      posted_at = suppressWarnings(as_datetime(posted_at)),
      transaction_type = case_when(
        "transaction_type" %in% names(.) & transaction_type %in% c("Bán", "Cho thuê") ~ as.character(transaction_type),
        "is_rent" %in% names(.) & as.logical(is_rent) ~ "Cho thuê",
        TRUE ~ "Bán"
      ),
      source = if_else(is.na(source) | source == "", "unknown", as.character(source)),
      district_name = if_else(is.na(district_name) | district_name == "", "Không rõ", as.character(district_name)),
      category_name = if_else(is.na(category_name) | category_name == "", "Không rõ", as.character(category_name)),
      price_display = if_else(transaction_type == "Bán", price / 1e9, price / 1e6),
      price_unit = if_else(transaction_type == "Bán", "Tỷ VND", "Triệu VND")
    ) %>%
    filter(!is.na(price), price > 0, !is.na(area), area > 0)

  summary_tbl <- df %>%
    group_by(transaction_type, source, district_name, category_name) %>%
    summarise(
      listings = n(),
      median_price = median(price, na.rm = TRUE),
      median_area = median(area, na.rm = TRUE),
      median_price_per_m2 = median(price_per_m2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(listings))
  write_csv(summary_tbl, PATHS$eda_summary_csv)

  p1 <- ggplot(df, aes(x = log1p(price), fill = transaction_type)) +
    geom_histogram(bins = 40, color = "white", alpha = 0.85) +
    facet_wrap(~ transaction_type, scales = "free_y") +
    guides(fill = "none") +
    labs(title = "Phân phối log(giá)", x = "log(1 + giá)", y = "Số tin") +
    theme_minimal(base_size = 12)
  save_plot(p1, "01_phan_phoi_log_gia.png")

  p2 <- df %>%
    filter_quantile("price_display") %>%
    ggplot(aes(x = reorder(district_name, price_display, median), y = price_display)) +
    geom_boxplot(fill = "#dbeafe", outlier.alpha = 0.2) +
    facet_wrap(~ paste(transaction_type, price_unit), scales = "free") +
    coord_flip() +
    labs(title = "Phân phối giá theo khu vực", x = NULL, y = "Giá") +
    theme_minimal(base_size = 12)
  save_plot(p2, "02_gia_theo_quan_boxplot.png")

  p3 <- df %>%
    filter_quantile("price_display") %>%
    filter_quantile("area") %>%
    ggplot(aes(x = area, y = price_display, color = category_name)) +
    geom_point(alpha = 0.45, size = 1.2) +
    facet_wrap(~ paste(transaction_type, price_unit), scales = "free_y") +
    labs(title = "Diện tích và giá", x = "Diện tích (m²)", y = "Giá", color = "Loại BĐS") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  save_plot(p3, "03_dien_tich_va_gia.png")

  p4 <- df %>%
    count(district_name, transaction_type, name = "listings") %>%
    group_by(district_name) %>%
    mutate(total = sum(listings)) %>%
    ungroup() %>%
    slice_max(total, n = 12) %>%
    ggplot(aes(x = reorder(district_name, total), y = listings, fill = transaction_type)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = format_count_axis) +
    labs(title = "Top khu vực có nhiều tin", x = NULL, y = "Số tin", fill = "Giao dịch") +
    theme_minimal(base_size = 12)
  save_plot(p4, "04_top_quan_nhieu_tin.png")

  p5 <- df %>%
    filter(!is.na(price_per_m2), price_per_m2 > 0) %>%
    group_by(transaction_type, district_name) %>%
    summarise(listings = n(), median_price_per_m2 = median(price_per_m2, na.rm = TRUE), .groups = "drop") %>%
    group_by(transaction_type) %>%
    filter(listings >= 3) %>%
    slice_max(median_price_per_m2, n = 10) %>%
    ungroup() %>%
    mutate(display_m2 = if_else(transaction_type == "Bán", median_price_per_m2 / 1e6, median_price_per_m2 / 1e3)) %>%
    ggplot(aes(x = reorder(district_name, display_m2), y = display_m2, fill = transaction_type)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ transaction_type, scales = "free") +
    coord_flip() +
    labs(title = "Top khu vực theo giá/m² trung vị", x = NULL, y = "Triệu VND/m² hoặc nghìn VND/m²") +
    theme_minimal(base_size = 12)
  save_plot(p5, "05_top_quan_gia_m2_cao.png")

  p6 <- df %>%
    mutate(posted_month = floor_date(as.Date(posted_at), "month")) %>%
    filter(!is.na(posted_month), posted_month <= Sys.Date()) %>%
    count(posted_month, transaction_type) %>%
    ggplot(aes(x = posted_month, y = n, color = transaction_type)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.5) +
    scale_y_continuous(labels = format_count_axis) +
    labs(title = "Xu hướng số tin theo tháng", x = "Tháng", y = "Số tin", color = "Giao dịch") +
    theme_minimal(base_size = 12)
  save_plot(p6, "06_xu_huong_tin_theo_ngay.png")

  if (all(c("lat", "lon") %in% names(df))) {
    p7 <- df %>%
      filter(!is.na(lat), !is.na(lon), lat >= 10.3, lat <= 11.2, lon >= 106.0, lon <= 107.3) %>%
      ggplot(aes(x = lon, y = lat, color = transaction_type)) +
      geom_point(alpha = 0.45, size = 1.1) +
      coord_fixed() +
      facet_wrap(~ transaction_type) +
      labs(title = "Phân bố tọa độ tin đăng", x = "Kinh độ", y = "Vĩ độ", color = "Giao dịch") +
      theme_minimal(base_size = 12)
    save_plot(p7, "07_phan_bo_dia_ly_gia.png")
  }

  p8 <- df %>%
    filter(!is.na(price_per_m2), price_per_m2 > 0) %>%
    filter_quantile("price_per_m2") %>%
    add_count(category_name, transaction_type, name = "category_n") %>%
    filter(category_n >= 10) %>%
    mutate(display_m2 = if_else(transaction_type == "Bán", price_per_m2 / 1e6, price_per_m2 / 1e3)) %>%
    ggplot(aes(x = reorder(category_name, display_m2, median), y = display_m2, fill = category_name)) +
    geom_boxplot(outlier.alpha = 0.2) +
    facet_wrap(~ transaction_type, scales = "free") +
    coord_flip() +
    guides(fill = "none") +
    labs(title = "Giá/m² theo loại bất động sản", x = NULL, y = "Triệu VND/m² hoặc nghìn VND/m²") +
    theme_minimal(base_size = 12)
  save_plot(p8, "08_gia_m2_theo_loai_bds.png")

  cat("Da tao EDA PNG va bang tom tat tai thu muc plots.\n")
  invisible(summary_tbl)
}

if (sys.nframe() == 0) {
  run_eda()
}
