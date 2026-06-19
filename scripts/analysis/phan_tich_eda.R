# ============================================================
# EDA & VISUALIZATION - BDS TP.HCM
# Chay: Rscript scripts/analysis/phan_tich_eda.R
# Output: plots/*.png va plots/tom_tat_eda_hcm.csv
# ============================================================

source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()

required_packages <- c("dplyr", "readr", "lubridate", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Thieu package: ", paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

FEATURED_CSV <- PATHS$featured_csv
RAW_CSV <- PATHS$combined_raw_csv
PLOT_DIR <- PATHS$plot_dir

# Chuẩn hóa các nhãn loại BĐS bị khác nhau giữa nguồn crawl.
normalize_category <- function(category) {
  category <- as.character(category)
  dplyr::case_when(
    grepl("căn hộ|chung cư", category, ignore.case = TRUE) ~ "Căn hộ / chung cư",
    grepl("phòng|nhà trọ", category, ignore.case = TRUE) ~ "Phòng / nhà trọ",
    grepl("văn phòng|mặt bằng|shop", category, ignore.case = TRUE) ~ "Văn phòng / mặt bằng",
    grepl("đất nền", category, ignore.case = TRUE) ~ "Đất nền",
    grepl("^đất$", category, ignore.case = TRUE) ~ "Đất",
    grepl("nhà phố|nhà ở|nhà đất", category, ignore.case = TRUE) ~ "Nhà phố / nhà ở",
    grepl("biệt thự", category, ignore.case = TRUE) ~ "Biệt thự",
    grepl("kho|xưởng", category, ignore.case = TRUE) ~ "Kho xưởng",
    grepl("khách sạn", category, ignore.case = TRUE) ~ "Khách sạn",
    TRUE ~ "Khác"
  )
}

filter_quantile_by_transaction <- function(data, col, low = 0.01, high = 0.99) {
  data %>%
    group_by(transaction_type) %>%
    mutate(
      q_low = quantile(.data[[col]], low, na.rm = TRUE),
      q_high = quantile(.data[[col]], high, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(.data[[col]] >= q_low, .data[[col]] <= q_high) %>%
    select(-q_low, -q_high)
}

label_count <- function(x) {
  format(round(x, 0), big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

label_money <- function(x) {
  format(round(x, 1), big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

# Hàm load_eda_data: nạp dữ liệu từ file, API hoặc cache.
load_eda_data <- function() {
  if (file.exists(FEATURED_CSV)) {
    return(read_csv(FEATURED_CSV, show_col_types = FALSE))
  }
  if (file.exists(RAW_CSV)) {
    return(read_csv(RAW_CSV, show_col_types = FALSE))
  }
  stop("Chua co data. Hay chay scripts/processing/gop_nguon_du_lieu.R va scripts/processing/tao_dac_trung.R truoc.")
}

# Hàm save_plot: lưu hoặc cập nhật dữ liệu đầu ra.
save_plot <- function(plot, filename, width = 10, height = 6) {
  ggsave(file.path(PLOT_DIR, filename), plot, width = width, height = height, dpi = 160)
}

# Hàm run_eda: chạy toàn bộ bước xử lý chính.
run_eda <- function() {
  dir.create(PLOT_DIR, showWarnings = FALSE)

  df <- load_eda_data() %>%
    mutate(
      posted_at = suppressWarnings(as_datetime(posted_at)),
      transaction_type = case_when(
        "transaction_type" %in% names(.) & transaction_type %in% c("Bán", "Cho thuê") ~ as.character(transaction_type),
        "is_rent" %in% names(.) & as.logical(is_rent) ~ "Cho thuê",
        TRUE ~ "Bán"
      ),
      is_rent = transaction_type == "Cho thuê",
      category_group = normalize_category(category_name),
      price_per_m2 = if_else(!is.na(area) & area > 0, price / area, NA_real_)
    ) %>%
    filter(
      !is.na(price), price > 0,
      !is.na(district_name), district_name != "",
      !is.na(category_name), category_name != ""
    )

  if (!"price_m" %in% names(df)) {
    df <- df %>% mutate(price_m = price / 1e6)
  }

  summary_tbl <- df %>%
    group_by(transaction_type, district_name, category_group) %>%
    summarise(
      listings = n(),
      median_price_m = median(price_m, na.rm = TRUE),
      median_area = median(area, na.rm = TRUE),
      median_price_per_m2 = median(price_per_m2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(listings))

  write_csv(summary_tbl, PATHS$eda_summary_csv)

  p1 <- ggplot(df, aes(x = log1p(price), fill = transaction_type)) +
    geom_histogram(bins = 45, color = "white", alpha = 0.9) +
    facet_wrap(~ transaction_type, scales = "free_y") +
    guides(fill = "none") +
    labs(
      title = "Phan phoi log(gia) theo loai giao dich",
      x = "log(1 + gia niem yet VND)",
      y = "So tin"
    ) +
    theme_minimal(base_size = 12)
  save_plot(p1, "01_phan_phoi_log_gia.png")

  price_by_district <- df %>%
    filter_quantile_by_transaction("price_m", low = 0.01, high = 0.99) %>%
    mutate(
      price_display = if_else(transaction_type == "Bán", price / 1e9, price / 1e6),
      transaction_label = if_else(transaction_type == "Bán", "Bán (tỷ VND)", "Cho thuê (triệu VND)")
    )

  p2 <- ggplot(price_by_district, aes(x = reorder(district_name, price_display, median), y = price_display)) +
    geom_boxplot(fill = "#D9EAF7", outlier.alpha = 0.25) +
    facet_wrap(~ transaction_label, scales = "free_x") +
    coord_flip() +
    labs(
      title = "Phan phoi gia theo quan/huyen",
      x = NULL,
      y = "Gia niem yet"
    ) +
    theme_minimal(base_size = 12)
  save_plot(p2, "02_gia_theo_quan_boxplot.png")

  area_price_vis <- df %>%
    filter(!is.na(area), area > 0) %>%
    filter_quantile_by_transaction("area", low = 0.01, high = 0.99) %>%
    filter_quantile_by_transaction("price_m", low = 0.01, high = 0.99) %>%
    mutate(
      price_display = if_else(transaction_type == "Bán", price / 1e9, price / 1e6),
      transaction_label = if_else(transaction_type == "Bán", "Bán (tỷ VND)", "Cho thuê (triệu VND)")
    )

  p3 <- area_price_vis %>%
    ggplot(aes(x = area, y = price_display, color = category_group)) +
    geom_point(alpha = 0.45, size = 1.4) +
    facet_wrap(~ transaction_label, scales = "free_y") +
    labs(
      title = "Dien tich va gia theo loai BDS",
      subtitle = "Da loc 1% diem cuc tri theo tung loai giao dich de bieu do de doc hon",
      x = "Dien tich (m2)",
      y = "Gia niem yet",
      color = "Loai"
    ) +
    theme_minimal(base_size = 12)
  save_plot(p3, "03_dien_tich_va_gia.png")

  top_districts <- df %>%
    count(district_name, name = "listings", sort = TRUE) %>%
    slice_max(listings, n = 10) %>%
    pull(district_name)

  p4 <- df %>%
    filter(district_name %in% top_districts) %>%
    count(district_name, transaction_type, name = "listings") %>%
    ggplot(aes(x = reorder(district_name, listings, sum), y = listings, fill = transaction_type)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = label_count) +
    labs(title = "Top 10 quan/huyen co nhieu tin nhat", x = NULL, y = "So tin", fill = "Giao dich") +
    theme_minimal(base_size = 12)
  save_plot(p4, "04_top_quan_nhieu_tin.png")

  p5 <- df %>%
    filter(transaction_type == "Bán", !is.na(price_per_m2), price_per_m2 > 0) %>%
    filter_quantile_by_transaction("price_per_m2", low = 0.01, high = 0.99) %>%
    group_by(district_name) %>%
    summarise(
      listings = n(),
      median_price_per_m2 = median(price_per_m2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(listings >= 30) %>%
    slice_max(median_price_per_m2, n = 10) %>%
    ggplot(aes(x = reorder(district_name, median_price_per_m2), y = median_price_per_m2 / 1e6)) +
    geom_col(fill = "#C0504D") +
    coord_flip() +
    labs(
      title = "Top 10 quan/huyen co gia ban/m2 cao nhat",
      subtitle = "Tinh median theo tung tin ban, da loc 1% outlier gia/m2",
      x = NULL,
      y = "Trieu VND/m2"
    ) +
    theme_minimal(base_size = 12)
  save_plot(p5, "05_top_quan_gia_m2_cao.png")

  if (sum(!is.na(df$posted_at)) > 0) {
    p6 <- df %>%
      mutate(posted_month = floor_date(as.Date(posted_at), "month")) %>%
      filter(!is.na(posted_month)) %>%
      count(posted_month, transaction_type) %>%
      ggplot(aes(x = posted_month, y = n, color = transaction_type)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.6) +
      scale_y_log10(labels = label_count) +
      labs(
        title = "Do phu tin dang theo thang",
        subtitle = "Dung thang log10 vi du lieu tap trung manh o cac dot crawl/import gan day",
        x = "Thang",
        y = "So tin (log10)",
        color = "Giao dich"
      ) +
      theme_minimal(base_size = 12)
    save_plot(p6, "06_xu_huong_tin_theo_ngay.png")
  }

  if (all(c("lat", "lon") %in% names(df))) {
    p7 <- df %>%
      filter(
        !is.na(lat), !is.na(lon),
        lat >= 10.3, lat <= 11.2,
        lon >= 106.3, lon <= 107.2
      ) %>%
      ggplot(aes(x = lon, y = lat, color = transaction_type)) +
      geom_point(alpha = 0.5, size = 1.15) +
      facet_wrap(~ transaction_type) +
      coord_fixed() +
      guides(color = "none") +
      labs(
        title = "Phan bo dia ly tin dang TP.HCM",
        subtitle = "Da loc toa do nam ngoai khung TP.HCM",
        x = "Kinh do",
        y = "Vi do"
      ) +
      theme_minimal(base_size = 12)
    save_plot(p7, "07_phan_bo_dia_ly_gia.png")
  }

  p8 <- df %>%
    filter(transaction_type == "Bán", !is.na(price_per_m2), price_per_m2 > 0) %>%
    filter_quantile_by_transaction("price_per_m2", low = 0.01, high = 0.99) %>%
    add_count(category_group, name = "category_listings") %>%
    filter(category_listings >= 30) %>%
    ggplot(aes(x = reorder(category_group, price_per_m2, median), y = price_per_m2 / 1e6, fill = category_group)) +
    geom_boxplot(outlier.alpha = 0.2) +
    coord_flip() +
    guides(fill = "none") +
    labs(
      title = "So sanh gia ban/m2 theo loai BDS",
      subtitle = "Da gop nhom loai BDS va loc 1% outlier gia/m2",
      x = NULL,
      y = "Trieu VND/m2"
    ) +
    theme_minimal(base_size = 12)
  save_plot(p8, "08_gia_m2_theo_loai_bds.png")

  cat(sprintf("Da tao EDA trong thu muc %s.\n", PLOT_DIR))
  invisible(summary_tbl)
}

if (sys.nframe() == 0) {
  run_eda()
}
