# ============================================================
# EDA & VISUALIZATION - BDS TP.HCM
# Chay: Rscript eda_analysis.R
# Output: plots/*.png va plots/eda_summary.csv
# ============================================================

if (dir.exists("R_libs")) .libPaths(c(normalizePath("R_libs"), .libPaths()))

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

FEATURED_CSV <- "data/hcmc_bds_featured.csv"
RAW_CSV <- "data/hcmc_bds_raw.csv"
PLOT_DIR <- "plots"

load_eda_data <- function() {
  if (file.exists(FEATURED_CSV)) {
    return(read_csv(FEATURED_CSV, show_col_types = FALSE))
  }
  if (file.exists(RAW_CSV)) {
    return(read_csv(RAW_CSV, show_col_types = FALSE))
  }
  stop("Chua co data. Hay chay scraper va feature_engineering truoc.")
}

save_plot <- function(plot, filename, width = 10, height = 6) {
  ggsave(file.path(PLOT_DIR, filename), plot, width = width, height = height, dpi = 160)
}

run_eda <- function() {
  dir.create(PLOT_DIR, showWarnings = FALSE)

  df <- load_eda_data() %>%
    mutate(
      posted_at = suppressWarnings(as_datetime(posted_at)),
      price_per_m2 = if_else(!is.na(area) & area > 0, price / area, NA_real_)
    ) %>%
    filter(!is.na(price), price > 0)

  if (!"price_m" %in% names(df)) {
    df <- df %>% mutate(price_m = price / 1e6)
  }

  summary_tbl <- df %>%
    group_by(district_name, category_name) %>%
    summarise(
      listings = n(),
      median_price_m = median(price_m, na.rm = TRUE),
      median_area = median(area, na.rm = TRUE),
      median_price_per_m2 = median(price_per_m2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(listings))

  write_csv(summary_tbl, file.path(PLOT_DIR, "eda_summary.csv"))

  p1 <- ggplot(df, aes(x = log1p(price))) +
    geom_histogram(bins = 50, fill = "#2E75B6", color = "white") +
    labs(title = "Phan phoi log(gia)", x = "log(1 + price)", y = "So tin") +
    theme_minimal(base_size = 12)
  save_plot(p1, "01_log_price_distribution.png")

  p2 <- ggplot(df, aes(x = reorder(district_name, price_m, median), y = price_m)) +
    geom_boxplot(fill = "#D9EAF7", outlier_alpha = 0.25) +
    coord_flip() +
    labs(title = "Phan phoi gia theo quan", x = NULL, y = "Gia (trieu VND)") +
    theme_minimal(base_size = 12)
  save_plot(p2, "02_price_by_district_boxplot.png")

  p3 <- df %>%
    filter(!is.na(area), area > 0, area <= quantile(area, 0.99, na.rm = TRUE)) %>%
    ggplot(aes(x = area, y = price_m, color = category_name)) +
    geom_point(alpha = 0.45, size = 1.4) +
    labs(title = "Dien tich va gia theo loai BDS", x = "Dien tich (m2)", y = "Gia (trieu VND)", color = "Loai") +
    theme_minimal(base_size = 12)
  save_plot(p3, "03_area_vs_price.png")

  p4 <- summary_tbl %>%
    group_by(district_name) %>%
    summarise(listings = sum(listings), .groups = "drop") %>%
    slice_max(listings, n = 10) %>%
    ggplot(aes(x = reorder(district_name, listings), y = listings)) +
    geom_col(fill = "#1B5E20") +
    coord_flip() +
    labs(title = "Top 10 quan co nhieu tin nhat", x = NULL, y = "So tin") +
    theme_minimal(base_size = 12)
  save_plot(p4, "04_top_districts_by_listing.png")

  p5 <- summary_tbl %>%
    filter(!is.na(median_price_per_m2)) %>%
    group_by(district_name) %>%
    summarise(median_price_per_m2 = median(median_price_per_m2, na.rm = TRUE), .groups = "drop") %>%
    slice_max(median_price_per_m2, n = 10) %>%
    ggplot(aes(x = reorder(district_name, median_price_per_m2), y = median_price_per_m2 / 1e6)) +
    geom_col(fill = "#C0504D") +
    coord_flip() +
    labs(title = "Top 10 quan gia/m2 cao nhat", x = NULL, y = "Trieu VND/m2") +
    theme_minimal(base_size = 12)
  save_plot(p5, "05_top_districts_by_price_m2.png")

  if (sum(!is.na(df$posted_at)) > 0) {
    p6 <- df %>%
      mutate(posted_date = as.Date(posted_at)) %>%
      count(posted_date) %>%
      ggplot(aes(x = posted_date, y = n)) +
      geom_line(color = "#2E75B6", linewidth = 0.9) +
      labs(title = "Xu huong tin dang theo ngay", x = "Ngay", y = "So tin") +
      theme_minimal(base_size = 12)
    save_plot(p6, "06_listing_trend_by_day.png")
  }

  if (all(c("lat", "lon") %in% names(df))) {
    p7 <- df %>%
      filter(!is.na(lat), !is.na(lon)) %>%
      ggplot(aes(x = lon, y = lat, color = log1p(price))) +
      geom_point(alpha = 0.55, size = 1.2) +
      scale_color_viridis_c() +
      labs(title = "Phan bo dia ly tin dang", x = "Kinh do", y = "Vi do", color = "log(gia)") +
      theme_minimal(base_size = 12)
    save_plot(p7, "07_geo_scatter_price.png")
  }

  p8 <- df %>%
    filter(!is.na(price_per_m2), price_per_m2 > 0) %>%
    ggplot(aes(x = category_name, y = price_per_m2 / 1e6, fill = category_name)) +
    geom_boxplot(outlier_alpha = 0.2) +
    coord_flip() +
    guides(fill = "none") +
    labs(title = "So sanh gia/m2 theo loai BDS", x = NULL, y = "Trieu VND/m2") +
    theme_minimal(base_size = 12)
  save_plot(p8, "08_price_m2_by_category.png")

  cat(sprintf("Da tao EDA trong thu muc %s.\n", PLOT_DIR))
  invisible(summary_tbl)
}

if (identical(environment(), globalenv())) {
  run_eda()
}
