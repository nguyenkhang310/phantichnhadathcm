#!/usr/bin/env Rscript

source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()

required_packages <- c("dplyr", "readr", "ggplot2", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Thieu package: ", paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(readr)
library(ggplot2)
library(tibble)

FEATURED_CSV <- PATHS$featured_csv
RAW_CSV <- PATHS$combined_raw_csv
PLOT_DIR <- PATHS$plot_dir

load_inference_data <- function() {
  if (file.exists(FEATURED_CSV)) {
    return(read_csv(FEATURED_CSV, show_col_types = FALSE))
  }
  if (file.exists(RAW_CSV)) {
    return(read_csv(RAW_CSV, show_col_types = FALSE))
  }
  stop("Chua co data. Hay chay scripts/02_xu_ly_du_lieu/gop_nguon_du_lieu.R va scripts/02_xu_ly_du_lieu/tao_dac_trung.R truoc.")
}

normalize_category <- function(category) {
  category <- as.character(category)
  case_when(
    grepl("căn hộ|chung cư", category, ignore.case = TRUE) ~ "Căn hộ / chung cư",
    grepl("phòng|nhà trọ", category, ignore.case = TRUE) ~ "Phòng / nhà trọ",
    grepl("văn phòng|mặt bằng|shop", category, ignore.case = TRUE) ~ "Văn phòng / mặt bằng",
    grepl("đất", category, ignore.case = TRUE) ~ "Đất",
    grepl("nhà|biệt thự", category, ignore.case = TRUE) ~ "Nhà / biệt thự",
    TRUE ~ "Khác"
  )
}

ci_log_mean <- function(x, conf = 0.95) {
  x <- log1p(x[is.finite(x) & x > 0])
  n <- length(x)
  if (n < 2) {
    return(tibble(n = n, mean_value = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  }
  alpha <- 1 - conf
  se <- stats::sd(x) / sqrt(n)
  center <- mean(x)
  margin <- stats::qt(1 - alpha / 2, df = n - 1) * se
  tibble(
    n = n,
    mean_value = expm1(center),
    ci_low = expm1(center - margin),
    ci_high = expm1(center + margin)
  )
}

bootstrap_median_ci <- function(x, reps = 300, conf = 0.95) {
  x <- x[is.finite(x) & x > 0]
  if (length(x) < 10) {
    return(tibble(median_value = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  }
  boot <- replicate(reps, median(sample(x, replace = TRUE), na.rm = TRUE))
  alpha <- 1 - conf
  tibble(
    median_value = median(x, na.rm = TRUE),
    ci_low = as.numeric(stats::quantile(boot, alpha / 2, na.rm = TRUE)),
    ci_high = as.numeric(stats::quantile(boot, 1 - alpha / 2, na.rm = TRUE))
  )
}

format_test_result <- function(test_name, hypothesis, test_object) {
  tibble(
    test = test_name,
    hypothesis = hypothesis,
    statistic = unname(test_object$statistic[[1]]),
    p_value = test_object$p.value,
    conclusion = if_else(
      test_object$p.value < 0.05,
      "Bác bỏ H0 ở mức ý nghĩa 5%",
      "Chưa đủ bằng chứng bác bỏ H0 ở mức ý nghĩa 5%"
    )
  )
}

run_statistical_inference <- function(bootstrap_reps = as.integer(Sys.getenv("BOOTSTRAP_REPS", "300"))) {
  dir.create(PLOT_DIR, showWarnings = FALSE)
  set.seed(42)

  df <- load_inference_data() %>%
    mutate(
      price = as.numeric(price),
      area = as.numeric(area),
      price_per_m2 = if_else(!is.na(price_per_m2), as.numeric(price_per_m2),
                             if_else(!is.na(area) & area > 0, price / area, NA_real_)),
      transaction_type = case_when(
        "transaction_type" %in% names(.) & transaction_type %in% c("Bán", "Cho thuê") ~ as.character(transaction_type),
        "is_rent" %in% names(.) & as.logical(is_rent) ~ "Cho thuê",
        TRUE ~ "Bán"
      ),
      district_name = as.character(district_name),
      category_group = normalize_category(category_name)
    ) %>%
    filter(!is.na(price), price > 0, !is.na(district_name), district_name != "")

  tx_ci <- df %>%
    group_by(transaction_type) %>%
    summarise(ci_log_mean(price), .groups = "drop") %>%
    mutate(metric = "gia_niem_yet", group = transaction_type, .before = 1)

  sale_m2 <- df %>%
    filter(transaction_type == "Bán", !is.na(price_per_m2), price_per_m2 > 0)

  district_ci <- sale_m2 %>%
    group_by(district_name) %>%
    filter(n() >= 30) %>%
    summarise(ci_log_mean(price_per_m2), .groups = "drop") %>%
    arrange(desc(n)) %>%
    mutate(metric = "gia_ban_m2", group = district_name, .before = 1)

  top_groups <- df %>%
    filter(!is.na(price_per_m2), price_per_m2 > 0) %>%
    count(transaction_type, district_name, name = "n") %>%
    group_by(transaction_type) %>%
    slice_max(n, n = 8, with_ties = FALSE) %>%
    ungroup()

  boot_rows <- lapply(seq_len(nrow(top_groups)), function(i) {
    row <- top_groups[i, ]
    values <- df %>%
      filter(transaction_type == row$transaction_type, district_name == row$district_name) %>%
      pull(price_per_m2)
    bootstrap_median_ci(values, reps = bootstrap_reps) %>%
      mutate(
        metric = "bootstrap_trung_vi_gia_m2",
        group = paste(row$transaction_type, row$district_name, sep = " - "),
        n = row$n,
        .before = 1
      )
  })
  bootstrap_tbl <- bind_rows(boot_rows)

  inference_tbl <- bind_rows(
    tx_ci,
    district_ci,
    bootstrap_tbl %>%
      transmute(metric, group, n, mean_value = median_value, ci_low, ci_high)
  )
  write_csv(inference_tbl, PATHS$inference_summary_csv)

  test_results <- list()

  top_sale_districts <- sale_m2 %>%
    count(district_name, name = "n") %>%
    filter(n >= 30) %>%
    slice_max(n, n = 2, with_ties = FALSE) %>%
    pull(district_name)

  if (length(top_sale_districts) == 2) {
    test_df <- sale_m2 %>% filter(district_name %in% top_sale_districts)
    t_res <- stats::t.test(log(price_per_m2) ~ district_name, data = test_df)
    test_results[[length(test_results) + 1L]] <- format_test_result(
      "Welch t-test log(gia_ban_m2)",
      paste("So sanh gia/m2 trung binh giua", paste(top_sale_districts, collapse = " va ")),
      t_res
    )
  }

  category_table <- table(df$transaction_type, df$category_group)
  if (all(dim(category_table) >= 2)) {
    chi_res <- suppressWarnings(stats::chisq.test(category_table))
    test_results[[length(test_results) + 1L]] <- format_test_result(
      "Chi-square",
      "Loai giao dich va nhom bat dong san doc lap nhau",
      chi_res
    )
  }

  tests_tbl <- bind_rows(test_results)
  write_csv(tests_tbl, PATHS$inference_tests_csv)

  if (nrow(district_ci) > 0) {
    p_ci <- district_ci %>%
      slice_max(n, n = 12, with_ties = FALSE) %>%
      ggplot(aes(x = reorder(group, mean_value), y = mean_value / 1e6)) +
      geom_pointrange(aes(ymin = ci_low / 1e6, ymax = ci_high / 1e6), color = "#2E75B6") +
      coord_flip() +
      labs(
        title = "Khoảng tin cậy 95% giá bán/m² theo quận/huyện",
        x = NULL,
        y = "Triệu VND/m²"
      ) +
      theme_minimal(base_size = 12)
    ggsave(file.path(PLOT_DIR, "09_ci_gia_ban_m2_quan.png"), p_ci, width = 9, height = 6, dpi = 160)
  }

  if (nrow(bootstrap_tbl) > 0) {
    p_boot <- bootstrap_tbl %>%
      mutate(
        type = if_else(grepl("^Bán", group), "Thị trường: Bán", "Thị trường: Cho thuê"),
        district_clean = gsub("Bán - |Cho thuê - ", "", group)
      ) %>%
      ggplot(aes(x = reorder(district_clean, median_value), y = median_value / 1e6)) +
      geom_pointrange(aes(ymin = ci_low / 1e6, ymax = ci_high / 1e6), color = "#C0504D") +
      coord_flip() +
      facet_wrap(~type, scales = "free", ncol = 1) + 
      labs(
        title = "Bootstrap CI trung vị giá/m² của các khu vực nhiều tin",
        x = NULL,
        y = "Triệu VND/m²"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        strip.text = element_text(face = "bold", size = 11),
        panel.spacing = unit(1.5, "lines") 
      )

    ggsave(file.path(PLOT_DIR, "10_bootstrap_ci_trung_vi_gia_m2.png"), p_boot, width = 10, height = 8, dpi = 160)
  }

  cat("Da tao suy luan thong ke:\n")
  cat("- ", PATHS$inference_summary_csv, "\n", sep = "")
  cat("- ", PATHS$inference_tests_csv, "\n", sep = "")
  invisible(list(summary = inference_tbl, tests = tests_tbl))
}

if (sys.nframe() == 0) {
  run_statistical_inference()
}
