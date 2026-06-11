# ============================================================
# SHINY APP - BĐS TP.HCM
# Giao diện dashboard tùy biến, backend giữ pipeline R.
# Chạy: Rscript -e 'shiny::runApp(".", host="127.0.0.1", port=3838)'
# ============================================================

source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()

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

# Hàm in_hcmc_bbox: kiểm tra tọa độ có nằm trong TP.HCM.
in_hcmc_bbox <- function(lat, lon) {
  !is.na(lat) & !is.na(lon) &
    lat >= 10.30 & lat <= 11.20 &
    lon >= 106.00 & lon <= 107.30
}

hcmc_district_centers <- tibble::tribble(
  ~district_key, ~map_lat_est, ~map_lon_est,
  "1", 10.7757, 106.7004,
  "3", 10.7844, 106.6840,
  "4", 10.7578, 106.7059,
  "5", 10.7540, 106.6634,
  "6", 10.7460, 106.6357,
  "7", 10.7380, 106.7218,
  "8", 10.7241, 106.6286,
  "10", 10.7732, 106.6679,
  "11", 10.7629, 106.6501,
  "12", 10.8671, 106.6413,
  "binh tan", 10.7653, 106.6038,
  "binh thanh", 10.8106, 106.7093,
  "go vap", 10.8387, 106.6653,
  "phu nhuan", 10.7992, 106.6803,
  "tan binh", 10.8015, 106.6526,
  "tan phu", 10.7900, 106.6282,
  "thu duc", 10.8494, 106.7537,
  "binh chanh", 10.6874, 106.5939,
  "can gio", 10.4114, 106.9547,
  "cu chi", 10.9739, 106.4930,
  "hoc mon", 10.8897, 106.5955,
  "nha be", 10.6956, 106.7404,
  "ben cat", 11.1397, 106.6075,
  "vung tau", 10.4114, 107.1362,
  "binh duong", 11.0286, 106.6753
)

# Hàm strip_vietnamese: loại dấu tiếng Việt để so khớp văn bản.
strip_vietnamese <- function(x) {
  x <- tolower(as.character(x))
  replacements <- list(
    a = "[àáạảãâầấậẩẫăằắặẳẵ]",
    e = "[èéẹẻẽêềếệểễ]",
    i = "[ìíịỉĩ]",
    o = "[òóọỏõôồốộổỗơờớợởỡ]",
    u = "[ùúụủũưừứựửữ]",
    y = "[ỳýỵỷỹ]",
    d = "đ"
  )
  for (replacement in names(replacements)) {
    x <- gsub(replacements[[replacement]], replacement, x)
  }
  x
}

# Hàm normalize_district_key: chuẩn hóa tên quận huyện.
normalize_district_key <- function(x) {
  x <- strip_vietnamese(x)
  x <- gsub("^(quan|huyen|thanh pho)\\s+", "", x)
  x <- gsub("^q\\.?\\s*", "", x)
  x <- gsub("^tp\\.?\\s*", "", x)
  x <- gsub("\\s+", " ", trimws(x))
  x <- gsub("\\s+cu$", "", x)
  ifelse(x %in% c("", "khong ro", "unknown", "na"), NA_character_, x)
}

# Hàm is_missing_label: định dạng giá trị để hiển thị.
is_missing_label <- function(x) {
  key <- strip_vietnamese(trimws(as.character(x)))
  is.na(x) | key %in% c("", "unknown", "khong ro", "na", "nan", "null")
}

# Hàm clean_display_label: định dạng giá trị để hiển thị.
clean_display_label <- function(x, fallback = "Không rõ") {
  x <- trimws(as.character(x))
  ifelse(is_missing_label(x), fallback, x)
}

# Hàm known_rows_or_all: đếm hoặc kiểm tra điều kiện xử lý.
known_rows_or_all <- function(df, col) {
  values <- df[[col]]
  known <- df[!is_missing_label(values), , drop = FALSE]
  if (nrow(known) > 0) known else df
}

# Hàm choice_values: tạo dữ liệu phục vụ xử lý hoặc trực quan hóa.
choice_values <- function(x) {
  x <- unique(clean_display_label(x))
  known <- sort(x[!is_missing_label(x)])
  missing <- sort(x[is_missing_label(x)])
  c(known, missing)
}

# Hàm nice_slider_max: hỗ trợ xử lý dữ liệu trong script.
nice_slider_max <- function(x, step, fallback) {
  x <- suppressWarnings(max(as.numeric(x), na.rm = TRUE))
  if (!is.finite(x) || is.na(x) || x <= 0) return(fallback)
  max(fallback, ceiling(x / step) * step)
}

# Hàm listing_jitter: hỗ trợ xử lý dữ liệu trong script.
listing_jitter <- function(id, scale = 0.018) {
  id <- as.character(id)
  vapply(id, function(value) {
    if (is.na(value) || value == "") value <- "unknown"
    seed <- sum(utf8ToInt(value))
    angle <- (seed %% 360) * pi / 180
    radius <- (((seed %% 100) / 100) - 0.5) * scale
    radius * c(cos(angle), sin(angle))
  }, numeric(2))
}

# Hàm add_map_coordinates: tạo dữ liệu phục vụ xử lý hoặc trực quan hóa.
add_map_coordinates <- function(df) {
  if (nrow(df) == 0) {
    df$map_lat <- numeric()
    df$map_lon <- numeric()
    df$coord_status <- character()
    return(df)
  }

  if (!"source_id" %in% names(df)) df$source_id <- seq_len(nrow(df))
  exact_coord <- in_hcmc_bbox(df$lat, df$lon)
  jitter <- listing_jitter(df$source_id)

  df %>%
    mutate(district_key = normalize_district_key(district_name)) %>%
    left_join(hcmc_district_centers, by = "district_key") %>%
    mutate(
      coord_status = case_when(
        exact_coord ~ "Tọa độ gốc từ nguồn",
        !is.na(map_lat_est) & !is.na(map_lon_est) ~ "Ước lượng theo khu vực cũ",
        TRUE ~ "Ước lượng theo TP.HCM"
      ),
      map_lat_est = coalesce(map_lat_est, 10.7758),
      map_lon_est = coalesce(map_lon_est, 106.7009),
      map_lat = if_else(exact_coord, lat, map_lat_est + jitter[2, ]),
      map_lon = if_else(exact_coord, lon, map_lon_est + jitter[1, ]),
      map_lat = if_else(in_hcmc_bbox(map_lat, map_lon), map_lat, NA_real_),
      map_lon = if_else(in_hcmc_bbox(map_lat, map_lon), map_lon, NA_real_)
    ) %>%
    select(-district_key, -map_lat_est, -map_lon_est)
}

# Hàm load_data: nạp dữ liệu từ file, API hoặc cache.
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

  for (col in c("address", "ad_url", "title", "district_name", "category_name", "ward")) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }

  df %>%
    mutate(
      title = if_else(is.na(title) | title == "", "Tin bất động sản", as.character(title)),
      source = if ("source" %in% names(.)) clean_display_label(source) else "Không rõ",
      district_name = canonical_hcmc_district(district_name, address, title, ad_url),
      category_name = clean_display_label(category_name),
      transaction_type = if ("transaction_type" %in% names(.)) {
        if_else(is.na(transaction_type) | transaction_type == "", if_else(as.logical(is_rent), "Cho thuê", "Bán"), as.character(transaction_type))
      } else {
        if_else(as.logical(is_rent), "Cho thuê", "Bán")
      },
      ward = clean_display_label(ward),
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
    ) %>%
    filter(
      (is_rent & price >= 300000 & price <= 2e9) |
        (!is_rent & price >= 300000000 & price <= 500e9)
    ) %>%
    add_map_coordinates()
}

# Hàm load_metrics: nạp dữ liệu từ file, API hoặc cache.
load_metrics <- function() {
  if (!file.exists(METRICS_PATH)) return(tibble())
  read_csv(METRICS_PATH, show_col_types = FALSE)
}

# Hàm load_registry: nạp dữ liệu từ file, API hoặc cache.
load_registry <- function() {
  if (!file.exists(REGISTRY_PATH)) return(tibble())
  read_csv(REGISTRY_PATH, show_col_types = FALSE)
}

# Hàm format_vnd: định dạng giá trị để hiển thị.
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

# Hàm format_vnd_full: định dạng giá trị để hiển thị.
format_vnd_full <- function(x) paste0(format_vnd(x), " VND")

# Hàm listing_url: hỗ trợ xử lý dữ liệu trong script.
listing_url <- function(ad_url, source = NULL) {
  url <- trimws(as.character(ad_url))
  url[is.na(ad_url) | url == ""] <- NA_character_
  url <- ifelse(!is.na(url) & grepl("^//", url), paste0("https:", url), url)

  source <- if (is.null(source)) rep(NA_character_, length(url)) else tolower(as.character(source))
  source <- rep_len(source, length(url))

  is_relative <- !is.na(url) & grepl("^/", url) & !grepl("^//", url)
  url <- ifelse(is_relative & source == "muaban", paste0("https://muaban.net", url), url)
  url <- ifelse(is_relative & source == "alonhadat", paste0("https://alonhadat.com.vn", url), url)
  url <- ifelse(is_relative & source == "luachonnhadat", paste0("https://luachonnhadat.vn", url), url)
  url <- ifelse(is_relative & source == "homedy", paste0("https://homedy.com", url), url)

  is_bare_numeric <- !is.na(url) & grepl("^[0-9]+$", url) & source == "chotot"
  url <- ifelse(is_bare_numeric, paste0("https://www.chotot.com/", url), url)

  is_chotot_id_url <- !is.na(url) &
    grepl("chotot\\.com/[0-9]+/?$", url) &
    (is.na(source) | source == "chotot")

  url <- ifelse(is_chotot_id_url, paste0(sub("/$", "", url), ".htm"), url)
  ifelse(!is.na(url) & !grepl("^https?://", url), NA_character_, url)
}

# Hàm format_number_vi: định dạng giá trị để hiển thị.
format_number_vi <- function(x, digits = 1) {
  format(round(x, digits), big.mark = ".", decimal.mark = ",", nsmall = digits, trim = TRUE)
}

# Hàm format_count_vi: định dạng giá trị để hiển thị.
format_count_vi <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
}

# Hàm source_label_vi: định dạng giá trị để hiển thị.
source_label_vi <- function(x) {
  recode(
    as.character(x),
    chotot = "Chợ Tốt",
    alonhadat = "Alonhadat",
    luachonnhadat = "Lựa Chọn Nhà Đất",
    muaban = "Mua Bán",
    mogi = "Mogi",
    homedy = "Homedy",
    unknown = "Không rõ",
    .default = as.character(x)
  )
}

# Hàm price_display_info: định dạng giá trị để hiển thị.
price_display_info <- function(transaction_type) {
  if (identical(transaction_type, "Cho thuê")) {
    list(value_col = "price_m", axis = "Giá thuê (triệu VND)", unit = "triệu VND", digits = 1)
  } else {
    list(value_col = "price_b", axis = "Giá bán (tỷ VND)", unit = "tỷ VND", digits = 2)
  }
}

# Hàm price_m2_display_info: định dạng giá trị để hiển thị.
price_m2_display_info <- function(transaction_type) {
  if (identical(transaction_type, "Cho thuê")) {
    list(scale = 1e3, axis = "Nghìn VND/m²", unit = "nghìn VND/m²", digits = 1)
  } else {
    list(scale = 1e6, axis = "Triệu VND/m²", unit = "triệu VND/m²", digits = 1)
  }
}

# Hàm feature_label_vi: định dạng giá trị để hiển thị.
feature_label_vi <- function(feature) {
  dplyr::recode(
    as.character(feature),
    area = "Diện tích",
    rooms = "Số phòng",
    inferred_rooms = "Số phòng suy luận",
    frontage_width_m = "Ngang nhà/đất",
    frontage_length_m = "Dài nhà/đất",
    frontage_ratio = "Tỷ lệ ngang/dài",
    inferred_floors = "Số tầng/lầu suy luận",
    title_has_frontage = "Có mặt tiền/mặt phố",
    title_has_alley = "Có hẻm/ngõ",
    title_has_car_access = "Hẻm xe hơi/ô tô",
    title_has_corner = "Căn góc/2 mặt tiền",
    title_has_elevator = "Có thang máy",
    title_has_furnished = "Có nội thất",
    title_has_legal = "Pháp lý/sổ hồng",
    title_has_income_info = "Có thông tin dòng tiền",
    title_token_count = "Độ chi tiết tiêu đề",
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

# Hàm format_metric: định dạng giá trị để hiển thị.
format_metric <- function(x) {
  vapply(x, function(value) {
    if (is.na(value) || !is.finite(value)) return("NA")
    format(round(value, 3), nsmall = 3)
  }, character(1))
}

# Hàm active_or_all: hỗ trợ xử lý dữ liệu trong script.
active_or_all <- function(x, all_label = "Tất cả") {
  if (is.null(x) || length(x) == 0 || identical(x, "__all__")) all_label else paste(x, collapse = ", ")
}

# Hàm active_source_or_all: hỗ trợ xử lý dữ liệu trong script.
active_source_or_all <- function(x, all_label = "Tất cả") {
  if (is.null(x) || length(x) == 0 || identical(x, "__all__")) {
    all_label
  } else {
    paste(source_label_vi(x), collapse = ", ")
  }
}

# Hàm safe_range: đếm hoặc kiểm tra điều kiện xử lý.
safe_range <- function(x, default) {
  if (is.null(x) || length(x) < 2) default else x
}

# Hàm is_selected_filter: tạo thành phần giao diện.
is_selected_filter <- function(x) {
  !is.null(x) && length(x) > 0 && !identical(x, "__all__")
}

# Hàm %||%: hỗ trợ xử lý dữ liệu trong script.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Hàm plot_sample: tạo dữ liệu phục vụ xử lý hoặc trực quan hóa.
plot_sample <- function(df, max_n = 1600) {
  if (nrow(df) > max_n) dplyr::slice_sample(df, n = max_n) else df
}

# Hàm best_model_label: định dạng giá trị để hiển thị.
best_model_label <- function(metrics) {
  if (nrow(metrics) == 0 || !"mape" %in% names(metrics)) return("Chưa có")
  best <- metrics %>% filter(segment == "sale") %>% arrange(mape) %>% slice(1)
  if (nrow(best) == 0) best <- metrics %>% arrange(mape) %>% slice(1)
  paste0(best$model[[1]], " · MAPE ", round(best$mape[[1]] * 100, 1), "%")
}

# Hàm best_model_name_only: hỗ trợ xử lý dữ liệu trong script.
best_model_name_only <- function(metrics) {
  if (nrow(metrics) == 0 || !"mape" %in% names(metrics)) return("Chưa có")
  best <- metrics %>% filter(segment == "sale") %>% arrange(mape) %>% slice(1)
  if (nrow(best) == 0) best <- metrics %>% arrange(mape) %>% slice(1)
  best$model[[1]]
}

# Hàm best_model_mape_only: hỗ trợ xử lý dữ liệu trong script.
best_model_mape_only <- function(metrics) {
  if (nrow(metrics) == 0 || !"mape" %in% names(metrics)) return(NULL)
  best <- metrics %>% filter(segment == "sale") %>% arrange(mape) %>% slice(1)
  if (nrow(best) == 0) best <- metrics %>% arrange(mape) %>% slice(1)
  paste0("MAPE ", round(best$mape[[1]] * 100, 1), "%")
}

# Hàm best_model_from_bundle: hỗ trợ xử lý dữ liệu trong script.
best_model_from_bundle <- function(bundle) {
  if (!is.null(bundle$best_model) && !is.na(bundle$best_model)) {
    return(bundle$best_model)
  }
  "Random Forest"
}

# Hàm model_label_vi: định dạng giá trị để hiển thị.
model_label_vi <- function(model_name) {
  dplyr::case_when(
    identical(model_name, "Linear Regression") ~ "Linear Regression",
    identical(model_name, "Random Forest") ~ "Random Forest",
    identical(model_name, "XGBoost") ~ "XGBoost",
    identical(model_name, "RF + XGBoost Ensemble") ~ "RF + XGBoost Ensemble",
    identical(model_name, "Tuned RF/XGBoost Ensemble") ~ "Tuned RF/XGBoost Ensemble",
    TRUE ~ as.character(model_name)
  )
}

# Hàm add_prediction_encoding_keys: tạo dữ liệu phục vụ xử lý hoặc trực quan hóa.
add_prediction_encoding_keys <- function(input_row) {
  input_row %>%
    mutate(
      district_category_key = paste(as.character(district_name), as.character(category_name), sep = " | "),
      source_category_key = paste(as.character(source), as.character(category_name), sep = " | ")
    )
}

# Hàm apply_bundle_encoding: làm sạch và chuẩn hóa dữ liệu nguồn.
apply_bundle_encoding <- function(input_row, encoding, output_col) {
  if (is.null(encoding) || is.null(encoding$table) || is.null(encoding$group_col)) return(input_row)
  group_col <- encoding$group_col
  if (!group_col %in% names(input_row)) return(input_row)
  input_row[[group_col]] <- as.character(input_row[[group_col]])
  encoding$table[[group_col]] <- as.character(encoding$table[[group_col]])

  input_row %>%
    select(-any_of(output_col)) %>%
    left_join(
      encoding$table %>% select(all_of(group_col), encoded_value),
      by = group_col
    ) %>%
    mutate("{output_col}" := coalesce(encoded_value, encoding$global)) %>%
    select(-encoded_value)
}

# Hàm prepare_prediction_for_bundle: làm sạch và chuẩn hóa dữ liệu nguồn.
prepare_prediction_for_bundle <- function(input_row, bundle) {
  input_row <- add_prediction_encoding_keys(input_row)

  if (!is.null(bundle$target_encodings)) {
    encoding_cols <- c(
      ward = "ward_price_encoded",
      district = "district_price_encoded",
      category = "category_price_encoded",
      source = "source_price_encoded",
      district_category = "district_category_price_encoded",
      source_category = "source_category_price_encoded"
    )
    for (encoding_name in names(encoding_cols)) {
      input_row <- apply_bundle_encoding(input_row, bundle$target_encodings[[encoding_name]], encoding_cols[[encoding_name]])
    }
  } else if (!is.null(bundle$ward_encoding)) {
    input_row <- apply_bundle_encoding(input_row, bundle$ward_encoding, "ward_price_encoded")
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

# Hàm make_xgb_prediction_matrix: huấn luyện hoặc đánh giá mô hình.
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

# Hàm predict_log_with_model: dự đoán giá từ model đã huấn luyện.
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

# Hàm predict_price: dự đoán giá từ model đã huấn luyện.
predict_price <- function(input_row, is_rent) {
  model_path <- if (is_rent) RENT_MODEL_PATH else SALE_MODEL_PATH
  if (!file.exists(model_path)) return(NA_real_)

  bundle <- readRDS(model_path)
  input_row <- prepare_prediction_for_bundle(input_row, bundle)
  model_name <- best_model_from_bundle(bundle)

  pred_log <- tryCatch({
    if (model_name %in% c("RF + XGBoost Ensemble", "Tuned RF/XGBoost Ensemble")) {
      rf_pred <- predict_log_with_model(bundle, "Random Forest", input_row)
      xgb_pred <- predict_log_with_model(bundle, "XGBoost", input_row)
      weight_rf <- if (identical(model_name, "Tuned RF/XGBoost Ensemble") && !is.null(bundle$ensemble_weight_rf)) {
        as.numeric(bundle$ensemble_weight_rf)
      } else {
        0.5
      }
      weight_rf * rf_pred + (1 - weight_rf) * xgb_pred
    } else {
      predict_log_with_model(bundle, model_name, input_row)
    }
  }, error = function(e) NA_real_)

  if (!is.null(bundle$train_log_bounds) && length(bundle$train_log_bounds) == 2) {
    pred_log <- pmin(pmax(pred_log, bundle$train_log_bounds[[1]]), bundle$train_log_bounds[[2]])
  }

  expm1(as.numeric(pred_log))
}

# Hàm prediction_model_label: dự đoán giá từ model đã huấn luyện.
prediction_model_label <- function(is_rent) {
  model_path <- if (is_rent) RENT_MODEL_PATH else SALE_MODEL_PATH
  if (!file.exists(model_path)) return("Chưa có model")
  bundle <- readRDS(model_path)
  model_label_vi(best_model_from_bundle(bundle))
}

# Hàm build_prediction_row: dự đoán giá từ model đã huấn luyện.
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

  # Hàm mode_chr: hỗ trợ xử lý dữ liệu trong script.
  mode_chr <- function(x, default = "Không rõ") {
    x <- as.character(x[!is.na(x) & x != ""])
    if (length(x) == 0) default else names(sort(table(x), decreasing = TRUE))[1]
  }
  # Hàm median_num: hỗ trợ xử lý dữ liệu trong script.
  median_num <- function(x, default = 0) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[!is.na(x)]
    if (length(x) == 0) default else median(x)
  }
  # Hàm mode_num: hỗ trợ xử lý dữ liệu trong script.
  mode_num <- function(x, default = 0) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[!is.na(x)]
    if (length(x) == 0) default else as.numeric(names(sort(table(x), decreasing = TRUE))[1])
  }

  tibble(
    district_name = district,
    category_name = category,
    ward = if_else(is.na(ward) | ward == "", "Không rõ", ward),
    source = mode_chr(local_df$source, "chotot"),
    area = as.numeric(area),
    log_area = log1p(as.numeric(area)),
    rooms = as.numeric(rooms),
    inferred_rooms = as.numeric(rooms),
    frontage_width_m = median_num(local_df$frontage_width_m, 0),
    frontage_length_m = median_num(local_df$frontage_length_m, 0),
    frontage_ratio = median_num(local_df$frontage_ratio, 0),
    inferred_floors = median_num(local_df$inferred_floors, 0),
    title_has_frontage = mode_num(local_df$title_has_frontage, 0),
    title_has_alley = mode_num(local_df$title_has_alley, 0),
    title_has_car_access = mode_num(local_df$title_has_car_access, 0),
    title_has_corner = mode_num(local_df$title_has_corner, 0),
    title_has_elevator = mode_num(local_df$title_has_elevator, 0),
    title_has_furnished = mode_num(local_df$title_has_furnished, 0),
    title_has_legal = mode_num(local_df$title_has_legal, 0),
    title_has_income_info = mode_num(local_df$title_has_income_info, 0),
    title_token_count = median_num(local_df$title_token_count, 0),
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

# Hàm assistant_text_key: xử lý logic trợ lý tư vấn.
assistant_text_key <- function(x) {
  x <- strip_vietnamese(as.character(x))
  x <- gsub("[^a-z0-9 ]+", " ", x)
  gsub("\\s+", " ", trimws(x))
}

# Hàm assistant_contains_any: xử lý logic trợ lý tư vấn.
assistant_contains_any <- function(text, patterns) {
  any(vapply(patterns, function(pattern) grepl(pattern, text), logical(1)))
}

# Hàm assistant_match_districts: xử lý logic trợ lý tư vấn.
assistant_match_districts <- function(question, choices) {
  key <- assistant_text_key(question)
  choices <- sort(unique(as.character(choices)))
  choices <- choices[!is.na(choices) & choices != ""]
  matched <- c()

  for (choice in choices) {
    label_key <- assistant_text_key(choice)
    core_key <- assistant_text_key(normalize_district_key(choice))
    escaped_label <- gsub(" ", "\\\\s+", label_key)
    escaped_core <- gsub(" ", "\\\\s+", core_key)

    hit <- FALSE
    district_number <- sub("^Quận\\s+", "", choice)
    if (grepl("^[0-9]+$", district_number)) {
      if (nzchar(label_key)) hit <- hit || grepl(paste0("\\b", escaped_label, "\\b"), key)
      hit <- hit || grepl(paste0("\\b(q|quan)\\s*", district_number, "\\b"), key)
    } else {
      if (nzchar(label_key)) hit <- hit || grepl(paste0("\\b", escaped_label, "\\b"), key)
      if (nzchar(core_key)) hit <- hit || grepl(paste0("\\b", escaped_core, "\\b"), key)
    }

    if (choice == "Thành phố Thủ Đức") {
      hit <- hit || grepl("\\bthu\\s+duc\\b|\\bquan\\s+(2|9)\\b|\\bq\\s*(2|9)\\b", key)
    }

    if (hit) matched <- c(matched, choice)
  }

  unique(matched)
}

# Hàm assistant_match_categories: xử lý logic trợ lý tư vấn.
assistant_match_categories <- function(question, choices) {
  key <- assistant_text_key(question)
  choices <- sort(unique(as.character(choices)))
  choices <- choices[!is.na(choices) & choices != ""]

  aliases <- list(
    "Căn hộ/Chung cư" = c("can ho", "chung cu", "apartment"),
    "Nhà ở" = c("nha o", "nha rieng", "nha nguyen can", "nha"),
    "Nhà phố" = c("nha pho", "mat tien", "nha mat tien"),
    "Đất" = c("dat", "dat nen", "lo dat"),
    "Văn phòng/Mặt bằng" = c("van phong", "mat bang", "kinh doanh"),
    "Văn phòng, Mặt bằng kinh doanh" = c("van phong", "mat bang", "kinh doanh"),
    "Phòng trọ" = c("phong tro", "nha tro"),
    "Căn hộ" = c("can ho"),
    "Biệt thự" = c("biet thu"),
    "Kho xưởng" = c("kho xuong", "nha xuong")
  )

  matched <- c()
  for (choice in choices) {
    choice_key <- assistant_text_key(choice)
    if (nzchar(choice_key) && grepl(paste0("\\b", gsub(" ", "\\\\s+", choice_key), "\\b"), key)) {
      matched <- c(matched, choice)
      next
    }
    alias_patterns <- aliases[[choice]]
    if (!is.null(alias_patterns) && any(vapply(alias_patterns, function(alias) grepl(paste0("\\b", alias, "\\b"), key), logical(1)))) {
      matched <- c(matched, choice)
    }
  }

  unique(matched)
}

# Hàm assistant_detect_transaction: xử lý logic trợ lý tư vấn.
assistant_detect_transaction <- function(question, budget = NA_real_) {
  key <- assistant_text_key(question)
  if (grepl("\\b(thue|cho thue|rent|phong tro|nha tro)\\b", key)) return("Cho thuê")
  if (grepl("\\b(ban|mua|sale)\\b", key)) return("Bán")
  if (!is.na(budget) && budget > 0 && budget < 250000000) return("Cho thuê")
  NULL
}

# Hàm assistant_extract_number: xử lý logic trợ lý tư vấn.
assistant_extract_number <- function(text) {
  value <- gsub(",", ".", text)
  number <- regmatches(value, regexpr("[0-9]+([.][0-9]+)?", value))
  if (length(number) == 0 || !nzchar(number)) return(NA_real_)
  suppressWarnings(as.numeric(number))
}

# Hàm assistant_extract_budget: xử lý logic trợ lý tư vấn.
assistant_extract_budget <- function(question) {
  key <- assistant_text_key(question)
  ty_match <- regmatches(key, regexpr("[0-9]+([.,][0-9]+)?\\s*(ty|ti)", key))
  trieu_match <- regmatches(key, regexpr("[0-9]+([.,][0-9]+)?\\s*(trieu|tr)", key))
  if (length(ty_match) > 0 && nzchar(ty_match)) return(assistant_extract_number(ty_match) * 1e9)
  if (length(trieu_match) > 0 && nzchar(trieu_match)) return(assistant_extract_number(trieu_match) * 1e6)
  NA_real_
}

# Hàm assistant_extract_area: xử lý logic trợ lý tư vấn.
assistant_extract_area <- function(question) {
  key <- assistant_text_key(question)
  area_match <- regmatches(key, regexpr("[0-9]+([.,][0-9]+)?\\s*(m2|m)", key))
  if (length(area_match) == 0 || !nzchar(area_match)) return(NA_real_)
  assistant_extract_number(area_match)
}

# Hàm assistant_extract_rooms: xử lý logic trợ lý tư vấn.
assistant_extract_rooms <- function(question) {
  key <- assistant_text_key(question)
  room_match <- regmatches(key, regexpr("[0-9]+\\s*(pn|phong|ngu)", key))
  if (length(room_match) == 0 || !nzchar(room_match)) return(NA_real_)
  assistant_extract_number(room_match)
}

# Hàm assistant_extract_criteria: xử lý logic trợ lý tư vấn.
assistant_extract_criteria <- function(question, df) {
  budget <- assistant_extract_budget(question)
  list(
    raw = question,
    key = assistant_text_key(question),
    budget = budget,
    area = assistant_extract_area(question),
    rooms = assistant_extract_rooms(question),
    transaction = assistant_detect_transaction(question, budget),
    districts = assistant_match_districts(question, unique(df$district_name)),
    categories = assistant_match_categories(question, unique(df$category_name))
  )
}

# Hàm assistant_apply_criteria: xử lý logic trợ lý tư vấn.
assistant_apply_criteria <- function(df, criteria, limit_budget = TRUE) {
  if (!is.null(criteria$transaction)) {
    df <- df %>% filter(transaction_type == criteria$transaction)
  }
  if (length(criteria$districts) > 0) {
    df <- df %>% filter(district_name %in% criteria$districts)
  }
  if (length(criteria$categories) > 0) {
    df <- df %>% filter(category_name %in% criteria$categories)
  }
  if (limit_budget && !is.na(criteria$budget) && criteria$budget > 0) {
    df <- df %>% filter(price <= criteria$budget)
  }
  df
}

# Hàm assistant_criteria_text: xử lý logic trợ lý tư vấn.
assistant_criteria_text <- function(criteria) {
  parts <- c()
  if (!is.null(criteria$transaction)) parts <- c(parts, criteria$transaction)
  if (length(criteria$districts) > 0) parts <- c(parts, paste(criteria$districts, collapse = ", "))
  if (length(criteria$categories) > 0) parts <- c(parts, paste(criteria$categories, collapse = ", "))
  if (!is.na(criteria$budget)) parts <- c(parts, paste0("ngân sách ", format_vnd(criteria$budget)))
  if (!is.na(criteria$area)) parts <- c(parts, paste0(format_number_vi(criteria$area, 1), " m²"))
  if (length(parts) == 0) "toàn bộ dữ liệu" else paste(parts, collapse = " · ")
}

# Hàm assistant_confidence_label: xử lý logic trợ lý tư vấn.
assistant_confidence_label <- function(n) {
  if (n >= 80) "Cao"
  else if (n >= 20) "Vừa"
  else "Thấp"
}

# Hàm assistant_relative_text: xử lý logic trợ lý tư vấn.
assistant_relative_text <- function(value, baseline, label = "mặt bằng chung") {
  if (!is.finite(value) || !is.finite(baseline) || baseline <= 0) return("chưa đủ dữ liệu để so chuẩn")
  pct <- (value / baseline - 1) * 100
  if (abs(pct) < 6) return(paste0("gần ngang ", label))
  paste0(ifelse(pct > 0, "cao hơn ", "thấp hơn "), label, " khoảng ", round(abs(pct), 1), "%")
}

# Hàm assistant_insight_block: xử lý logic trợ lý tư vấn.
assistant_insight_block <- function(title, items) {
  items <- items[!is.na(items) & nzchar(items)]
  if (length(items) == 0) return("")
  paste0(
    "<div class='assistant-insight'>",
    "<div class='assistant-insight-title'>", htmltools::htmlEscape(title), "</div>",
    "<ul>", paste0("<li>", htmltools::htmlEscape(items), "</li>", collapse = ""), "</ul>",
    "</div>"
  )
}

# Hàm assistant_sort_recommendations: xử lý logic trợ lý tư vấn.
assistant_sort_recommendations <- function(df) {
  if (nrow(df) == 0) return(df)
  area_floor <- max(20, suppressWarnings(quantile(df$area, 0.25, na.rm = TRUE)))
  if (!is.finite(area_floor)) area_floor <- 20
  preferred <- df %>% filter(is.na(area) | area >= area_floor)
  if (nrow(preferred) == 0) preferred <- df
  preferred %>%
    mutate(
      value_rank = percent_rank(desc(coalesce(price_per_m2, median(price_per_m2, na.rm = TRUE)))),
      area_rank = percent_rank(coalesce(area, median(area, na.rm = TRUE))),
      quality_score = coalesce(value_rank, 0) + coalesce(area_rank, 0) * 0.35
    ) %>%
    arrange(desc(quality_score), price)
}

# Hàm assistant_listing_cards: xử lý logic trợ lý tư vấn.
assistant_listing_cards <- function(df, max_n = 5, compact = FALSE) {
  if (nrow(df) == 0) return("")
  cards <- df %>%
    mutate(source_link = listing_url(ad_url, source)) %>%
    slice_head(n = max_n) %>%
    mutate(
      price_m2_label = ifelse(is.na(price_per_m2), "Chưa có giá/m²", paste0(format_vnd_full(price_per_m2), "/m²")),
      reason = case_when(
        !is.na(area) & area >= median(area, na.rm = TRUE) & !is.na(price_per_m2) ~ "Diện tích ổn, giá/m² đáng so",
        !is.na(price_per_m2) ~ "Giá/m² thấp trong nhóm lọc",
        TRUE ~ "Khớp tiêu chí lọc"
      )
    ) %>%
    transmute(
      title = htmltools::htmlEscape(title),
      meta = htmltools::htmlEscape(paste0(district_name, " · ", category_name, " · ", source_label_vi(source))),
      price = htmltools::htmlEscape(format_vnd_full(price)),
      area = htmltools::htmlEscape(paste0(format_number_vi(area, 1), " m²")),
      price_m2 = htmltools::htmlEscape(price_m2_label),
      reason = htmltools::htmlEscape(reason),
      url = htmltools::htmlEscape(source_link)
    )

  paste0(
    "<div class='assistant-listings", if (compact) " compact" else "", "'>",
    paste0(
      "<div class='assistant-listing'>",
      "<div class='assistant-listing-reason'>", cards$reason, "</div>",
      "<div class='assistant-listing-title'>", cards$title, "</div>",
      "<div class='assistant-listing-meta'>", cards$meta, "</div>",
      "<div class='assistant-listing-stats'><span>", cards$price, "</span><span>", cards$area, "</span><span>", cards$price_m2, "</span></div>",
      ifelse(!is.na(cards$url) & cards$url != "", paste0("<a href='", cards$url, "' target='_blank' rel='noopener noreferrer'>Xem tin</a>"), ""),
      "</div>",
      collapse = ""
    ),
    "</div>"
  )
}

# Hàm assistant_stats_response: xử lý logic trợ lý tư vấn.
assistant_stats_response <- function(df, criteria) {
  scoped <- assistant_apply_criteria(df, criteria, limit_budget = FALSE)
  if (nrow(scoped) == 0) {
    return(paste0(
      "<p>Mình chưa tìm thấy dữ liệu khớp với <b>", htmltools::htmlEscape(assistant_criteria_text(criteria)), "</b>.</p>",
      "<p>Thử hỏi rộng hơn, ví dụ: <b>Giá nhà ở Bình Tân thế nào?</b> hoặc <b>So sánh Thủ Đức với Quận 7</b>.</p>"
    ))
  }

  top_district <- scoped %>% count(district_name, sort = TRUE) %>% slice(1)
  top_category <- scoped %>% count(category_name, sort = TRUE) %>% slice(1)
  tx <- criteria$transaction %||% names(sort(table(scoped$transaction_type), decreasing = TRUE))[1]
  reference <- df %>% filter(transaction_type == tx)
  if (length(criteria$categories) > 0) {
    category_reference <- reference %>% filter(category_name %in% criteria$categories)
    if (nrow(category_reference) >= 20) reference <- category_reference
  }
  median_price <- median(scoped$price, na.rm = TRUE)
  median_m2 <- median(scoped$price_per_m2, na.rm = TRUE)
  reference_price <- median(reference$price, na.rm = TRUE)
  reference_m2 <- median(reference$price_per_m2, na.rm = TRUE)
  q <- quantile(scoped$price, probs = c(0.25, 0.75), na.rm = TRUE)
  sample_label <- assistant_confidence_label(nrow(scoped))
  insight_items <- c(
    paste0("Giá trung vị đang ", assistant_relative_text(median_price, reference_price, paste0("nhóm ", tx))),
    paste0("Giá/m² ", assistant_relative_text(median_m2, reference_m2, "nhóm cùng giao dịch")),
    paste0("Khoảng giá phổ biến rơi vào ", format_vnd(q[[1]]), " - ", format_vnd(q[[2]]), "."),
    paste0("Độ tin cậy dữ liệu: ", sample_label, " vì có ", format_count_vi(nrow(scoped)), " tin.")
  )

  paste0(
    "<p class='assistant-lead'>", assistant_intro("stats", criteria), "</p>",
    "<div class='assistant-answer-grid'>",
    "<div><b>", format_count_vi(nrow(scoped)), "</b><span>tin phù hợp</span></div>",
    "<div><b>", htmltools::htmlEscape(format_vnd_full(median_price)), "</b><span>giá trung vị</span></div>",
    "<div><b>", htmltools::htmlEscape(ifelse(is.finite(median_m2), format_vnd_full(median_m2), "Chưa có")), "</b><span>giá/m² trung vị</span></div>",
    "</div>",
    assistant_insight_block("Nhận định nhanh", insight_items),
    "<p>Khu vực nổi bật trong nhóm này là <b>", htmltools::htmlEscape(top_district$district_name[[1]]), "</b> với ", format_count_vi(top_district$n[[1]]),
    " tin. Loại BĐS xuất hiện nhiều nhất là <b>", htmltools::htmlEscape(top_category$category_name[[1]]), "</b>.</p>",
    assistant_outro("stats")
  )
}

# Hàm assistant_recommend_response: xử lý logic trợ lý tư vấn.
assistant_recommend_response <- function(df, criteria) {
  scoped <- assistant_sort_recommendations(assistant_apply_criteria(df, criteria, limit_budget = TRUE))
  if (nrow(scoped) == 0) {
    scoped <- assistant_sort_recommendations(assistant_apply_criteria(df, criteria, limit_budget = FALSE))
    if (nrow(scoped) == 0) return(assistant_stats_response(df, criteria))
    return(paste0(
      "<p class='assistant-lead'>Mình chưa tìm thấy tin đăng nào khớp hoàn toàn với ngân sách bạn yêu cầu. Tuy nhiên, dưới đây là một số tin đăng có tiêu chí gần nhất mà bạn có thể tham khảo:</p>",
      assistant_listing_cards(scoped, 5),
      assistant_insight_block("Gợi ý xử lý", c(
        "Nới ngân sách hoặc đổi sang khu vực lân cận để có nhiều lựa chọn hơn.",
        "Ưu tiên kiểm tra pháp lý, diện tích thực và hẻm/đường trước khi so giá."
      ))
    ))
  }

  median_fit <- median(scoped$price, na.rm = TRUE)
  budget_note <- if (!is.na(criteria$budget)) {
    paste0("Giá trung vị của nhóm lọc thấp hơn ngân sách khoảng ", format_vnd(max(criteria$budget - median_fit, 0)), ".")
  } else {
    "Mình xếp tin theo diện tích và giá/m² để tránh chỉ chọn các tin rẻ nhưng quá nhỏ."
  }

  paste0(
    "<p class='assistant-lead'>", assistant_intro("recommend", criteria), "</p>",
    assistant_insight_block("Nên xem theo thứ tự này", c(
      budget_note,
      "Các card dưới đây có link gốc, nên bấm vào để kiểm hình ảnh, vị trí và mô tả chi tiết.",
      paste0("Độ tin cậy dữ liệu: ", assistant_confidence_label(nrow(scoped)), ".")
    )),
    assistant_listing_cards(scoped, 5),
    assistant_outro("recommend")
  )
}

# Hàm assistant_compare_response: xử lý logic trợ lý tư vấn.
assistant_compare_response <- function(df, criteria) {
  districts <- criteria$districts
  if (length(districts) < 2) {
    districts <- df %>%
      count(district_name, sort = TRUE) %>%
      slice_head(n = 5) %>%
      pull(district_name)
  }

  scoped <- df %>%
    filter(district_name %in% districts)
  if (!is.null(criteria$transaction)) scoped <- scoped %>% filter(transaction_type == criteria$transaction)
  if (length(criteria$categories) > 0) scoped <- scoped %>% filter(category_name %in% criteria$categories)

  summary_df <- scoped %>%
    group_by(district_name) %>%
    summarise(
      listings = n(),
      median_price = median(price, na.rm = TRUE),
      median_m2 = median(price_per_m2, na.rm = TRUE),
      median_area = median(area, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(median_m2))

  if (nrow(summary_df) == 0) return(assistant_stats_response(df, criteria))
  best_value <- summary_df %>% arrange(median_m2) %>% slice(1)
  strongest <- summary_df %>% arrange(desc(listings)) %>% slice(1)
  premium <- summary_df %>% arrange(desc(median_m2)) %>% slice(1)

  rows <- paste0(
    "<tr><td>", htmltools::htmlEscape(summary_df$district_name), "</td>",
    "<td>", format_count_vi(summary_df$listings), "</td>",
    "<td>", htmltools::htmlEscape(format_vnd_full(summary_df$median_price)), "</td>",
    "<td>", htmltools::htmlEscape(format_vnd_full(summary_df$median_m2)), "/m²</td>",
    "<td>", format_number_vi(summary_df$median_area, 1), " m²</td></tr>",
    collapse = ""
  )

  paste0(
    "<p class='assistant-lead'>", assistant_intro("compare", criteria), "</p>",
    assistant_insight_block("Kết luận nhanh", c(
      paste0(best_value$district_name[[1]], " đang mềm nhất theo giá/m²."),
      paste0(premium$district_name[[1]], " đang cao nhất theo giá/m²."),
      paste0(strongest$district_name[[1]], " có độ phủ dữ liệu tốt nhất với ", format_count_vi(strongest$listings[[1]]), " tin.")
    )),
    "<div class='assistant-table-wrap'><table class='assistant-table'><thead><tr><th>Khu vực</th><th>Số tin</th><th>Giá trung vị</th><th>Giá/m²</th><th>Diện tích</th></tr></thead><tbody>",
    rows,
    "</tbody></table></div>",
    assistant_outro("compare")
  )
}

# Hàm assistant_predict_response: xử lý logic trợ lý tư vấn.
assistant_predict_response <- function(df, criteria) {
  if (length(criteria$districts) == 0 || is.na(criteria$area)) {
    return("<p>Để dự đoán giá, bạn cho mình tối thiểu <b>khu vực cũ</b> và <b>diện tích</b>. Ví dụ: <b>Dự đoán căn hộ 70m2 ở Thủ Đức</b>.</p>")
  }

  transaction <- criteria$transaction %||% "Bán"
  district <- criteria$districts[[1]]
  segment_df <- df %>% filter(transaction_type == transaction, district_name == district)
  if (nrow(segment_df) == 0) segment_df <- df %>% filter(transaction_type == transaction)
  category <- if (length(criteria$categories) > 0) {
    category_counts <- segment_df %>%
      filter(category_name %in% criteria$categories) %>%
      count(category_name, sort = TRUE)
    if (nrow(category_counts) > 0) category_counts$category_name[[1]] else criteria$categories[[1]]
  } else {
    segment_df %>% count(category_name, sort = TRUE) %>% slice(1) %>% pull(category_name)
  }
  if (length(category) == 0 || is.na(category)) category <- df$category_name[[1]]

  input_row <- build_prediction_row(
    df = df,
    district = district,
    category = category,
    ward = "Không rõ",
    area = criteria$area,
    rooms = ifelse(is.na(criteria$rooms), 0, criteria$rooms),
    transaction_type = transaction
  )
  pred <- predict_price(input_row, identical(transaction, "Cho thuê"))

  if (is.na(pred)) {
    return("<p>Model hiện chưa dự đoán được case này. Bạn thử chọn khu vực/loại BĐS có nhiều dữ liệu hơn nhé.</p>")
  }

  local_df <- df %>%
    filter(transaction_type == transaction, district_name == district, category_name == category)
  if (nrow(local_df) == 0) local_df <- df %>% filter(transaction_type == transaction, district_name == district)
  comparable <- local_df %>%
    filter(!is.na(area), area >= criteria$area * 0.8, area <= criteria$area * 1.2) %>%
    assistant_sort_recommendations()
  if (nrow(comparable) == 0) comparable <- assistant_sort_recommendations(local_df)
  sample_label <- assistant_confidence_label(nrow(local_df))

  paste0(
    "<p class='assistant-lead'>", assistant_intro("predict", criteria), "</p>",
    "<div class='assistant-prediction'>", htmltools::htmlEscape(format_vnd_full(pred)), "</div>",
    assistant_insight_block("Cách hiểu kết quả", c(
      paste0("Mô hình dùng: ", prediction_model_label(identical(transaction, "Cho thuê")), "."),
      paste0("Độ tin cậy dữ liệu: ", sample_label, " với ", format_count_vi(nrow(local_df)), " tin cùng khu vực/loại BĐS."),
      "Con số này nên dùng để so sánh ban đầu, chưa thay thế thẩm định thực tế."
    )),
    if (nrow(comparable) > 0) paste0("<p>Một vài tin gần diện tích này để đối chiếu:</p>", assistant_listing_cards(comparable, 3, compact = TRUE)) else "",
    assistant_outro("predict")
  )
}

# Hàm assistant_help_response: xử lý logic trợ lý tư vấn.
assistant_help_response <- function() {
  paste0(
    "<p class='assistant-lead'>Mình là trợ lý BĐS local bằng R. Mình không có API bên ngoài, nhưng có thể đọc data, gọi model và trả lời theo số liệu thật.</p>",
    assistant_insight_block("Bạn có thể hỏi", c(
      "Giá nhà ở Bình Tân thế nào?",
      "So sánh Thủ Đức với Quận 7",
      "Ngân sách 4 tỷ mua nhà ở Bình Tân có tin nào?",
      "Dự đoán căn hộ 70m2 ở Thủ Đức"
    ))
  )
}

# Hàm assistant_answer: xử lý logic trợ lý tư vấn.
assistant_answer <- function(question, df) {
  criteria <- assistant_extract_criteria(question, df)
  key <- criteria$key

  if (!nzchar(key) || assistant_contains_any(key, c("\\bhelp\\b", "huong dan", "hoi duoc gi", "ban lam duoc gi", "tro giup"))) {
    return(assistant_help_response())
  }

  if (assistant_contains_any(key, c("du doan", "uoc tinh", "gia khoang bao nhieu", "bao nhieu tien"))) {
    return(assistant_predict_response(df, criteria))
  }

  if (assistant_contains_any(key, c("so sanh", "khac nhau", "re hon", "dat hon", "cao hon", "thap hon"))) {
    return(assistant_compare_response(df, criteria))
  }

  if (assistant_contains_any(key, c("goi y", "tim", "co tin", "co gi", "nen mua", "nen thue", "ngan sach", "duoi", "listing", "xem tin"))) {
    return(assistant_recommend_response(df, criteria))
  }

  assistant_stats_response(df, criteria)
}

# ============================================================
# ASSISTANT V2 - local tool-based assistant with memory support
# ============================================================

# Hàm assistant_escape: xử lý logic trợ lý tư vấn.
assistant_escape <- function(x) {
  htmltools::htmlEscape(as.character(x))
}

# Hàm assistant_has_value: xử lý logic trợ lý tư vấn.
assistant_has_value <- function(x) {
  !is.null(x) && length(x) > 0 && !all(is.na(x)) && any(nzchar(trimws(as.character(x))))
}

# Hàm assistant_is_finite: xử lý logic trợ lý tư vấn.
assistant_is_finite <- function(x) {
  !is.null(x) && length(x) > 0 && !is.na(x[[1]]) && is.finite(as.numeric(x[[1]]))
}

# Hàm assistant_number_label: xử lý logic trợ lý tư vấn.
assistant_number_label <- function(x, digits = 1, suffix = "") {
  if (!assistant_is_finite(x)) return("Chưa rõ")
  paste0(format_number_vi(as.numeric(x[[1]]), digits), suffix)
}

# Hàm assistant_format_area: xử lý logic trợ lý tư vấn.
assistant_format_area <- function(x) {
  vapply(x, function(value) {
    if (is.na(value) || !is.finite(value) || value <= 0) return("Chưa rõ diện tích")
    paste0(format_number_vi(value, 1), " m²")
  }, character(1))
}

# Hàm assistant_squish_number_text: xử lý logic trợ lý tư vấn.
assistant_squish_number_text <- function(text) {
  text <- strip_vietnamese(text)
  text <- gsub(",", ".", text, fixed = TRUE)
  gsub("([0-9]+)\\s+([0-9]+)\\s*(ty|ti|trieu|tr|m2|m vuong|met vuong)", "\\1.\\2 \\3", text, perl = TRUE)
}

# Hàm assistant_extract_money_values: xử lý logic trợ lý tư vấn.
assistant_extract_money_values <- function(question) {
  text <- assistant_squish_number_text(question)
  matches <- regmatches(
    text,
    gregexpr("[0-9]+(?:[.][0-9]+)?\\s*(ty|ti|trieu|tr|trd|nghin|k)\\b", text, perl = TRUE)
  )[[1]]
  if (length(matches) == 0 || identical(matches, character(0)) || matches[[1]] == "-1") return(numeric())

  vapply(matches, function(token) {
    value <- suppressWarnings(as.numeric(regmatches(token, regexpr("[0-9]+(?:[.][0-9]+)?", token, perl = TRUE))))
    unit <- trimws(sub("^[0-9.]+\\s*", "", token))
    scale <- if (grepl("^(ty|ti)$", unit)) {
      1e9
    } else if (grepl("^(nghin|k)$", unit)) {
      1e3
    } else {
      1e6
    }
    value * scale
  }, numeric(1))
}

# Hàm assistant_extract_budget_info: xử lý logic trợ lý tư vấn.
assistant_extract_budget_info <- function(question) {
  key <- assistant_text_key(question)
  values <- assistant_extract_money_values(question)
  budget_min <- NA_real_
  budget_max <- NA_real_

  if (length(values) >= 2 && assistant_contains_any(key, c("\\btu\\b", "\\bden\\b", "\\btoi\\b", "\\bkhoang\\b"))) {
    budget_min <- min(values[1:2], na.rm = TRUE)
    budget_max <- max(values[1:2], na.rm = TRUE)
  } else if (length(values) >= 1) {
    value <- values[[1]]
    if (assistant_contains_any(key, c("\\btren\\b", "lon hon", "cao hon", "tu [0-9]", "toi thieu", "tro len"))) {
      budget_min <- value
    } else {
      budget_max <- value
    }
  }

  list(
    budget = ifelse(is.na(budget_max), NA_real_, budget_max),
    budget_min = budget_min,
    budget_max = budget_max
  )
}

# Hàm assistant_extract_area_info: xử lý logic trợ lý tư vấn.
assistant_extract_area_info <- function(question) {
  key <- assistant_text_key(question)
  text <- assistant_squish_number_text(question)
  area <- NA_real_
  area_min <- NA_real_
  area_max <- NA_real_

  range_match <- regexec(
    "([0-9]+(?:[.][0-9]+)?)\\s*(?:-|den|toi)\\s*([0-9]+(?:[.][0-9]+)?)\\s*(m2|m vuong|met vuong|m\\b)",
    text,
    perl = TRUE
  )
  range_parts <- regmatches(text, range_match)[[1]]
  if (length(range_parts) >= 3) {
    values <- suppressWarnings(as.numeric(range_parts[2:3]))
    area_min <- min(values, na.rm = TRUE)
    area_max <- max(values, na.rm = TRUE)
    area <- mean(values, na.rm = TRUE)
  } else {
    matches <- regmatches(
      text,
      gregexpr("[0-9]+(?:[.][0-9]+)?\\s*(m2|m vuong|met vuong|m\\b)", text, perl = TRUE)
    )[[1]]
    if (length(matches) > 0 && !identical(matches, character(0)) && matches[[1]] != "-1") {
      value <- suppressWarnings(as.numeric(regmatches(matches[[1]], regexpr("[0-9]+(?:[.][0-9]+)?", matches[[1]], perl = TRUE))))
      area <- value
      if (assistant_contains_any(key, c("\\btren\\b", "lon hon", "tu [0-9]", "toi thieu", "tro len"))) {
        area_min <- value
      } else if (assistant_contains_any(key, c("\\bduoi\\b", "nho hon", "toi da", "tro xuong"))) {
        area_max <- value
      } else {
        area_min <- max(0, value * 0.85)
        area_max <- value * 1.2
      }
    }
  }

  list(area = area, area_min = area_min, area_max = area_max)
}

# Hàm assistant_extract_budget: xử lý logic trợ lý tư vấn.
assistant_extract_budget <- function(question) {
  assistant_extract_budget_info(question)$budget
}

# Hàm assistant_extract_area: xử lý logic trợ lý tư vấn.
assistant_extract_area <- function(question) {
  assistant_extract_area_info(question)$area
}

# Hàm assistant_extract_rooms: xử lý logic trợ lý tư vấn.
assistant_extract_rooms <- function(question) {
  key <- assistant_text_key(question)
  room_match <- regmatches(key, regexpr("[0-9]+\\s*(pn|p n|phong ngu|phong|ngu)", key, perl = TRUE))
  if (length(room_match) == 0 || !nzchar(room_match)) return(NA_real_)
  assistant_extract_number(room_match)
}

# Hàm assistant_detect_transaction: xử lý logic trợ lý tư vấn.
assistant_detect_transaction <- function(question, budget = NA_real_) {
  key <- assistant_text_key(question)
  if (assistant_contains_any(key, c("\\bcho thue\\b", "\\bthue\\b", "\\brent\\b", "moi thang", "hang thang", "phong tro", "nha tro", "can ho dich vu"))) {
    return("Cho thuê")
  }
  if (assistant_contains_any(key, c("\\bmua\\b", "\\bban\\b", "\\bsale\\b", "so huu", "xuong tien", "dat coc"))) {
    return("Bán")
  }
  if (!is.na(budget) && budget > 0 && budget < 250000000) return("Cho thuê")
  if (!is.na(budget) && budget >= 250000000) return("Bán")
  NULL
}

# Hàm assistant_match_categories: xử lý logic trợ lý tư vấn.
assistant_match_categories <- function(question, choices) {
  key <- assistant_text_key(question)
  choices <- sort(unique(as.character(choices)))
  choices <- choices[!is.na(choices) & choices != ""]

  alias_catalog <- list(
    "can ho" = c("can ho", "chung cu", "apartment", "studio", "chdv", "can ho dich vu"),
    "can ho chung cu" = c("can ho", "chung cu", "apartment"),
    "can ho dich vu" = c("can ho dich vu", "chdv", "studio"),
    "nha o" = c("nha o", "nha rieng", "nha nguyen can", "nha hem", "nha"),
    "nha pho" = c("nha pho", "mat tien", "nha mat tien", "shophouse", "shop house"),
    "shop house" = c("shophouse", "shop house", "nha pho thuong mai"),
    "dat" = c("dat", "dat nen", "lo dat", "nen dat"),
    "van phong mat bang" = c("van phong", "mat bang", "office", "kinh doanh", "mb"),
    "mat bang" = c("mat bang", "mb", "kinh doanh"),
    "phong tro" = c("phong tro", "nha tro", "phong cho thue", "o tro"),
    "phong cho thue" = c("phong tro", "phong cho thue", "nha tro"),
    "biet thu" = c("biet thu", "villa"),
    "kho xuong" = c("kho xuong", "nha xuong", "xuong", "kho"),
    "khach san" = c("khach san", "hotel")
  )

  matched <- c()
  for (choice in choices) {
    choice_key <- assistant_text_key(choice)
    compact_choice_key <- gsub("[^a-z0-9]+", " ", choice_key)
    direct_hit <- nzchar(compact_choice_key) && grepl(paste0("\\b", gsub(" ", "\\\\s+", compact_choice_key), "\\b"), key)

    alias_hits <- c()
    for (alias_name in names(alias_catalog)) {
      if (grepl(alias_name, compact_choice_key, fixed = TRUE) || grepl(compact_choice_key, alias_name, fixed = TRUE)) {
        alias_hits <- c(alias_hits, alias_catalog[[alias_name]])
      }
    }
    alias_hit <- length(alias_hits) > 0 && any(vapply(alias_hits, function(alias) {
      grepl(paste0("\\b", gsub(" ", "\\\\s+", assistant_text_key(alias)), "\\b"), key)
    }, logical(1)))

    if (direct_hit || alias_hit) matched <- c(matched, choice)
  }

  unique(matched)
}

# Hàm assistant_extract_preference: xử lý logic trợ lý tư vấn.
assistant_extract_preference <- function(question) {
  key <- assistant_text_key(question)
  if (assistant_contains_any(key, c("re hon", "mem hon", "gia tot", "hoi", "duoi gia", "thap hon mat bang", "deal"))) return("value")
  if (assistant_contains_any(key, c("rong hon", "dien tich lon", "to hon", "nhieu m2"))) return("larger")
  if (assistant_contains_any(key, c("moi hon", "tin moi", "dang gan day"))) return("recent")
  if (assistant_contains_any(key, c("gan trung tam", "trung tam"))) return("central")
  NULL
}

# Hàm assistant_extract_criteria: xử lý logic trợ lý tư vấn.
assistant_extract_criteria <- function(question, df) {
  budget_info <- assistant_extract_budget_info(question)
  area_info <- assistant_extract_area_info(question)
  list(
    raw = question,
    key = assistant_text_key(question),
    budget = budget_info$budget,
    budget_min = budget_info$budget_min,
    budget_max = budget_info$budget_max,
    area = area_info$area,
    area_min = area_info$area_min,
    area_max = area_info$area_max,
    rooms = assistant_extract_rooms(question),
    transaction = assistant_detect_transaction(question, budget_info$budget),
    districts = assistant_match_districts(question, unique(df$district_name)),
    categories = assistant_match_categories(question, unique(df$category_name)),
    preference = assistant_extract_preference(question),
    wants_location = assistant_contains_any(assistant_text_key(question), c("khu nao", "quan nao", "o dau", "khu vuc nao", "nen mua dau", "nen thue dau"))
  )
}

# Hàm assistant_empty_context: xử lý logic trợ lý tư vấn.
assistant_empty_context <- function() {
  list(criteria = NULL, intent = NULL, pending = FALSE, last_answer_at = NULL)
}

# Hàm assistant_has_context: xử lý logic trợ lý tư vấn.
assistant_has_context <- function(context) {
  is.list(context) && !is.null(context$criteria)
}

# Hàm assistant_is_followup: xử lý logic trợ lý tư vấn.
assistant_is_followup <- function(criteria, context) {
  if (!assistant_has_context(context)) return(FALSE)
  key <- criteria$key
  assistant_contains_any(key, c(
    "\\bcon\\b", "thi sao", "so voi", "doi sang", "chuyen sang", "loc tiep", "chi lay",
    "\\bchi\\b", "them", "nua", "re hon", "dat hon", "rong hon", "gan day", "tin nay",
    "khu do", "can do", "nhom nay", "tiep"
  )) ||
    isTRUE(context$pending) ||
    (
      length(criteria$districts) == 0 &&
        length(criteria$categories) == 0 &&
        is.null(criteria$transaction) &&
        !criteria$wants_location &&
        (assistant_is_finite(criteria$budget_max) || assistant_is_finite(criteria$area) || assistant_is_finite(criteria$rooms))
    )
}

# Hàm assistant_merge_criteria: xử lý logic trợ lý tư vấn.
assistant_merge_criteria <- function(criteria, context) {
  if (!assistant_is_followup(criteria, context)) return(criteria)
  previous <- context$criteria
  if (is.null(previous)) return(criteria)

  merged <- criteria
  if (length(merged$districts) == 0 && !isTRUE(merged$wants_location)) {
    merged$districts <- previous$districts
  } else if (
    length(merged$districts) == 1 &&
      length(previous$districts) > 0 &&
      assistant_contains_any(merged$key, c("so voi", "\\bvoi\\b", "\\bcon\\b", "thi sao"))
  ) {
    merged$districts <- unique(c(previous$districts, merged$districts))
  }
  if (length(merged$categories) == 0) merged$categories <- previous$categories
  if (is.null(merged$transaction)) merged$transaction <- previous$transaction
  if (!assistant_is_finite(merged$budget_max) && assistant_is_finite(previous$budget_max)) merged$budget_max <- previous$budget_max
  if (!assistant_is_finite(merged$budget_min) && assistant_is_finite(previous$budget_min)) merged$budget_min <- previous$budget_min
  if (!assistant_is_finite(merged$budget) && assistant_is_finite(merged$budget_max)) merged$budget <- merged$budget_max
  if (!assistant_is_finite(merged$area) && assistant_is_finite(previous$area)) merged$area <- previous$area
  if (!assistant_is_finite(merged$area_min) && assistant_is_finite(previous$area_min)) merged$area_min <- previous$area_min
  if (!assistant_is_finite(merged$area_max) && assistant_is_finite(previous$area_max)) merged$area_max <- previous$area_max
  if (!assistant_is_finite(merged$rooms) && assistant_is_finite(previous$rooms)) merged$rooms <- previous$rooms
  if (is.null(merged$preference)) merged$preference <- previous$preference

  if (assistant_contains_any(merged$key, c("re hon", "mem hon")) && assistant_is_finite(previous$budget_max) && !assistant_is_finite(criteria$budget_max)) {
    merged$budget_max <- previous$budget_max * 0.9
    merged$budget <- merged$budget_max
  }
  if (assistant_contains_any(merged$key, c("noi ngan sach", "rong ngan sach", "cao hon chut")) && assistant_is_finite(previous$budget_max) && !assistant_is_finite(criteria$budget_max)) {
    merged$budget_max <- previous$budget_max * 1.15
    merged$budget <- merged$budget_max
  }

  merged
}

# Hàm assistant_detect_intent: xử lý logic trợ lý tư vấn.
assistant_detect_intent <- function(question, criteria, context = assistant_empty_context()) {
  key <- criteria$key
  if (!nzchar(key) || assistant_contains_any(key, c("\\bhelp\\b", "huong dan", "hoi duoc gi", "ban lam duoc gi", "tro giup", "xin chao", "hello"))) {
    return("help")
  }
  if (assistant_contains_any(key, c("yeu to", "vi sao", "giai thich", "anh huong", "model dua vao", "tai sao"))) {
    return("explain")
  }
  if (assistant_contains_any(key, c("du doan", "uoc tinh", "dinh gia", "gia khoang bao nhieu", "bao nhieu tien", "model"))) {
    return("predict")
  }
  if (assistant_contains_any(key, c("so sanh", "khac nhau", "\\bvs\\b", "voi quan", "voi thu duc")) || length(criteria$districts) >= 2) {
    return("compare")
  }
  if (assistant_contains_any(key, c("hoi", "duoi gia", "re hon mat bang", "thap hon mat bang", "deal", "gia tot", "mem hon mat bang"))) {
    return("undervalued")
  }
  if (isTRUE(criteria$wants_location)) {
    return("scout")
  }
  if (assistant_contains_any(key, c("goi y", "tim", "cac tin", "nhung tin", "danh sach", "tin dang", "\\btin\\b", "co tin", "co gi", "nen mua", "nen thue", "ngan sach", "duoi", "listing", "xem tin", "phu hop", "loc"))) {
    return("recommend")
  }
  if (assistant_is_followup(criteria, context) && assistant_has_value(context$intent)) {
    return(context$intent)
  }
  "stats"
}

# Hàm assistant_budget_text: xử lý logic trợ lý tư vấn.
assistant_budget_text <- function(criteria) {
  has_min <- assistant_is_finite(criteria$budget_min)
  has_max <- assistant_is_finite(criteria$budget_max)
  if (has_min && has_max) return(paste0("ngân sách ", format_vnd(criteria$budget_min), " - ", format_vnd(criteria$budget_max)))
  if (has_max) return(paste0("ngân sách dưới ", format_vnd(criteria$budget_max)))
  if (has_min) return(paste0("ngân sách trên ", format_vnd(criteria$budget_min)))
  NULL
}

# Hàm assistant_area_text: xử lý logic trợ lý tư vấn.
assistant_area_text <- function(criteria) {
  has_min <- assistant_is_finite(criteria$area_min)
  has_max <- assistant_is_finite(criteria$area_max)
  if (has_min && has_max && assistant_is_finite(criteria$area)) {
    return(paste0("diện tích quanh ", format_number_vi(criteria$area, 1), " m²"))
  }
  if (has_min && has_max) return(paste0("diện tích ", format_number_vi(criteria$area_min, 1), " - ", format_number_vi(criteria$area_max, 1), " m²"))
  if (has_min) return(paste0("diện tích từ ", format_number_vi(criteria$area_min, 1), " m²"))
  if (has_max) return(paste0("diện tích dưới ", format_number_vi(criteria$area_max, 1), " m²"))
  NULL
}

# Hàm assistant_criteria_text: xử lý logic trợ lý tư vấn.
assistant_criteria_text <- function(criteria) {
  parts <- c()
  if (!is.null(criteria$transaction)) parts <- c(parts, criteria$transaction)
  if (length(criteria$districts) > 0) parts <- c(parts, paste(criteria$districts, collapse = ", "))
  if (length(criteria$categories) > 0) parts <- c(parts, paste(criteria$categories, collapse = ", "))
  parts <- c(parts, assistant_budget_text(criteria), assistant_area_text(criteria))
  if (assistant_is_finite(criteria$rooms)) parts <- c(parts, paste0("từ ", as.integer(criteria$rooms), " phòng"))
  if (length(parts) == 0) "toàn bộ dữ liệu" else paste(parts, collapse = " · ")
}

# Hàm assistant_apply_criteria: xử lý logic trợ lý tư vấn.
assistant_apply_criteria <- function(df, criteria, limit_budget = TRUE, limit_area = TRUE, limit_rooms = TRUE) {
  if (!is.null(criteria$transaction)) {
    df <- df %>% filter(transaction_type == criteria$transaction)
  }
  if (length(criteria$districts) > 0) {
    df <- df %>% filter(district_name %in% criteria$districts)
  }
  if (length(criteria$categories) > 0) {
    df <- df %>% filter(category_name %in% criteria$categories)
  }
  if (limit_budget && assistant_is_finite(criteria$budget_min)) {
    df <- df %>% filter(price >= criteria$budget_min)
  }
  if (limit_budget && assistant_is_finite(criteria$budget_max)) {
    df <- df %>% filter(price <= criteria$budget_max)
  }
  if (limit_area && assistant_is_finite(criteria$area_min)) {
    df <- df %>% filter(is.na(area) | area >= criteria$area_min)
  }
  if (limit_area && assistant_is_finite(criteria$area_max)) {
    df <- df %>% filter(is.na(area) | area <= criteria$area_max)
  }
  if (limit_rooms && assistant_is_finite(criteria$rooms)) {
    df <- df %>% filter(is.na(rooms) | rooms >= criteria$rooms)
  }
  df
}

# Hàm assistant_sort_recommendations: xử lý logic trợ lý tư vấn.
assistant_sort_recommendations <- function(df, criteria = NULL) {
  if (nrow(df) == 0) return(df)
  median_m2 <- suppressWarnings(median(df$price_per_m2, na.rm = TRUE))
  median_area <- suppressWarnings(median(df$area, na.rm = TRUE))
  target_area <- if (!is.null(criteria) && assistant_is_finite(criteria$area)) as.numeric(criteria$area) else NA_real_
  budget_max <- if (!is.null(criteria) && assistant_is_finite(criteria$budget_max)) as.numeric(criteria$budget_max) else NA_real_
  preference <- if (!is.null(criteria)) criteria$preference else NULL

  df %>%
    mutate(
      value_score = if_else(
        is.finite(price_per_m2) & is.finite(median_m2) & median_m2 > 0,
        pmax(pmin((median_m2 - price_per_m2) / median_m2, 0.55), -0.55),
        0
      ),
      area_score = case_when(
        is.finite(target_area) & target_area > 0 & is.finite(area) ~ pmax(0, 1 - abs(area - target_area) / target_area),
        is.finite(median_area) & median_area > 0 & is.finite(area) ~ pmin(area / median_area, 1.5) / 1.5,
        TRUE ~ 0
      ),
      budget_score = case_when(
        is.finite(budget_max) & budget_max > 0 & is.finite(price) & price <= budget_max ~ pmax(0, 1 - price / budget_max),
        is.finite(budget_max) & budget_max > 0 & is.finite(price) ~ -0.5,
        TRUE ~ 0
      ),
      room_score = if_else(is.finite(rooms), pmin(rooms / 4, 1), 0),
      recent_score = if ("listing_age_days" %in% names(.)) {
        age <- coalesce(listing_age_days, median(listing_age_days, na.rm = TRUE))
        if_else(is.finite(age), pmax(0, 1 - age / 60), 0)
      } else {
        0
      },
      quality_score = value_score * 2 + area_score * 0.8 + budget_score * 0.8 + room_score * 0.25 + recent_score * 0.15,
      quality_score = quality_score + if (identical(preference, "larger")) {
        area_score * 0.9
      } else if (identical(preference, "recent")) {
        recent_score
      } else if (identical(preference, "value")) {
        value_score
      } else {
        0
      }
    ) %>%
    arrange(desc(quality_score), price)
}

# Hàm assistant_listing_cards: xử lý logic trợ lý tư vấn.
assistant_listing_cards <- function(df, max_n = 5, compact = FALSE) {
  if (nrow(df) == 0) return("")
  cards_df <- df %>% mutate(source_link = listing_url(ad_url, source)) %>% slice_head(n = max_n)
  if (!"deal_pct" %in% names(cards_df)) cards_df$deal_pct <- NA_real_
  if (!"quality_score" %in% names(cards_df)) cards_df$quality_score <- NA_real_

  cards <- cards_df %>%
    mutate(
      price_m2_label = ifelse(is.na(price_per_m2) | !is.finite(price_per_m2), "Chưa có giá/m²", paste0(format_vnd_full(price_per_m2), "/m²")),
      reason = case_when(
        is.finite(deal_pct) & deal_pct >= 20 ~ paste0("Thấp hơn mặt bằng ", round(deal_pct, 0), "%"),
        is.finite(deal_pct) & deal_pct >= 10 ~ paste0("Giá/m² mềm hơn khoảng ", round(deal_pct, 0), "%"),
        is.finite(quality_score) & quality_score > 0.8 ~ "Cân bằng tốt giữa giá và diện tích",
        !is.na(price_per_m2) ~ "Giá/m² đáng đem đi so sánh",
        TRUE ~ "Khớp tiêu chí lọc"
      )
    ) %>%
    transmute(
      title = assistant_escape(title),
      meta = assistant_escape(paste0(district_name, " · ", category_name, " · ", source_label_vi(source))),
      price = assistant_escape(format_vnd_full(price)),
      area = assistant_escape(assistant_format_area(area)),
      price_m2 = assistant_escape(price_m2_label),
      reason = assistant_escape(reason),
      url = ifelse(!is.na(source_link) & source_link != "", assistant_escape(source_link), "")
    )

  paste0(
    "<div class='assistant-listings", if (compact) " compact" else "", "'>",
    paste0(
      "<div class='assistant-listing'>",
      "<div class='assistant-listing-reason'>", cards$reason, "</div>",
      "<div class='assistant-listing-title'>", cards$title, "</div>",
      "<div class='assistant-listing-meta'>", cards$meta, "</div>",
      "<div class='assistant-listing-stats'><span>", cards$price, "</span><span>", cards$area, "</span><span>", cards$price_m2, "</span></div>",
      ifelse(cards$url != "", paste0("<a href='", cards$url, "' target='_blank' rel='noopener noreferrer'>Xem tin</a>"), ""),
      "</div>",
      collapse = ""
    ),
    "</div>"
  )
}

# Hàm assistant_no_data_response: xử lý logic trợ lý tư vấn.
assistant_no_data_response <- function(criteria) {
  paste0(
    "<p>Mình chưa tìm thấy dữ liệu khớp với <b>", assistant_escape(assistant_criteria_text(criteria)), "</b>.</p>",
    assistant_insight_block("Có thể thử", c(
      "Nới ngân sách hoặc bỏ bớt điều kiện diện tích/phòng ngủ.",
      "Đổi sang khu vực lân cận để tăng số lượng mẫu.",
      "Hỏi theo dạng: ngân sách 4 tỷ mua căn hộ ở khu nào ổn?"
    ))
  )
}

# Hàm assistant_scope_note: xử lý logic trợ lý tư vấn.
assistant_scope_note <- function(criteria) {
  paste0("<p class='assistant-lead'>Mình đang hiểu tiêu chí là <b>", assistant_escape(assistant_criteria_text(criteria)), "</b>.</p>")
}

# Hàm assistant_stats_response: xử lý logic trợ lý tư vấn.
assistant_stats_response <- function(df, criteria) {
  scoped <- assistant_apply_criteria(df, criteria, limit_budget = FALSE, limit_area = TRUE)
  if (nrow(scoped) == 0) return(assistant_no_data_response(criteria))

  tx_count <- n_distinct(scoped$transaction_type)
  if (is.null(criteria$transaction) && tx_count > 1) {
    summary_df <- scoped %>%
      group_by(transaction_type) %>%
      summarise(
        listings = n(),
        median_price = median(price, na.rm = TRUE),
        median_m2 = median(price_per_m2, na.rm = TRUE),
        median_area = median(area, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(listings))
    rows <- paste0(
      "<tr><td>", assistant_escape(summary_df$transaction_type), "</td>",
      "<td>", format_count_vi(summary_df$listings), "</td>",
      "<td>", assistant_escape(format_vnd_full(summary_df$median_price)), "</td>",
      "<td>", assistant_escape(format_vnd_full(summary_df$median_m2)), "/m²</td>",
      "<td>", assistant_escape(assistant_format_area(summary_df$median_area)), "</td></tr>",
      collapse = ""
    )
    return(paste0(
      assistant_scope_note(criteria),
      "<p>Dữ liệu khớp đang lẫn cả bán và cho thuê, nên mình tách riêng để số giá không bị trộn sai đơn vị:</p>",
      "<div class='assistant-table-wrap'><table class='assistant-table'><thead><tr><th>Giao dịch</th><th>Số tin</th><th>Giá trung vị</th><th>Giá/m²</th><th>Diện tích</th></tr></thead><tbody>",
      rows,
      "</tbody></table></div>",
      assistant_insight_block("Gợi ý đọc nhanh", c(
        "Nếu bạn muốn mua, hãy hỏi thêm chữ mua hoặc ngân sách theo tỷ.",
        "Nếu bạn muốn thuê, hãy hỏi thêm chữ thuê hoặc ngân sách theo triệu/tháng."
      ))
    ))
  }

  tx <- criteria$transaction %||% names(sort(table(scoped$transaction_type), decreasing = TRUE))[1]
  scoped <- scoped %>% filter(transaction_type == tx)
  if (nrow(scoped) == 0) return(assistant_no_data_response(criteria))

  reference <- df %>% filter(transaction_type == tx)
  if (length(criteria$categories) > 0) {
    category_reference <- reference %>% filter(category_name %in% criteria$categories)
    if (nrow(category_reference) >= 20) reference <- category_reference
  }
  median_price <- median(scoped$price, na.rm = TRUE)
  median_m2 <- median(scoped$price_per_m2, na.rm = TRUE)
  reference_price <- median(reference$price, na.rm = TRUE)
  reference_m2 <- median(reference$price_per_m2, na.rm = TRUE)
  q <- suppressWarnings(quantile(scoped$price, probs = c(0.25, 0.75), na.rm = TRUE))
  top_district <- scoped %>% count(district_name, sort = TRUE) %>% slice(1)
  top_category <- scoped %>% count(category_name, sort = TRUE) %>% slice(1)

  paste0(
    "<p class='assistant-lead'>", assistant_intro("stats", criteria), "</p>",
    "<div class='assistant-answer-grid'>",
    "<div><b>", format_count_vi(nrow(scoped)), "</b><span>tin phù hợp</span></div>",
    "<div><b>", assistant_escape(format_vnd_full(median_price)), "</b><span>giá trung vị</span></div>",
    "<div><b>", assistant_escape(ifelse(is.finite(median_m2), format_vnd_full(median_m2), "Chưa có")), "</b><span>giá/m² trung vị</span></div>",
    "</div>",
    assistant_insight_block("Nhận định nhanh", c(
      paste0("Giá trung vị đang ", assistant_relative_text(median_price, reference_price, paste0("nhóm ", tx)), "."),
      paste0("Giá/m² ", assistant_relative_text(median_m2, reference_m2, "nhóm cùng giao dịch"), "."),
      if (all(is.finite(q))) paste0("Vùng giá phổ biến nằm trong khoảng ", format_vnd(q[[1]]), " - ", format_vnd(q[[2]]), ".") else NA_character_,
      paste0("Độ tin cậy dữ liệu: ", assistant_confidence_label(nrow(scoped)), " với ", format_count_vi(nrow(scoped)), " tin.")
    )),
    "<p>Khu vực có nhiều tin nhất trong nhóm này là <b>", assistant_escape(top_district$district_name[[1]]), "</b>. Loại BĐS xuất hiện nhiều nhất là <b>", assistant_escape(top_category$category_name[[1]]), "</b>.</p>"
  )
}

# Hàm assistant_mixed_listing_response: xử lý logic trợ lý tư vấn.
assistant_mixed_listing_response <- function(scoped, criteria, relaxed_note = NULL) {
  if (nrow(scoped) == 0) return(assistant_no_data_response(criteria))

  summary_df <- scoped %>%
    group_by(transaction_type) %>%
    summarise(
      listings = n(),
      median_price = median(price, na.rm = TRUE),
      median_m2 = median(price_per_m2, na.rm = TRUE),
      median_area = median(area, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(match(transaction_type, c("Bán", "Cho thuê")), desc(listings))

  rows <- paste0(
    "<tr><td>", assistant_escape(summary_df$transaction_type), "</td>",
    "<td>", format_count_vi(summary_df$listings), "</td>",
    "<td>", assistant_escape(format_vnd_full(summary_df$median_price)), "</td>",
    "<td>", assistant_escape(format_vnd_full(summary_df$median_m2)), "/m²</td>",
    "<td>", assistant_escape(assistant_format_area(summary_df$median_area)), "</td></tr>",
    collapse = ""
  )

  section_html <- paste0(vapply(summary_df$transaction_type, function(tx) {
    tx_criteria <- criteria
    tx_criteria$transaction <- tx
    tx_cards <- scoped %>%
      filter(transaction_type == tx) %>%
      assistant_sort_recommendations(tx_criteria)
    if (nrow(tx_cards) == 0) return("")
    paste0(
      "<p><b>", assistant_escape(ifelse(identical(tx, "Bán"), "Tin bán nổi bật", "Tin thuê nổi bật")), "</b></p>",
      assistant_listing_cards(tx_cards, ifelse(identical(tx, "Bán"), 4, 3), compact = TRUE)
    )
  }, character(1)), collapse = "")

  paste0(
    assistant_scope_note(criteria),
    "<p>Bạn hỏi theo hướng xem <b>tin đăng</b>, nên mình đưa listing trực tiếp. Vì câu chưa nói rõ mua hay thuê, mình tách hai nhóm để giá không bị trộn sai đơn vị:</p>",
    "<div class='assistant-table-wrap'><table class='assistant-table'><thead><tr><th>Giao dịch</th><th>Số tin</th><th>Giá trung vị</th><th>Giá/m²</th><th>Diện tích</th></tr></thead><tbody>",
    rows,
    "</tbody></table></div>",
    assistant_insight_block("Cách đọc nhanh", c(
      relaxed_note,
      paste0("Tổng cộng có ", format_count_vi(nrow(scoped)), " tin trong nhóm này."),
      "Mỗi card có link gốc để bạn kiểm ảnh, vị trí, mô tả và tình trạng tin."
    )),
    section_html
  )
}

# Hàm assistant_recommend_response: xử lý logic trợ lý tư vấn.
assistant_recommend_response <- function(df, criteria) {
  if (is.null(criteria$transaction) && !assistant_is_finite(criteria$budget_max) && length(criteria$districts) == 0 && length(criteria$categories) == 0) {
    return(assistant_clarify_response("recommend", criteria, c("Bạn muốn mua hay thuê?", "Ngân sách khoảng bao nhiêu?")))
  }
  if (isTRUE(criteria$wants_location) && length(criteria$districts) == 0) {
    return(assistant_scout_response(df, criteria))
  }

  scoped_raw <- assistant_apply_criteria(df, criteria, limit_budget = TRUE, limit_area = TRUE)
  relaxed_note <- NULL
  if (nrow(scoped_raw) == 0) {
    scoped_raw <- assistant_apply_criteria(df, criteria, limit_budget = FALSE, limit_area = FALSE)
    relaxed_note <- "Mình không thấy tin khớp đủ mọi điều kiện, nên đang nới ngân sách/diện tích để lấy nhóm gần nhất."
  }
  if (is.null(criteria$transaction) && n_distinct(scoped_raw$transaction_type) > 1) {
    return(assistant_mixed_listing_response(scoped_raw, criteria, relaxed_note))
  }

  scoped <- assistant_sort_recommendations(scoped_raw, criteria)
  if (nrow(scoped) == 0) return(assistant_no_data_response(criteria))

  median_fit <- median(scoped$price, na.rm = TRUE)
  budget_note <- if (assistant_is_finite(criteria$budget_max)) {
    paste0("Giá trung vị nhóm gợi ý là ", format_vnd(median_fit), ", so với ngân sách ", format_vnd(criteria$budget_max), ".")
  } else {
    "Mình xếp hạng theo giá/m², diện tích và độ khớp với tiêu chí để tránh chọn tin rẻ nhưng quá nhỏ."
  }

  paste0(
    "<p class='assistant-lead'>", assistant_intro("recommend", criteria), "</p>",
    assistant_insight_block("Cách mình chọn", c(
      relaxed_note,
      budget_note,
      paste0("Có ", format_count_vi(nrow(scoped)), " tin trong nhóm ứng viên. Độ tin cậy: ", assistant_confidence_label(nrow(scoped)), "."),
      "Nên mở link gốc để kiểm hình ảnh, pháp lý, hẻm/đường và tình trạng tin."
    )),
    assistant_listing_cards(scoped, 6)
  )
}

# Hàm assistant_scout_response: xử lý logic trợ lý tư vấn.
assistant_scout_response <- function(df, criteria) {
  if (is.null(criteria$transaction)) {
    criteria$transaction <- if (assistant_is_finite(criteria$budget_max) && criteria$budget_max < 250000000) "Cho thuê" else "Bán"
  }
  criteria$districts <- character()

  base <- assistant_apply_criteria(df, criteria, limit_budget = FALSE, limit_area = TRUE)
  fit <- assistant_apply_criteria(df, criteria, limit_budget = TRUE, limit_area = TRUE)
  if (nrow(base) == 0) return(assistant_no_data_response(criteria))
  if (nrow(fit) == 0) fit <- base

  base_summary <- base %>%
    group_by(district_name) %>%
    summarise(
      listings = n(),
      median_price = median(price, na.rm = TRUE),
      median_m2 = median(price_per_m2, na.rm = TRUE),
      median_area = median(area, na.rm = TRUE),
      .groups = "drop"
    )
  fit_summary <- fit %>%
    group_by(district_name) %>%
    summarise(fit_count = n(), best_price = min(price, na.rm = TRUE), .groups = "drop")
  city_m2 <- median(base$price_per_m2, na.rm = TRUE)

  summary_df <- base_summary %>%
    left_join(fit_summary, by = "district_name") %>%
    mutate(
      fit_count = coalesce(fit_count, 0L),
      best_price = if_else(is.finite(best_price), best_price, NA_real_),
      fit_rate = fit_count / pmax(listings, 1),
      value_score = if_else(is.finite(median_m2) & is.finite(city_m2) & city_m2 > 0, pmax(0, (city_m2 - median_m2) / city_m2), 0),
      scout_score = fit_count * 1.4 + fit_rate * 8 + value_score * 5
    ) %>%
    filter(listings >= 3 | fit_count > 0) %>%
    arrange(desc(scout_score), median_m2) %>%
    slice_head(n = 6)

  if (nrow(summary_df) == 0) return(assistant_no_data_response(criteria))
  rows <- paste0(
    "<tr><td>", assistant_escape(summary_df$district_name), "</td>",
    "<td>", format_count_vi(summary_df$fit_count), "/", format_count_vi(summary_df$listings), "</td>",
    "<td>", assistant_escape(format_vnd_full(summary_df$median_price)), "</td>",
    "<td>", assistant_escape(format_vnd_full(summary_df$median_m2)), "/m²</td>",
    "<td>", assistant_escape(assistant_format_area(summary_df$median_area)), "</td></tr>",
    collapse = ""
  )

  top_districts <- summary_df$district_name
  cards <- fit %>% filter(district_name %in% top_districts) %>% assistant_sort_recommendations(criteria)

  paste0(
    assistant_scope_note(criteria),
    assistant_insight_block("Khu vực nên soi trước", c(
      paste0(summary_df$district_name[[1]], " đang nổi bật nhất vì có ", format_count_vi(summary_df$fit_count[[1]]), " tin khớp/tiệm cận tiêu chí."),
      "Bảng dưới ưu tiên nơi vừa có đủ mẫu, vừa có giá/m² mềm hơn mặt bằng nhóm lọc.",
      "Nếu mục tiêu là ở thật, vẫn nên lọc thêm khoảng cách đi làm, tiện ích và pháp lý."
    )),
    "<div class='assistant-table-wrap'><table class='assistant-table'><thead><tr><th>Khu vực</th><th>Tin khớp/tổng</th><th>Giá trung vị</th><th>Giá/m²</th><th>Diện tích</th></tr></thead><tbody>",
    rows,
    "</tbody></table></div>",
    if (nrow(cards) > 0) paste0("<p>Một vài tin tiêu biểu trong các khu vực này:</p>", assistant_listing_cards(cards, 4, compact = TRUE)) else ""
  )
}

# Hàm assistant_compare_response: xử lý logic trợ lý tư vấn.
assistant_compare_response <- function(df, criteria) {
  districts <- criteria$districts
  if (length(districts) < 2) {
    scoped_for_top <- assistant_apply_criteria(df, criteria, limit_budget = FALSE, limit_area = TRUE)
    districts <- scoped_for_top %>%
      count(district_name, sort = TRUE) %>%
      slice_head(n = 5) %>%
      pull(district_name)
  }

  scoped <- df %>% filter(district_name %in% districts)
  scoped <- assistant_apply_criteria(scoped, modifyList(criteria, list(districts = districts)), limit_budget = FALSE, limit_area = TRUE)
  if (nrow(scoped) == 0) return(assistant_no_data_response(criteria))

  summary_df <- scoped %>%
    group_by(district_name) %>%
    summarise(
      listings = n(),
      median_price = median(price, na.rm = TRUE),
      median_m2 = median(price_per_m2, na.rm = TRUE),
      median_area = median(area, na.rm = TRUE),
      fit_budget = if (assistant_is_finite(criteria$budget_max)) sum(price <= criteria$budget_max, na.rm = TRUE) else NA_integer_,
      .groups = "drop"
    ) %>%
    arrange(median_m2)

  best_value <- summary_df %>% arrange(median_m2) %>% slice(1)
  strongest <- summary_df %>% arrange(desc(listings)) %>% slice(1)
  premium <- summary_df %>% arrange(desc(median_m2)) %>% slice(1)
  budget_header <- if (assistant_is_finite(criteria$budget_max)) "<th>Dưới ngân sách</th>" else ""
  budget_cells <- if (assistant_is_finite(criteria$budget_max)) paste0("<td>", format_count_vi(summary_df$fit_budget), "</td>") else ""

  rows <- paste0(
    "<tr><td>", assistant_escape(summary_df$district_name), "</td>",
    "<td>", format_count_vi(summary_df$listings), "</td>",
    "<td>", assistant_escape(format_vnd_full(summary_df$median_price)), "</td>",
    "<td>", assistant_escape(format_vnd_full(summary_df$median_m2)), "/m²</td>",
    "<td>", assistant_escape(assistant_format_area(summary_df$median_area)), "</td>",
    budget_cells,
    "</tr>",
    collapse = ""
  )

  paste0(
    "<p class='assistant-lead'>", assistant_intro("compare", criteria), "</p>",
    assistant_insight_block("Kết luận nhanh", c(
      paste0(best_value$district_name[[1]], " mềm nhất theo giá/m² trong nhóm so sánh."),
      paste0(premium$district_name[[1]], " đang cao nhất theo giá/m²."),
      paste0(strongest$district_name[[1]], " có độ phủ dữ liệu tốt nhất với ", format_count_vi(strongest$listings[[1]]), " tin.")
    )),
    "<div class='assistant-table-wrap'><table class='assistant-table'><thead><tr><th>Khu vực</th><th>Số tin</th><th>Giá trung vị</th><th>Giá/m²</th><th>Diện tích</th>",
    budget_header,
    "</tr></thead><tbody>",
    rows,
    "</tbody></table></div>"
  )
}

# Hàm assistant_undervalued_response: xử lý logic trợ lý tư vấn.
assistant_undervalued_response <- function(df, criteria) {
  scoped <- assistant_apply_criteria(df, criteria, limit_budget = TRUE, limit_area = TRUE)
  if (nrow(scoped) == 0) scoped <- assistant_apply_criteria(df, criteria, limit_budget = FALSE, limit_area = TRUE)
  if (nrow(scoped) == 0) return(assistant_no_data_response(criteria))

  reference <- df %>%
    filter(!is.na(price_per_m2), price_per_m2 > 0) %>%
    group_by(transaction_type, district_name, category_name) %>%
    summarise(ref_m2 = median(price_per_m2, na.rm = TRUE), ref_n = n(), .groups = "drop")

  deals <- scoped %>%
    filter(!is.na(price_per_m2), price_per_m2 > 0) %>%
    left_join(reference, by = c("transaction_type", "district_name", "category_name")) %>%
    mutate(deal_pct = (ref_m2 - price_per_m2) / ref_m2 * 100) %>%
    filter(is.finite(deal_pct), ref_n >= 5, deal_pct >= 8) %>%
    arrange(desc(deal_pct), price) %>%
    assistant_sort_recommendations(criteria)

  if (nrow(deals) == 0) {
    closest <- assistant_sort_recommendations(scoped, criteria)
    return(paste0(
      assistant_scope_note(criteria),
      assistant_insight_block("Chưa thấy deal rõ ràng", c(
        "Không có tin nào thấp hơn mặt bằng đủ mạnh trong nhóm này.",
        "Dưới đây là các tin gần tiêu chí nhất để bạn so tiếp."
      )),
      assistant_listing_cards(closest, 5)
    ))
  }

  paste0(
    assistant_scope_note(criteria),
    assistant_insight_block("Tin có dấu hiệu giá tốt", c(
      paste0("Mình so giá/m² từng tin với trung vị cùng khu vực và loại BĐS, chỉ lấy nhóm thấp hơn ít nhất 8%."),
      paste0("Tìm thấy ", format_count_vi(nrow(deals)), " ứng viên. Đây là tín hiệu sàng lọc ban đầu, chưa thay cho kiểm pháp lý và vị trí thực.")
    )),
    assistant_listing_cards(deals, 6)
  )
}

# Hàm assistant_predict_response: xử lý logic trợ lý tư vấn.
assistant_predict_response <- function(df, criteria) {
  missing <- c()
  if (length(criteria$districts) == 0) missing <- c(missing, "khu vực cũ")
  if (!assistant_is_finite(criteria$area)) missing <- c(missing, "diện tích")
  if (length(missing) > 0) {
    return(assistant_clarify_response("predict", criteria, paste0("Cần thêm ", paste(missing, collapse = " và "), " để chạy model.")))
  }

  transaction <- criteria$transaction %||% "Bán"
  district <- criteria$districts[[1]]
  segment_df <- df %>% filter(transaction_type == transaction, district_name == district)
  if (nrow(segment_df) == 0) segment_df <- df %>% filter(transaction_type == transaction)
  category <- if (length(criteria$categories) > 0) {
    category_counts <- segment_df %>%
      filter(category_name %in% criteria$categories) %>%
      count(category_name, sort = TRUE)
    if (nrow(category_counts) > 0) category_counts$category_name[[1]] else criteria$categories[[1]]
  } else {
    segment_df %>% count(category_name, sort = TRUE) %>% slice(1) %>% pull(category_name)
  }
  if (length(category) == 0 || is.na(category)) category <- df$category_name[[1]]

  input_row <- build_prediction_row(
    df = df,
    district = district,
    category = category,
    ward = "Không rõ",
    area = criteria$area,
    rooms = ifelse(is.na(criteria$rooms), 0, criteria$rooms),
    transaction_type = transaction
  )
  pred <- predict_price(input_row, identical(transaction, "Cho thuê"))
  if (is.na(pred)) {
    return("<p>Model hiện chưa dự đoán được case này. Bạn thử chọn khu vực/loại BĐS có nhiều dữ liệu hơn nhé.</p>")
  }

  local_df <- df %>% filter(transaction_type == transaction, district_name == district, category_name == category)
  if (nrow(local_df) == 0) local_df <- df %>% filter(transaction_type == transaction, district_name == district)
  comparable <- local_df %>%
    filter(!is.na(area), area >= criteria$area * 0.8, area <= criteria$area * 1.2) %>%
    assistant_sort_recommendations(criteria)
  local_median <- median(local_df$price, na.rm = TRUE)
  delta_text <- assistant_relative_text(pred, local_median, "trung vị nhóm cùng khu vực/loại")

  paste0(
    "<p class='assistant-lead'>", assistant_intro("predict", criteria), "</p>",
    "<div class='assistant-prediction'>", assistant_escape(format_vnd_full(pred)), "</div>",
    assistant_insight_block("Cách hiểu kết quả", c(
      paste0("Mô hình dùng: ", prediction_model_label(identical(transaction, "Cho thuê")), "."),
      paste0("So với dữ liệu thực cùng nhóm, giá dự đoán đang ", delta_text, "."),
      paste0("Độ tin cậy dữ liệu: ", assistant_confidence_label(nrow(local_df)), " với ", format_count_vi(nrow(local_df)), " tin cùng khu vực/loại BĐS."),
      "Con số này dùng để sàng lọc và thương lượng ban đầu, chưa thay thế thẩm định thực tế."
    )),
    if (nrow(comparable) > 0) paste0("<p>Một vài tin gần diện tích này để đối chiếu:</p>", assistant_listing_cards(comparable, 3, compact = TRUE)) else ""
  )
}

# Hàm assistant_explain_response: xử lý logic trợ lý tư vấn.
assistant_explain_response <- function(df, criteria) {
  tx <- criteria$transaction %||% "Bán"
  importance_path <- if (identical(tx, "Cho thuê") && file.exists(RF_IMPORTANCE_RENT_PATH)) RF_IMPORTANCE_RENT_PATH else RF_IMPORTANCE_SALE_PATH
  factor_items <- c()
  if (file.exists(importance_path)) {
    importance <- read_csv(importance_path, show_col_types = FALSE) %>% slice_head(n = 5)
    if (all(c("feature", "IncNodePurity") %in% names(importance))) {
      factor_items <- paste0(feature_label_vi(importance$feature), " là yếu tố có ảnh hưởng cao trong Random Forest.")
    }
  }

  scoped <- assistant_apply_criteria(df, criteria, limit_budget = FALSE, limit_area = TRUE)
  if (nrow(scoped) == 0) scoped <- df %>% filter(transaction_type == tx)
  local_items <- c(
    paste0("Mẫu đang xét có ", format_count_vi(nrow(scoped)), " tin, độ tin cậy ", assistant_confidence_label(nrow(scoped)), "."),
    paste0("Giá trung vị hiện là ", format_vnd_full(median(scoped$price, na.rm = TRUE)), "."),
    paste0("Giá/m² trung vị là ", format_vnd_full(median(scoped$price_per_m2, na.rm = TRUE)), "/m².")
  )

  paste0(
    assistant_scope_note(criteria),
    assistant_insight_block("Yếu tố chính trong dữ liệu", c(local_items, factor_items)),
    "<p>Nói ngắn gọn: bot không tự đoán cảm tính, mà lấy tiêu chí bạn nhập, lọc dữ liệu thật, rồi dùng thống kê/model để trả lời. Những chỗ dữ liệu ít sẽ được đánh dấu độ tin cậy thấp.</p>"
  )
}

# Hàm assistant_help_response: xử lý logic trợ lý tư vấn.
assistant_help_response <- function() {
  paste0(
    "<p class='assistant-lead'>Mình là trợ lý BĐS local chạy trên dữ liệu và model R của dashboard. Mình có memory hội thoại, biết hỏi lại khi thiếu dữ kiện và chỉ dùng số liệu thật trong project.</p>",
    assistant_insight_block("Bạn có thể hỏi", c(
      "4 tỷ mua căn hộ tầm 60m2 ở khu nào ổn?",
      "So sánh Thủ Đức với Quận 7 cho căn hộ bán.",
      "Tìm tin giá tốt hơn mặt bằng ở Bình Tân dưới 4 tỷ.",
      "Dự đoán căn hộ 70m2 2PN ở Thủ Đức.",
      "Còn Quận 7 thì sao?"
    ))
  )
}

# Hàm assistant_clarify_response: xử lý logic trợ lý tư vấn.
assistant_clarify_response <- function(intent, criteria, questions) {
  questions <- unique(as.character(questions))
  paste0(
    assistant_scope_note(criteria),
    assistant_insight_block("Mình cần thêm một chút", questions),
    "<p>Bạn trả lời ngắn cũng được, ví dụ: <b>mua, 4 tỷ, căn hộ</b> hoặc <b>thuê, dưới 15 triệu, Thủ Đức</b>.</p>"
  )
}

# Hàm assistant_missing_requirements: xử lý logic trợ lý tư vấn.
assistant_missing_requirements <- function(intent, criteria) {
  missing <- c()
  if (identical(intent, "predict")) {
    if (length(criteria$districts) == 0) missing <- c(missing, "khu vực cũ")
    if (!assistant_is_finite(criteria$area)) missing <- c(missing, "diện tích")
  }
  if (identical(intent, "scout") && is.null(criteria$transaction) && !assistant_is_finite(criteria$budget_max)) {
    missing <- c(missing, "bạn muốn mua hay thuê và ngân sách khoảng bao nhiêu")
  }
  missing
}

# Hàm assistant_answer_bundle: xử lý logic trợ lý tư vấn.
assistant_answer_bundle <- function(question, df, context = assistant_empty_context()) {
  criteria <- assistant_extract_criteria(question, df)
  criteria <- assistant_merge_criteria(criteria, context)
  intent <- assistant_detect_intent(question, criteria, context)
  missing <- assistant_missing_requirements(intent, criteria)

  if (length(missing) > 0) {
    html <- assistant_clarify_response(intent, criteria, paste0("Cần thêm ", missing, "."))
    return(list(html = html, context = list(criteria = criteria, intent = intent, pending = TRUE, last_answer_at = Sys.time())))
  }

  html <- switch(
    intent,
    help = assistant_help_response(),
    predict = assistant_predict_response(df, criteria),
    compare = assistant_compare_response(df, criteria),
    recommend = assistant_recommend_response(df, criteria),
    scout = assistant_scout_response(df, criteria),
    undervalued = assistant_undervalued_response(df, criteria),
    explain = assistant_explain_response(df, criteria),
    stats = assistant_stats_response(df, criteria),
    assistant_stats_response(df, criteria)
  )

  list(
    html = html,
    context = list(criteria = criteria, intent = intent, pending = FALSE, last_answer_at = Sys.time())
  )
}

# Hàm assistant_answer: xử lý logic trợ lý tư vấn.
assistant_answer <- function(question, df, context = assistant_empty_context()) {
  assistant_answer_bundle(question, df, context)$html
}

# Hàm assistant_message: xử lý logic trợ lý tư vấn.
assistant_message <- function(role, html) {
  list(role = role, html = html)
}

# Hàm gemini_star_svg: tạo thành phần giao diện.
gemini_star_svg <- function() {
  htmltools::HTML('<svg class="gemini-star-icon" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
    <path d="M12 2C12 2 12.5 7.5 18 8C12.5 8.5 12 14 12 14C12 14 11.5 8.5 6 8C11.5 7.5 12 2 12 2Z" fill="url(#geminiGradient)"/>
    <path d="M17 14C17 14 17.25 16.75 20 17C17.25 17.25 17 20 17 20C17 20 16.75 17.25 14 17C16.75 16.75 17 14 17 14Z" fill="url(#geminiGradient)"/>
    <defs>
      <linearGradient id="geminiGradient" x1="0%" y1="0%" x2="100%" y2="100%">
        <stop offset="0%" stop-color="#1a73e8"/>
        <stop offset="50%" stop-color="#8ab4f8"/>
        <stop offset="100%" stop-color="#e289f2"/>
      </linearGradient>
    </defs>
  </svg>')
}

# Hàm assistant_intro: xử lý logic trợ lý tư vấn.
assistant_intro <- function(type, criteria = NULL) {
  intros <- switch(type,
    "stats" = c(
      paste0("Chào bạn! Dưới đây là phân tích thị trường chi tiết cho khu vực <b>", htmltools::htmlEscape(assistant_criteria_text(criteria)), "</b> mà mình vừa tổng hợp được:"),
      paste0("Dựa trên nguồn dữ liệu bất động sản mới nhất cho nhóm <b>", htmltools::htmlEscape(assistant_criteria_text(criteria)), "</b>, mình xin gửi bạn một số nhận định nhanh nhé:"),
      paste0("Thị trường khu vực <b>", htmltools::htmlEscape(assistant_criteria_text(criteria)), "</b> đang có những chỉ số khá thú vị đấy. Cùng mình điểm qua các con số thống kê này nhé:")
    ),
    "recommend" = c(
      paste0("Mình đã lùng sục dữ liệu và tìm thấy <b>", htmltools::htmlEscape(assistant_criteria_text(criteria)), "</b> phù hợp. Dưới đây là các tin đăng chất lượng nhất dành cho bạn:"),
      paste0("Chào bạn! Đây là các lựa chọn tốt nhất khớp với tiêu chí <b>", htmltools::htmlEscape(assistant_criteria_text(criteria)), "</b> mà mình đã chọn lọc kỹ:"),
      paste0("Dưới đây là danh sách bất động sản nổi bật trong nhóm <b>", htmltools::htmlEscape(assistant_criteria_text(criteria)), "</b> mà bạn đang quan tâm:")
    ),
    "compare" = c(
      "Dưới đây là bảng phân tích so sánh chi tiết giữa các khu vực mà bạn yêu cầu. Hy vọng thông tin này giúp bạn có cái nhìn tổng quan nhất:",
      "Chào bạn! Mình đã đặt lên bàn cân các chỉ số giá bán, diện tích trung bình của các quận này. Cùng xem kết quả nhé:",
      "Mỗi khu vực đều có một mức giá và tiềm năng khác nhau. Dưới đây là thống kê so sánh chi tiết để bạn dễ đánh giá:"
    ),
    "predict" = c(
      "Chào bạn! Sử dụng mô hình Machine Learning được huấn luyện trên dữ liệu thị trường thật, mình xin gửi bạn kết quả dự báo giá như sau:",
      "Đây là kết quả ước lượng giá trị từ mô hình dự đoán bất động sản TP.HCM dành cho bạn:",
      "Dựa trên các đặc trưng bạn cung cấp (khu vực, diện tích, số phòng...), mô hình dự báo của mình đưa ra kết quả sau:"
    ),
    c("Chào bạn! Dưới đây là câu trả lời từ mình:")
  )
  sample(intros, 1)
}

# Hàm assistant_outro: xử lý logic trợ lý tư vấn.
assistant_outro <- function(type) {
  outros <- switch(type,
    "stats" = c(
      "<p>Nếu bạn muốn tìm hiểu sâu hơn về tin đăng cụ thể hoặc dự đoán giá nhà tại khu vực này, cứ hỏi mình nhé! 😉</p>",
      "<p>Hy vọng bức tranh thống kê này giúp ích cho bạn trong việc định giá khu vực. Hãy cho mình biết nếu bạn cần xem danh sách tin đăng nhé!</p>",
      "<p>Cần thêm thông tin chi tiết về từng phường/xã hay so sánh với quận khác, bạn cứ nhắn mình nha!</p>"
    ),
    "recommend" = c(
      "<p>Bạn có thể click vào link <b>Xem tin &rarr;</b> ở từng card để đến trang tin đăng gốc và xem ảnh chi tiết nhé. Chúc bạn tìm được căn nhà ưng ý! 🏡</p>",
      "<p>Các tin đăng này đều được chọn lọc dựa trên sự tối ưu về giá và diện tích. Hãy tham khảo kỹ trước khi liên hệ chính chủ nha!</p>",
      "<p>Nếu bạn muốn nới rộng ngân sách hoặc đổi sang khu vực khác để so sánh, cứ nói cho mình biết nhé!</p>"
    ),
    "compare" = c(
      "<p>Như bạn thấy đấy, sự chênh lệch giá giữa các khu vực là khá rõ rệt. Hãy cân đối tài chính và nhu cầu di chuyển để đưa ra lựa chọn phù hợp nhất nhé! ⚖️</p>",
      "<p>Bạn có muốn mình gợi ý một số tin đăng cụ thể tại quận có mức giá tốt nhất trong số này không?</p>",
      "<p>Hy vọng bảng so sánh trực quan này sẽ giúp bạn dễ dàng đưa ra quyết định đầu tư hoặc an cư!</p>"
    ),
    "predict" = c(
      "<p>Lưu ý là mô hình dự đoán này mang tính tham khảo cao dựa trên thuật toán, bạn nên kết hợp khảo sát thực tế trước khi xuống tiền nhé! 🧠</p>",
      "<p>Bạn có muốn mình so sánh giá dự đoán này với mặt bằng giá trung bình thực tế tại quận đó không?</p>",
      "<p>Chúc bạn có những quyết định đầu tư thật sáng suốt! Nếu muốn chạy lại dự đoán cho căn khác, cứ nhập thông tin nha.</p>"
    ),
    c("")
  )
  sample(outros, 1)
}

# Hàm price_color: định dạng giá trị để hiển thị.
price_color <- function(price, low_cutoff = 3e9, high_cutoff = 8e9) {
  dplyr::case_when(
    is.na(price) ~ "#64748b",
    price < low_cutoff ~ "#059669",
    price < high_cutoff ~ "#d97706",
    TRUE ~ "#ed1c24"
  )
}

# Hàm kpi_card: tạo thành phần giao diện.
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

# Hàm app_panel: tạo thành phần giao diện.
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

# Hàm filter_field: tạo thành phần giao diện.
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

# Hàm filter_actions: hỗ trợ xử lý dữ liệu trong script.
filter_actions <- function(reset_id) {
  div(
    class = "filter-actions",
    actionButton(reset_id, label = tagList(icon("rotate-left"), span("Đặt lại")), class = "btn-filter-reset")
  )
}

# Hàm filter_select: tạo thành phần giao diện.
filter_select <- function(input_id, choices, all_label) {
  selectInput(
    input_id,
    NULL,
    choices = c(setNames("__all__", all_label), setNames(choices, choices)),
    selected = "__all__",
    selectize = FALSE
  )
}

# Hàm filter_source_select: tạo thành phần giao diện.
filter_source_select <- function(input_id, choices, all_label) {
  selectInput(
    input_id,
    NULL,
    choices = c(setNames("__all__", all_label), setNames(choices, source_label_vi(choices))),
    selected = "__all__",
    selectize = FALSE
  )
}

# Hàm chart_mode_control: tạo thành phần giao diện.
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

# Hàm chart_colors: định dạng giá trị để hiển thị.
chart_colors <- function(n) {
  if (n <= length(chart_palette)) return(chart_palette[seq_len(n)])
  grDevices::hcl.colors(n, palette = "Dark 3")
}

# Hàm chart_theme: tạo thành phần giao diện.
chart_theme <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(color = "#1f2937"),
      plot.margin = ggplot2::margin(8, 14, 8, 8),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#e5e7eb", linewidth = 0.35),
      axis.title = element_text(color = "#475569", face = "bold", size = 11),
      axis.text = element_text(color = "#334155", size = 11),
      legend.position = "bottom",
      legend.title = element_text(color = "#475569", face = "bold", size = 11),
      legend.text = element_text(color = "#334155", size = 11)
    )
}

# Hàm interactive_chart: tạo thành phần giao diện.
interactive_chart <- function(plot, tooltip = "all") {
  ggplotly(plot, tooltip = tooltip) %>%
    layout(
      margin = list(l = 140, r = 24, t = 20, b = 96),
      font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
      hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#d7e6f5", font = list(color = "#1f2937")),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      hovermode = "closest",
      legend = list(orientation = "h", x = 0, y = -0.28, font = list(size = 11), itemwidth = 30),
      xaxis = list(automargin = TRUE, nticks = 4, gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb", tickfont = list(size = 10)),
      yaxis = list(automargin = TRUE, tickfont = list(size = 11))
    ) %>%
    config(displayModeBar = FALSE, responsive = TRUE)
}

# ============================================================
# SUY LUAN THONG KE / BOOTSTRAP / CLT HELPERS
# Cac ham trong khoi nay phuc vu tab "Suy luan thong ke":
# - empirical probability va conditional probability
# - standard error, sampling distribution theo CLT
# - bootstrap confidence interval
# - hypothesis testing va data quality diagnostics
# ============================================================

# Hàm finite_positive: kiểm tra giá trị số dương hữu hạn.
finite_positive <- function(x) {
  !is.na(x) & is.finite(x) & x > 0
}

# Hàm safe_quantile: tính quantile an toàn khi dữ liệu ít hoặc thiếu.
safe_quantile <- function(x, probs, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return(rep(default, length(probs)))
  as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE))
}

# Hàm confidence_level_value: chuyển lựa chọn UI thành mức tin cậy.
confidence_level_value <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  if (!is.finite(value) || !(value %in% c(0.90, 0.95, 0.99))) 0.95 else value
}

# Hàm bootstrap_median_ci: ước lượng CI bootstrap cho trung vị.
bootstrap_median_ci <- function(x, reps = 600, confidence = 0.95, seed = 2026) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  reps <- max(100, min(3000, as.integer(reps %||% 600)))
  confidence <- confidence_level_value(confidence)
  if (length(x) < 5) {
    return(list(
      observed = ifelse(length(x) == 0, NA_real_, median(x, na.rm = TRUE)),
      lower = NA_real_,
      upper = NA_real_,
      distribution = numeric(),
      n = length(x),
      confidence = confidence
    ))
  }

  set.seed(seed)
  boot <- replicate(reps, median(sample(x, size = length(x), replace = TRUE), na.rm = TRUE))
  alpha <- (1 - confidence) / 2
  ci <- safe_quantile(boot, c(alpha, 1 - alpha))
  list(
    observed = median(x, na.rm = TRUE),
    lower = ci[[1]],
    upper = ci[[2]],
    distribution = boot,
    n = length(x),
    confidence = confidence
  )
}

# Hàm bootstrap_mean_distribution: mô phỏng phân phối mẫu của trung bình theo CLT.
bootstrap_mean_distribution <- function(x, sample_size = 30, reps = 600, seed = 2026) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  reps <- max(100, min(3000, as.integer(reps %||% 600)))
  sample_size <- max(2, min(length(x), as.integer(sample_size %||% 30)))
  if (length(x) < 5) return(numeric())
  set.seed(seed)
  replicate(reps, mean(sample(x, size = sample_size, replace = TRUE), na.rm = TRUE))
}

# Hàm p_value_label: định dạng p-value cho bảng kiểm định.
p_value_label <- function(p_value) {
  if (!is.finite(p_value)) return("NA")
  if (p_value < 0.001) return("< 0,001")
  format_number_vi(p_value, 3)
}

# Hàm hypothesis_decision: diễn giải quyết định kiểm định.
hypothesis_decision <- function(p_value, alpha = 0.05) {
  if (!is.finite(p_value)) return("Không đủ dữ liệu")
  if (p_value < alpha) {
    "Bác bỏ H0"
  } else {
    "Chưa đủ bằng chứng bác bỏ H0"
  }
}

# Hàm build_data_quality_summary: kiểm tra chất lượng dữ liệu đang dùng.
build_data_quality_summary <- function(df) {
  today <- Sys.Date()
  date_values <- as.Date(df$posted_at)
  price_hi <- safe_quantile(df$price, 0.995)
  area_hi <- safe_quantile(df$area, 0.995)
  duplicate_key <- paste(
    clean_display_label(df$source),
    clean_display_label(df$title),
    round(df$price %||% 0, -3),
    round(df$area %||% 0, 1),
    clean_display_label(df$district_name),
    sep = "|"
  )

  tibble::tibble(
    nhom = c(
      "Ngày đăng tương lai",
      "Thiếu khu vực/loại BĐS",
      "Không có giá/m² hợp lệ",
      "Tọa độ phải ước lượng",
      "Trùng lặp nghi ngờ",
      "Giá thuộc 0,5% cao nhất",
      "Diện tích thuộc 0,5% cao nhất"
    ),
    so_dong = c(
      sum(!is.na(date_values) & date_values > today),
      sum(is_missing_label(df$district_name) | is_missing_label(df$category_name)),
      sum(!finite_positive(df$price_per_m2)),
      sum(df$coord_status != "Tọa độ gốc từ nguồn", na.rm = TRUE),
      sum(duplicated(duplicate_key), na.rm = TRUE),
      sum(is.finite(df$price) & df$price >= price_hi[[1]], na.rm = TRUE),
      sum(is.finite(df$area) & df$area >= area_hi[[1]], na.rm = TRUE)
    ),
    muc_do = c(
      "Cần rà",
      "Cần rà",
      "Cần rà",
      "Theo dõi",
      "Theo dõi",
      "Theo dõi",
      "Theo dõi"
    ),
    ghi_chu = c(
      "Ngày lớn hơn ngày chạy app, nên kiểm tra nguồn crawl hoặc format ngày.",
      "Các dòng này làm yếu phân tích theo khu vực/loại hình.",
      "Thiếu giá hoặc diện tích hợp lệ sẽ ảnh hưởng biểu đồ giá/m².",
      "App đã dùng tâm khu vực cũ và jitter nhẹ để vẫn hiển thị bản đồ.",
      "Cùng nguồn, tiêu đề, giá, diện tích và khu vực.",
      "Không xóa tự động; dùng để cảnh báo outlier khi đọc kết quả.",
      "Không xóa tự động; dùng để cảnh báo outlier khi đọc kết quả."
    )
  )
}

# Hàm prediction_market_band: tạo khoảng tham khảo thị trường cho dự đoán.
prediction_market_band <- function(df, district, category, transaction_type, area) {
  area <- suppressWarnings(as.numeric(area))
  scoped <- df %>%
    filter(transaction_type == !!transaction_type, district_name == !!district, category_name == !!category)
  if (is.finite(area) && area > 0) {
    comparable <- scoped %>% filter(is.na(.data$area) | (.data$area >= area * 0.8 & .data$area <= area * 1.2))
    if (nrow(comparable) >= 15) scoped <- comparable
  }
  if (nrow(scoped) < 10) {
    scoped <- df %>% filter(transaction_type == !!transaction_type, district_name == !!district)
  }
  if (nrow(scoped) < 10) {
    scoped <- df %>% filter(transaction_type == !!transaction_type, category_name == !!category)
  }
  if (nrow(scoped) < 10) {
    scoped <- df %>% filter(transaction_type == !!transaction_type)
  }

  prices <- scoped$price[finite_positive(scoped$price)]
  q <- safe_quantile(prices, c(0.25, 0.5, 0.75))
  list(
    data = scoped,
    lower = q[[1]],
    median = q[[2]],
    upper = q[[3]],
    n = length(prices)
  )
}

# Hàm predict_prices_for_rows: dự đoán giá hàng loạt từ model bundle hiện có.
predict_prices_for_rows <- function(df, bundle) {
  if (nrow(df) == 0 || is.null(bundle)) return(numeric())
  input_rows <- prepare_prediction_for_bundle(df, bundle)

  model_vars <- all.vars(bundle$formula)
  rhs_vars <- setdiff(model_vars, "log_price")
  missing_vars <- setdiff(rhs_vars, names(input_rows))
  if (length(missing_vars) > 0) {
    for (col in missing_vars) input_rows[[col]] <- 0
  }

  model_name <- best_model_from_bundle(bundle)
  pred_log <- tryCatch({
    if (model_name %in% c("RF + XGBoost Ensemble", "Tuned RF/XGBoost Ensemble")) {
      rf_pred <- predict_log_with_model(bundle, "Random Forest", input_rows)
      xgb_pred <- predict_log_with_model(bundle, "XGBoost", input_rows)
      weight_rf <- if (identical(model_name, "Tuned RF/XGBoost Ensemble") && !is.null(bundle$ensemble_weight_rf)) {
        as.numeric(bundle$ensemble_weight_rf)
      } else {
        0.5
      }
      weight_rf * rf_pred + (1 - weight_rf) * xgb_pred
    } else {
      predict_log_with_model(bundle, model_name, input_rows)
    }
  }, error = function(e) rep(NA_real_, nrow(input_rows)))

  if (!is.null(bundle$train_log_bounds) && length(bundle$train_log_bounds) == 2) {
    pred_log <- pmin(pmax(pred_log, bundle$train_log_bounds[[1]]), bundle$train_log_bounds[[2]])
  }
  expm1(as.numeric(pred_log))
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
.app-sidebar-nav { min-width: 0; }

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
.html-widget, .js-plotly-plot, .plot-container, .svg-container {
  flex: 1 !important;
  width: 100% !important;
  max-width: 100% !important;
  min-width: 0 !important;
}
.panel-body .html-widget { min-height: 300px; }
.js-plotly-plot .plotly .main-svg { overflow: visible; }

/* Forms & Filters */
.filter-panel { display: flex; flex-direction: column; gap: 18px; }
.chart-mode { display: inline-flex; align-items: center; gap: 10px; min-height: 36px; margin: 0 0 12px; padding: 4px 12px; border: 1px solid rgba(226, 232, 240, 0.8); border-radius: 10px; background: #f8fafc; }
.chart-mode-label { color: #64748b; font-size: 12px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.06em; }
.chart-mode .form-group { margin: 0; }
.chart-mode .radio-inline { min-height: 24px; margin: 0 0 0 8px; padding-left: 24px; color: #334155; font-size: 13px; font-weight: 600; }
.chart-mode input[type='radio'] { margin-top: 4px; }
.filter-toolbar { display: grid; grid-template-columns: minmax(140px, 1fr) minmax(150px, 1.15fr) minmax(160px, 1.15fr) minmax(170px, .95fr) minmax(170px, .95fr) auto; gap: 20px; align-items: end; }
.filter-toolbar.stat-toolbar { grid-template-columns: repeat(6, minmax(135px, 1fr)); }
.filter-toolbar.stat-toolbar.compact { grid-template-columns: minmax(180px, 260px); }
.stat-toolbar .chart-mode { margin: 0; }
.filter-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 16px; }
.filter-field { min-width: 0; }
.filter-field .form-group { margin-bottom: 0; }
.filter-field > .form-group > label, .filter-field label.control-label, .predict-form label.control-label { display: none; }
.filter-field-label { display: flex; align-items: center; gap: 8px; min-height: 20px; margin-bottom: 10px; color: #475569; font-size: 12px; font-weight: 700; letter-spacing: 0.05em; text-transform: uppercase; }
.filter-field-label i { width: 16px; color: var(--primary); text-align: center; }
.filter-summary { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 12px; border: 1px solid var(--border); background: #f8fafc; border-radius: 12px; padding: 14px; }
.filter-summary.full { grid-template-columns: repeat(4, minmax(0, 1fr)); }
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

.irs--shiny { height: 58px; }
.irs--shiny .irs-line {
  top: 34px; height: 10px; border: 1px solid rgba(203, 213, 225, 0.75);
  background: linear-gradient(180deg, #f8fafc, #e2e8f0); border-radius: 999px;
  box-shadow: inset 0 1px 2px rgba(15, 23, 42, 0.08);
}
.irs--shiny .irs-bar {
  top: 34px; height: 10px; border: 0; border-radius: 999px;
  background: linear-gradient(90deg, #0ea5e9, var(--primary));
  box-shadow: 0 5px 14px rgba(0, 114, 188, 0.22);
}
.irs--shiny .irs-handle {
  top: 24px; width: 30px; height: 30px; border: 7px solid #ffffff;
  background: linear-gradient(135deg, var(--primary), #0284c7);
  box-shadow: 0 8px 18px rgba(15, 23, 42, 0.18), 0 0 0 1px rgba(0, 114, 188, 0.16);
  border-radius: 50%; cursor: grab; transition: transform 0.15s ease, box-shadow 0.15s ease;
}
.irs--shiny .irs-handle:hover, .irs--shiny .irs-handle.state_hover {
  transform: scale(1.06); box-shadow: 0 10px 24px rgba(0, 114, 188, 0.25), 0 0 0 5px rgba(14, 165, 233, 0.12);
}
.irs--shiny .irs-handle:active { cursor: grabbing; transform: scale(1.02); }
.irs--shiny .irs-grid, .irs--shiny .irs-min, .irs--shiny .irs-max { display: none; }
.irs--shiny .irs-from, .irs--shiny .irs-to { display: none !important; }
.irs--shiny .irs-single {
  top: 0; visibility: visible !important; display: block !important; max-width: calc(100% - 16px);
  overflow: hidden; text-overflow: ellipsis; white-space: nowrap; border: 0; border-radius: 999px;
  background: #0f172a; color: #ffffff; font-size: 11px; line-height: 20px; font-weight: 800;
  padding: 4px 10px; box-shadow: 0 8px 18px rgba(15, 23, 42, 0.16);
}
.irs--shiny .irs-single:before { border-top-color: #0f172a; }

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

/* Assistant */
.assistant-context { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 14px; margin-bottom: 20px; }
.assistant-context-card { min-width: 0; padding: 16px; border: 1px solid rgba(226, 232, 240, 0.8); border-radius: 12px; background: #ffffff; box-shadow: 0 8px 24px rgba(15, 23, 42, 0.04); }
.assistant-context-label { color: var(--muted); font-size: 11px; font-weight: 800; text-transform: uppercase; letter-spacing: 0.06em; }
.assistant-context-value { margin-top: 6px; color: var(--fg); font-size: 20px; line-height: 1.15; font-weight: 800; overflow-wrap: anywhere; }
.assistant-layout { display: grid; grid-template-columns: minmax(0, 1fr) 340px; gap: 24px; align-items: start; min-width: 0; }
.assistant-chat-panel { min-width: 0; overflow: hidden !important; }
.assistant-chat-panel .panel-body { padding: 0; min-height: 650px; min-width: 0; }
.assistant-chat-log { flex: 1; min-width: 0; min-height: 460px; max-height: 650px; overflow-y: auto; overflow-x: hidden; padding: 24px; display: flex; flex-direction: column; gap: 16px; background: linear-gradient(180deg, #ffffff 0%, #f8fafc 100%); }
.assistant-message { display: flex; gap: 12px; align-items: flex-start; max-width: 94%; min-width: 0; animation: assistantSlideIn .24s ease-out; }
.assistant-message.user { margin-left: auto; flex-direction: row-reverse; }
.assistant-avatar { width: 38px; height: 38px; min-width: 38px; border-radius: 12px; display: flex; align-items: center; justify-content: center; background: linear-gradient(135deg, rgba(0, 114, 188, 0.12), rgba(16, 185, 129, 0.12)); color: var(--primary); font-size: 16px; box-shadow: inset 0 0 0 1px rgba(0, 114, 188, 0.08); }
.assistant-message.user .assistant-avatar { background: #e2e8f0; color: #475569; }
.assistant-bubble { min-width: 0; max-width: 100%; overflow-wrap: anywhere; padding: 16px 18px; border: 1px solid #dbe7f3; border-radius: 14px; background: #ffffff; color: var(--fg); box-shadow: 0 10px 26px rgba(15, 23, 42, 0.06); font-size: 14px; line-height: 1.6; }
.assistant-message.user .assistant-bubble { background: linear-gradient(135deg, var(--primary), #0099d8); border-color: var(--primary); color: #ffffff; box-shadow: 0 10px 22px rgba(0, 114, 188, 0.18); }
.assistant-lead { font-size: 15px; color: #1e293b; }
.assistant-bubble p { margin: 0 0 10px; }
.assistant-bubble p:last-child { margin-bottom: 0; }
.assistant-bubble ul { margin: 8px 0 0; padding-left: 18px; }
.assistant-answer-grid { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 10px; margin: 12px 0; }
.assistant-answer-grid div { padding: 14px; border: 1px solid #dbe7f3; border-radius: 12px; background: linear-gradient(180deg, #ffffff, #f8fafc); }
.assistant-answer-grid b { display: block; color: var(--primary-dark); font-size: 18px; line-height: 1.2; word-break: break-word; }
.assistant-answer-grid span { display: block; margin-top: 4px; color: var(--muted); font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.04em; }
.assistant-insight { margin: 12px 0; padding: 14px 16px; border: 1px solid rgba(16, 185, 129, 0.22); border-radius: 12px; background: rgba(16, 185, 129, 0.06); }
.assistant-insight-title { color: #047857; font-size: 12px; font-weight: 800; text-transform: uppercase; letter-spacing: 0.06em; margin-bottom: 8px; }
.assistant-insight ul { margin: 0; padding-left: 18px; }
.assistant-insight li { margin: 4px 0; }
.assistant-listings { display: grid; gap: 10px; margin-top: 12px; }
.assistant-listing { padding: 14px; border: 1px solid #dbe7f3; border-radius: 12px; background: #ffffff; box-shadow: 0 4px 14px rgba(15, 23, 42, 0.04); transition: border-color .18s ease, transform .18s ease; }
.assistant-listing:hover { border-color: rgba(0, 114, 188, 0.3); transform: translateY(-1px); }
.assistant-listing-reason { display: inline-flex; align-items: center; min-height: 24px; padding: 0 8px; margin-bottom: 8px; border-radius: 6px; background: rgba(245, 158, 11, 0.12); color: #92400e; font-size: 11px; font-weight: 800; }
.assistant-listing-title { font-weight: 800; color: var(--fg); line-height: 1.35; }
.assistant-listing-meta { margin-top: 4px; color: var(--muted); font-size: 12px; }
.assistant-listing-stats { display: flex; flex-wrap: wrap; gap: 8px; margin: 8px 0; }
.assistant-listing-stats span { padding: 4px 8px; border-radius: 6px; background: var(--primary-alpha); color: var(--primary-dark); font-size: 12px; font-weight: 800; }
.assistant-listing a { display: inline-flex; min-height: 32px; align-items: center; padding: 0 10px; border-radius: 6px; background: var(--primary); color: #ffffff; font-size: 12px; font-weight: 800; text-decoration: none; }
.assistant-input-bar { display: grid; grid-template-columns: minmax(0, 1fr) auto; gap: 12px; padding: 18px 24px; border-top: 1px solid var(--border); background: #ffffff; border-bottom-left-radius: var(--radius); border-bottom-right-radius: var(--radius); min-width: 0; }
.assistant-input-bar .form-group { margin-bottom: 0; }
.assistant-input-bar textarea.form-control { min-height: 52px; resize: vertical; border-radius: 10px; }
.assistant-send { min-width: 98px; min-height: 52px; }
.assistant-side-panel .panel-body { gap: 14px; }
.assistant-suggestion { width: 100%; min-height: 42px; text-align: left; border: 1px solid var(--border); border-radius: 10px; background: #ffffff; color: #334155; font-weight: 700; box-shadow: 0 2px 8px rgba(15, 23, 42, 0.03); white-space: normal; }
.assistant-suggestion:hover, .assistant-suggestion:focus { border-color: rgba(0, 114, 188, 0.35); color: var(--primary); background: var(--primary-alpha); }
.assistant-note { padding: 12px; border-radius: 10px; background: #f8fafc; color: var(--muted); font-size: 13px; line-height: 1.5; border: 1px solid var(--border); }
.assistant-prediction { margin: 12px 0; padding: 18px; border-radius: 12px; border: 1px solid rgba(0, 114, 188, 0.18); background: linear-gradient(135deg, rgba(0, 114, 188, 0.08), #ffffff); color: var(--primary-dark); font-size: 28px; line-height: 1.15; font-weight: 800; text-align: center; }
.assistant-table-wrap { max-width: 100%; overflow-x: auto; margin-top: 12px; border: 1px solid var(--border); border-radius: 10px; }
.assistant-table { width: 100%; border-collapse: collapse; font-size: 13px; background: #ffffff; }
.assistant-table th, .assistant-table td { padding: 10px 12px; border-bottom: 1px solid #eef2f7; text-align: left; white-space: nowrap; }
.assistant-table th { background: #f8fafc; color: #475569; font-size: 11px; text-transform: uppercase; letter-spacing: 0.04em; }
@keyframes assistantSlideIn { from { opacity: 0; transform: translateY(6px); } to { opacity: 1; transform: translateY(0); } }

@media (max-width: 1200px) {
  .kpi-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
  .filter-toolbar { grid-template-columns: repeat(3, minmax(0, 1fr)); }
  .assistant-context { grid-template-columns: repeat(2, minmax(0, 1fr)); }
  .assistant-layout { grid-template-columns: 1fr; }
}
@media (max-width: 900px) {
  .app-shell { flex-direction: column; }
  .app-sidebar { position: relative; width: 100%; min-width: 0; height: auto; border-right: none; border-bottom: 1px solid var(--border); box-shadow: none; z-index: 100; }
  .app-brand { flex-direction: row; padding: 16px 24px; justify-content: flex-start; text-align: left; }
  .brand-logo { width: 64px; }
  .nav-section-label, .app-sidebar-footer { display: none; }
  .app-sidebar-nav { display: flex; overflow-x: auto; -webkit-overflow-scrolling: touch; padding: 12px 24px; gap: 10px; }
  .app-nav-link { margin: 0; padding: 10px 20px; border-radius: 999px; background: #f1f5f9; white-space: nowrap; min-height: 44px; display: flex; align-items: center; }
  .page-wrap { padding: 24px; }
  .kpi-value { font-size: 28px; }
  .kpi-value.text-mode { font-size: 20px; }
  .assistant-chat-panel .panel-body { min-height: 560px; }
  .assistant-chat-log { min-height: 360px; max-height: 520px; }
}
@media (max-width: 640px) {
  .kpi-grid { grid-template-columns: 1fr; }
  .filter-toolbar { grid-template-columns: 1fr; }
  .filter-grid, .filter-summary.full { grid-template-columns: 1fr; }
  .page-wrap { padding: 16px; }
  .panel-head, .panel-body { padding: 16px; }
  .panel-title { font-size: 15px; }
  .panel-subtitle { font-size: 12px; }
  .assistant-chat-panel .panel-body { padding: 0; }
  .assistant-chat-log { padding: 16px; }
  .assistant-message { max-width: 100%; width: 100%; }
  .assistant-message.user { margin-left: 0; }
  .assistant-context { grid-template-columns: 1fr; }
  .assistant-context-value { font-size: 18px; }
  .assistant-answer-grid { grid-template-columns: 1fr; }
  .assistant-input-bar { grid-template-columns: 1fr; padding: 16px; }
  .assistant-send { width: 100%; }
  .panel-body .html-widget { min-height: 280px; }
  .kpi-value { font-size: 28px; }
  .prediction-value { font-size: 36px; }
  .topbar-subtitle { display: none; }
  .app-topbar { height: auto; min-height: 68px; padding: 10px 16px; gap: 10px; }
  .topbar-title { font-size: 17px; line-height: 1.22; }
  .topbar-title-wrap { flex: 1; }
  .topbar-actions { gap: 8px; flex-shrink: 0; }
  .status-badge { width: 42px; min-width: 42px; height: 42px; padding: 0; justify-content: center; font-size: 0; }
  .status-dot { margin: 0; }
  .map-shell { min-height: 520px; }
}

/* =========================================================
   GEMINI-STYLE ASSISTANT STYLES
   ========================================================= */
.gemini-page-wrap {
  padding: 0 !important;
  max-width: none !important;
  height: calc(100vh - 73px);
  display: flex;
  flex-direction: column;
}

.gemini-wrapper {
  display: flex;
  flex-direction: column;
  flex: 1;
  position: relative;
  background: radial-gradient(circle at 50% 45%, rgba(215, 230, 255, 0.85) 0%, rgba(240, 228, 255, 0.55) 25%, rgba(255, 255, 255, 0) 60%), #ffffff;
  background-size: 140% 140%;
  animation: geminiAmbientGlow 12s ease-in-out infinite alternate;
  overflow: hidden;
}

@keyframes geminiAmbientGlow {
  0% {
    background-position: 50% 35%;
  }
  50% {
    background-position: 55% 45%;
  }
  100% {
    background-position: 45% 40%;
  }
}

.gemini-chat-container {
  flex: 1;
  overflow-y: auto;
  overflow-x: hidden;
  width: 100%;
  padding-bottom: 20px;
  scroll-behavior: smooth;
  display: flex;
  flex-direction: column;
}

/* Custom scrollbar for Gemini assistant */
.gemini-chat-container::-webkit-scrollbar,
.assistant-table-wrap::-webkit-scrollbar,
.gemini-input-bar textarea.form-control::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}

.gemini-chat-container::-webkit-scrollbar-track,
.assistant-table-wrap::-webkit-scrollbar-track,
.gemini-input-bar textarea.form-control::-webkit-scrollbar-track {
  background: transparent;
}

.gemini-chat-container::-webkit-scrollbar-thumb,
.assistant-table-wrap::-webkit-scrollbar-thumb,
.gemini-input-bar textarea.form-control::-webkit-scrollbar-thumb {
  background: rgba(0, 0, 0, 0.15);
  border-radius: 99px;
}

.gemini-chat-container::-webkit-scrollbar-thumb:hover,
.assistant-table-wrap::-webkit-scrollbar-thumb:hover,
.gemini-input-bar textarea.form-control::-webkit-scrollbar-thumb:hover {
  background: rgba(0, 0, 0, 0.25);
}

/* Shiny dynamic UI wrapper centering fix */
#gemini_chat_view {
  display: flex;
  flex-direction: column;
  flex: 1;
  width: 100%;
  min-height: 100%;
}

.gemini-welcome-view {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  flex: 1;
  min-height: 100%;
  max-width: 960px;
  width: 100%;
  margin: 0 auto;
  padding: 40px 20px;
  box-sizing: border-box;
}

.gemini-greeting {
  font-size: 44px;
  font-weight: 500;
  background: linear-gradient(74deg, #4285f4 0%, #9b51e0 25%, #e94263 50%, #a855f7 75%, #4285f4 100%);
  background-size: 200% auto;
  -webkit-background-clip: text;
  background-clip: text;
  color: transparent;
  margin-bottom: 8px;
  text-align: center;
  font-family: 'Inter', sans-serif;
  letter-spacing: -0.02em;
  line-height: 1.25;
  animation: geminiGradientFlow 8s linear infinite;
}

@keyframes geminiGradientFlow {
  0% {
    background-position: 0% center;
  }
  100% {
    background-position: 200% center;
  }
}

.gemini-sub-greeting {
  font-size: 44px;
  font-weight: 500;
  color: #c4c7c5;
  text-align: center;
  font-family: 'Inter', sans-serif;
  letter-spacing: -0.02em;
  margin-bottom: 12px;
  line-height: 1.25;
}

.gemini-suggest-grid {
  display: grid;
  grid-template-columns: repeat(4, 1fr);
  gap: 16px;
  width: 100%;
  margin: 48px auto 20px;
}

@media (max-width: 992px) {
  .gemini-suggest-grid {
    grid-template-columns: repeat(2, 1fr);
    margin: 32px auto 20px;
  }
  .gemini-greeting, .gemini-sub-greeting {
    font-size: 32px;
  }
}

@media (max-width: 576px) {
  .gemini-suggest-grid {
    grid-template-columns: 1fr;
  }
}

.gemini-suggest-card {
  background: #f0f4f9;
  border-radius: 16px;
  padding: 20px;
  position: relative;
  min-height: 140px;
  display: flex;
  flex-direction: column;
  justify-content: flex-start;
  text-decoration: none !important;
  color: #1f1f1f !important;
  border: none;
  transition: background-color 0.2s, transform 0.2s;
  cursor: pointer;
  text-align: left;
}

.gemini-suggest-card:hover {
  background: #e3e8f0;
  transform: translateY(-2px);
}

.gemini-suggest-card-title {
  font-size: 14px;
  font-weight: 700;
  color: #1f1f1f;
  margin-bottom: 6px;
}

.gemini-suggest-card-desc {
  font-size: 12px;
  color: #5f6368;
  line-height: 1.4;
  margin-bottom: 24px;
}

.gemini-suggest-card-icon {
  position: absolute;
  bottom: 16px;
  right: 16px;
  width: 32px;
  height: 32px;
  background: #ffffff;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  color: var(--primary);
  font-size: 14px;
  box-shadow: 0 2px 6px rgba(0,0,0,0.06);
}

.gemini-chat-log {
  max-width: 960px;
  width: 100%;
  margin: 0 auto;
  padding: 32px 20px 20px;
  display: flex;
  flex-direction: column;
  gap: 32px;
}

.gemini-message {
  display: flex;
  width: 100%;
  animation: assistantSlideIn .24s ease-out;
  align-items: flex-start;
}

.gemini-avatar {
  width: 32px;
  height: 32px;
  min-width: 32px;
  display: flex;
  align-items: center;
  justify-content: center;
  margin-right: 16px;
  margin-top: 2px;
}

.gemini-star-icon {
  width: 24px;
  height: 24px;
}

/* Typing animation for bot messages is dynamically handled via JS streamReveal */

/* Loading dots indicator */
.gemini-loading {
  display: flex;
  gap: 6px;
  padding: 16px 0;
}
.gemini-loading-dot {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: #dadce0;
  animation: geminiDots 1.4s infinite both;
}
.gemini-loading-dot:nth-child(2) { animation-delay: 0.2s; }
.gemini-loading-dot:nth-child(3) { animation-delay: 0.4s; }
@keyframes geminiDots {
  0%, 80%, 100% { transform: scale(0.6); opacity: 0.4; }
  40% { transform: scale(1); opacity: 1; }
}

.gemini-message.user {
  justify-content: flex-end;
}

.gemini-message.user .gemini-bubble {
  background: #f0f4f9;
  color: #1f1f1f;
  border-radius: 20px;
  padding: 12px 20px;
  max-width: 70%;
  font-size: 15px;
  line-height: 1.5;
  box-shadow: 0 1px 2px rgba(0,0,0,0.02);
  word-break: break-word;
}

.gemini-message.bot {
  justify-content: flex-start;
}

.gemini-bot-container {
  display: flex;
  flex-direction: column;
  width: 100%;
  max-width: 920px;
}

.gemini-text {
  font-size: 15px;
  line-height: 1.6;
  color: #1f1f1f;
  word-break: break-word;
}

.gemini-text p {
  margin-bottom: 12px;
}

.gemini-text p:last-child {
  margin-bottom: 0;
}

.gemini-text ul, .gemini-text ol {
  margin-top: 8px;
  margin-bottom: 12px;
  padding-left: 24px;
}

.gemini-text li {
  margin-bottom: 6px;
}

/* ====== Gemini-styled assistant response components ====== */

/* Lead paragraph */
.gemini-text .assistant-lead {
  font-size: 15px;
  color: #1f1f1f;
  line-height: 1.6;
}

/* Answer stats grid (3 KPI cards) */
.gemini-text .assistant-answer-grid {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 16px;
  margin: 20px 0;
}
@media (max-width: 600px) {
  .gemini-text .assistant-answer-grid {
    grid-template-columns: 1fr;
  }
}
.gemini-text .assistant-answer-grid div {
  padding: 16px 20px;
  border-radius: 16px;
  background: linear-gradient(135deg, #f0f4f9 0%, #e8edf5 100%);
  border: 1px solid rgba(0, 114, 188, 0.04);
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.02);
  transition: transform 0.2s, box-shadow 0.2s;
}
.gemini-text .assistant-answer-grid div:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 12px rgba(66, 133, 244, 0.08);
}
.gemini-text .assistant-answer-grid b {
  display: block;
  color: #1a73e8;
  font-size: 24px;
  font-weight: 800;
  line-height: 1.2;
  margin-bottom: 4px;
  letter-spacing: -0.01em;
}
.gemini-text .assistant-answer-grid span {
  display: block;
  color: #5f6368;
  font-size: 11px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  margin-top: 4px;
}

/* Insight block (green tips box) */
.gemini-text .assistant-insight {
  margin: 24px 0;
  padding: 20px;
  border-radius: 16px;
  background: linear-gradient(135deg, rgba(52, 168, 83, 0.06) 0%, rgba(52, 168, 83, 0.02) 100%);
  border: 1px solid rgba(52, 168, 83, 0.12);
  border-left: 4px solid #34a853;
  box-shadow: 0 2px 8px rgba(52, 168, 83, 0.02);
}
.gemini-text .assistant-insight-title {
  color: #137333;
  font-size: 13px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  margin-bottom: 12px;
  display: flex;
  align-items: center;
  gap: 8px;
}
.gemini-text .assistant-insight-title::before {
  content: '\f0eb';
  font-family: 'Font Awesome 6 Free';
  font-weight: 900;
  font-size: 14px;
}
.gemini-text .assistant-insight ul {
  margin: 0;
  padding-left: 20px;
}
.gemini-text .assistant-insight li {
  color: #3c4043;
  font-size: 14.5px;
  line-height: 1.6;
  margin-bottom: 8px;
}
.gemini-text .assistant-insight li:last-child {
  margin-bottom: 0;
}

/* Listing cards */
.gemini-text .assistant-listings {
  display: grid;
  grid-template-columns: repeat(2, 1fr);
  gap: 16px;
  margin: 20px 0;
}
@media (max-width: 768px) {
  .gemini-text .assistant-listings {
    grid-template-columns: 1fr;
  }
}
.gemini-text .assistant-listing {
  padding: 18px;
  border: 1px solid #e8eaed;
  border-radius: 16px;
  background: #ffffff;
  box-shadow: 0 2px 6px rgba(0, 0, 0, 0.03);
  transition: border-color 0.2s, box-shadow 0.2s, transform 0.2s;
  display: flex;
  flex-direction: column;
  justify-content: space-between;
}
.gemini-text .assistant-listing:hover {
  border-color: #a8c7fa;
  box-shadow: 0 6px 16px rgba(66, 133, 244, 0.08);
  transform: translateY(-2px);
}
.gemini-text .assistant-listing-reason {
  display: inline-flex;
  align-items: center;
  min-height: 24px;
  padding: 2px 10px;
  margin-bottom: 12px;
  border-radius: 8px;
  background: linear-gradient(135deg, rgba(251, 188, 4, 0.12), rgba(251, 188, 4, 0.06));
  color: #b06a00;
  font-size: 11px;
  font-weight: 700;
  letter-spacing: 0.02em;
  align-self: flex-start;
}
.gemini-text .assistant-listing-title {
  font-weight: 700;
  font-size: 14.5px;
  color: #1f1f1f;
  line-height: 1.45;
}
.gemini-text .assistant-listing-meta {
  margin-top: 6px;
  color: #5f6368;
  font-size: 12px;
}
.gemini-text .assistant-listing-stats {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  margin: 12px 0;
}
.gemini-text .assistant-listing-stats span {
  padding: 4px 10px;
  border-radius: 8px;
  background: rgba(26, 115, 232, 0.08);
  color: #1a73e8;
  font-size: 12px;
  font-weight: 700;
}
.gemini-text .assistant-listing a {
  display: inline-flex;
  align-items: center;
  background: transparent !important;
  padding: 0 !important;
  min-height: 0 !important;
  border-radius: 0 !important;
  color: #1a73e8;
  font-size: 13px;
  font-weight: 700;
  text-decoration: none !important;
  gap: 4px;
  margin-top: 8px;
  transition: color 0.2s;
  align-self: flex-start;
}
.gemini-text .assistant-listing a:hover {
  color: #1557b0;
}
.gemini-text .assistant-listing a::after {
  content: ' \\2192';
  transition: transform 0.2s;
}
.gemini-text .assistant-listing a:hover::after {
  transform: translateX(4px);
}

/* Prediction hero block */
.gemini-text .assistant-prediction {
  margin: 16px 0;
  padding: 24px;
  border-radius: 16px;
  background: linear-gradient(135deg, #e8f0fe 0%, #f0e6ff 100%);
  border: none;
  color: #1a73e8;
  font-size: 32px;
  line-height: 1.15;
  font-weight: 800;
  text-align: center;
  letter-spacing: -0.02em;
}

/* Comparison table */
.gemini-text .assistant-table-wrap {
  max-width: 100%;
  overflow-x: auto;
  margin: 16px 0;
  border-radius: 14px;
  border: 1px solid #e8eaed;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.04);
}
.gemini-text .assistant-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 13px;
  background: #ffffff;
}
.gemini-text .assistant-table th {
  padding: 12px 14px;
  background: #f8f9fa;
  color: #5f6368;
  font-size: 11px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.04em;
  border-bottom: 1px solid #e8eaed;
  text-align: left;
  white-space: nowrap;
}
.gemini-text .assistant-table td {
  padding: 12px 14px;
  border-bottom: 1px solid #f1f3f4;
  color: #3c4043;
  font-size: 13px;
  white-space: nowrap;
}
.gemini-text .assistant-table tbody tr:last-child td {
  border-bottom: none;
}
.gemini-text .assistant-table tbody tr:hover td {
  background: rgba(66, 133, 244, 0.04);
}

.gemini-msg-actions {
  display: flex;
  align-items: center;
  gap: 8px;
  margin-top: 12px;
}

.gemini-action-btn {
  background: transparent;
  border: none;
  color: #70757a;
  cursor: pointer;
  padding: 6px;
  font-size: 13px;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  transition: background-color 0.2s, color 0.2s;
  width: 28px;
  height: 28px;
}

.gemini-action-btn:hover {
  background-color: rgba(0, 0, 0, 0.05);
  color: #1f1f1f;
}

.gemini-input-container-wrap {
  max-width: 960px;
  width: 100%;
  margin: 0 auto;
  padding: 0 20px;
  position: relative;
  z-index: 10;
}

.gemini-input-bar {
  display: flex;
  align-items: center;
  background: #f0f4f9;
  border-radius: 32px;
  padding: 8px 16px;
  transition: background-color 0.2s, box-shadow 0.2s;
  min-height: 56px;
}

.gemini-input-bar:focus-within {
  background: #ffffff;
  box-shadow: 0 2px 12px rgba(0, 0, 0, 0.08), 0 0 0 1px rgba(66, 133, 244, 0.08);
}

.gemini-input-btn {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 36px;
  height: 36px;
  border-radius: 50%;
  color: #444746;
  text-decoration: none !important;
  font-size: 16px;
  cursor: pointer;
  background: transparent;
  border: none;
  transition: background-color 0.2s;
}

.gemini-input-btn:hover {
  background: rgba(68, 71, 70, 0.08);
  color: #1f1f1f;
}

.gemini-input-bar .shiny-input-container {
  flex: 1;
  margin-bottom: 0 !important;
  padding: 0 8px;
}

.gemini-input-bar textarea.form-control {
  border: none !important;
  background: transparent !important;
  box-shadow: none !important;
  resize: none;
  padding: 6px 0 !important;
  font-size: 15px;
  color: #1f1f1f;
  min-height: 36px;
  max-height: 120px;
  width: 100%;
  line-height: 1.5;
}

.gemini-input-bar textarea.form-control::placeholder {
  color: #5f6368;
}

.gemini-input-right {
  display: flex;
  align-items: center;
  gap: 6px;
}

.gemini-model-badge {
  display: flex;
  align-items: center;
  gap: 4px;
  padding: 6px 12px;
  border-radius: 16px;
  background: rgba(0, 0, 0, 0.04);
  color: #444746;
  font-size: 12px;
  font-weight: 500;
  cursor: pointer;
  user-select: none;
}

.gemini-model-badge:hover {
  background: rgba(0, 0, 0, 0.08);
}

.gemini-btn-send {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 36px;
  height: 36px;
  border-radius: 50%;
  background: #1a73e8;
  color: #ffffff !important;
  font-size: 16px;
  cursor: pointer;
  border: none;
  transition: background-color 0.2s, transform 0.2s;
}

.gemini-btn-send:hover {
  background: #1557b0;
  transform: scale(1.05);
}

.gemini-btn-send:active {
  transform: scale(0.95);
}

.gemini-footer-note {
  max-width: 960px;
  width: 100%;
  margin: 8px auto 16px;
  padding: 0 20px;
  font-size: 11px;
  color: #70757a;
  text-align: center;
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 12px;
}

.gemini-clear-link {
  color: #d93025;
  text-decoration: none !important;
  font-weight: 600;
  display: inline-flex;
  align-items: center;
  gap: 4px;
}
.gemini-clear-link:hover {
  text-decoration: underline !important;
}

/* Voice recording / speaking CSS styles */
.gemini-input-btn.listening {
  background: rgba(234, 67, 53, 0.1) !important;
  color: #ea4335 !important;
}

@keyframes geminiMicPulse {
  0% { transform: scale(0.9); opacity: 0.6; }
  50% { transform: scale(1.15); opacity: 1; }
  100% { transform: scale(0.9); opacity: 0.6; }
}

.gemini-action-btn.speak-btn.speaking {
  color: #1a73e8 !important;
  animation: geminiSpeakerPulse 1.2s infinite ease-in-out;
}

@keyframes geminiSpeakerPulse {
  0% { transform: scale(1); }
  50% { transform: scale(1.15); }
  100% { transform: scale(1); }
}
")

# Hàm nav_link: tạo thành phần giao diện.
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
      function resizeDashboardWidgets() {
        setTimeout(function() {
          window.dispatchEvent(new Event('resize'));
          if (window.Plotly) {
            $('.js-plotly-plot').each(function() {
              Plotly.Plots.resize(this);
            });
          }
        }, 120);
      }
      $(document).on('click', '.app-nav-link', resizeDashboardWidgets);
      $(document).on('shiny:value', resizeDashboardWidgets);
      $(window).on('resize', resizeDashboardWidgets);
      function scrollAssistantChat() {
        setTimeout(function() {
          var logs = document.querySelectorAll('.gemini-chat-container');
          logs.forEach(function(log) { log.scrollTop = log.scrollHeight; });
        }, 80);
      }
      function streamReveal(element) {
        var $el = $(element);
        var children = $el.find('p, li, .assistant-answer-grid > div, .assistant-listing, .assistant-insight, table tr');
        if (children.length === 0) {
          children = $el.children();
        }
        if (children.length === 0) {
          children = $el;
        }
        
        children.css({
          'opacity': '0',
          'transform': 'translateY(10px)',
          'transition': 'opacity 0.5s ease-out, transform 0.5s ease-out'
        });
        
        children.each(function(index) {
          var child = this;
          setTimeout(function() {
            $(child).css({
              'opacity': '1',
              'transform': 'translateY(0)'
            });
            scrollAssistantChat();
          }, index * 200);
        });
      }
      $(document).on('shiny:value', function(e) {
        if (e.name === 'gemini_chat_view' || e.name === 'assistant_chat') {
          scrollAssistantChat();
          setTimeout(function() {
            var latestBotText = $('.gemini-message.bot.typing .gemini-text').last();
            if (latestBotText.length > 0 && !latestBotText.hasClass('streamed')) {
              latestBotText.addClass('streamed');
              streamReveal(latestBotText);
            }
          }, 100);
        }
      });
      $(document).on('keydown', '#assistant_question', function(e) {
        if (e.key === 'Enter' && !e.shiftKey) {
          e.preventDefault();
          $('#assistant_send').click();
        }
      });

      // Voice Chat - Speech Recognition (STT)
      var SpeechRecognition = window.SpeechRecognition || window.webkitSpeechRecognition;
      var recognition = null;
      var isListening = false;
      var recognitionTimeout = null;

      if (SpeechRecognition) {
        recognition = new SpeechRecognition();
        recognition.lang = 'vi-VN';
        recognition.interimResults = true;
        recognition.continuous = true;

        recognition.onstart = function() {
          isListening = true;
          $('#assistant_mic').addClass('listening').html('<i class=\"fa fa-circle-dot\" style=\"color: #ea4335; animation: geminiMicPulse 1s infinite;\"></i>');
        };

        recognition.onresult = function(event) {
          clearTimeout(recognitionTimeout);
          var finalTranscript = '';
          for (var i = event.resultIndex; i < event.results.length; ++i) {
            if (event.results[i].isFinal) {
              finalTranscript += event.results[i][0].transcript;
            } else {
              // Show interim results if user wants live feedback
              var interim = event.results[i][0].transcript;
              if (interim !== '') {
                $('#assistant_question').val(interim);
              }
            }
          }
          if (finalTranscript !== '') {
            $('#assistant_question').val(finalTranscript).trigger('change');
            
            // Auto stop after 2s of silence
            recognitionTimeout = setTimeout(function() {
              recognition.stop();
            }, 2000);
          }
        };

        recognition.onerror = function(event) {
          console.error('Speech recognition error:', event.error);
          recognition.stop();
        };

        recognition.onend = function() {
          isListening = false;
          $('#assistant_mic').removeClass('listening').html('<i class=\"fa fa-microphone\"></i>');
          clearTimeout(recognitionTimeout);
        };
      }

      $(document).on('click', '#assistant_mic', function(e) {
        e.preventDefault();
        if (!recognition) {
          alert('Trình duyệt của bạn không hỗ trợ nhận diện giọng nói. Hãy dùng Google Chrome hoặc Safari.');
          return;
        }
        if (isListening) {
          recognition.stop();
        } else {
          $('#assistant_question').val('').trigger('change');
          recognition.start();
        }
      });

      // Voice Chat - Speech Synthesis (TTS)
      window.speakText = function(btn) {
        var $btn = $(btn);
        var botContainer = $btn.closest('.gemini-bot-container');
        // Extract raw text without the actions panel HTML
        var textToSpeak = botContainer.find('.gemini-text').text().trim();
        
        if (window.speechSynthesis.speaking && $btn.hasClass('speaking')) {
          window.speechSynthesis.cancel();
          $btn.removeClass('speaking');
          return;
        }
        
        window.speechSynthesis.cancel();
        $('.speak-btn').removeClass('speaking');
        
        var utterance = new SpeechSynthesisUtterance(textToSpeak);
        utterance.lang = 'vi-VN';
        
        var voices = window.speechSynthesis.getVoices();
        var viVoice = voices.find(function(v) { return v.lang.indexOf('vi') > -1; });
        if (viVoice) utterance.voice = viVoice;
        
        $btn.addClass('speaking');
        
        utterance.onend = function() {
          $btn.removeClass('speaking');
        };
        utterance.onerror = function() {
          $btn.removeClass('speaking');
        };
        
        window.speechSynthesis.speak(utterance);
      };

      if (window.speechSynthesis) {
        window.speechSynthesis.getVoices();
        if (window.speechSynthesis.onvoiceschanged !== undefined) {
          window.speechSynthesis.onvoiceschanged = function() {
            window.speechSynthesis.getVoices();
          };
        }
      }
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
          span(class = "status-badge", span(class = "status-dot"), "Snapshot dữ liệu")
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
          # heatmap khu vuc x loai BDS, xu huong thoi gian va correlation.
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
                column(6, app_panel("Cơ cấu nguồn dữ liệu", "Sunburst nguồn - giao dịch - loại BĐS", chart_mode_control("source_sunburst_tx"), plotlyOutput("source_sunburst_plot", height = 390)))
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
          # conditional probability, CLT, bootstrap CI va kiem dinh gia thuyet.
          # ======================================================
          tabPanel(
            title = "statistics", value = "statistics",
            div(
              class = "page-wrap",
              h1(class = "page-title", "Suy luận thống kê"),
              div(class = "page-subtitle", "Áp dụng xác suất, phân phối mẫu, kiểm định giả thuyết, CLT và bootstrap trực tiếp trên dữ liệu BĐS TP.HCM."),
              app_panel(
                "Thiết lập phân tích",
                NULL,
                div(
                  class = "filter-toolbar stat-toolbar",
                  filter_field("Giao dịch", selectInput("stat_transaction", NULL, choices = c("Bán", "Cho thuê"), selected = "Bán", selectize = FALSE), icon_name = "tags"),
                  filter_field("Loại BĐS", uiOutput("stat_category_filter"), icon_name = "building"),
                  filter_field("Khu vực A", uiOutput("stat_district_a_filter"), icon_name = "location-dot"),
                  filter_field("Khu vực B", uiOutput("stat_district_b_filter"), icon_name = "code-compare"),
                  filter_field("Cỡ mẫu CLT", sliderInput("stat_sample_size", NULL, min = 10, max = 300, value = 50, step = 10, ticks = FALSE), icon_name = "dice"),
                  filter_field("Số lần lặp", sliderInput("stat_reps", NULL, min = 200, max = 1500, value = 600, step = 100, ticks = FALSE), icon_name = "rotate")
                ),
                div(
                  class = "filter-toolbar stat-toolbar compact",
                  filter_field("Mức tin cậy", selectInput("stat_confidence", NULL, choices = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99), selected = 0.95, selectize = FALSE), icon_name = "shield-halved")
                ),
                class = "filter-card"
              ),
              uiOutput("stat_kpi_cards"),
              fluidRow(
                column(6, app_panel("Xác suất có điều kiện", "P(loại BĐS | khu vực) theo số tin", plotlyOutput("probability_heatmap", height = 390))),
                column(6, app_panel("Phân phối giá/m²", "ECDF giữa hai khu vực được chọn", plotlyOutput("stat_distribution_plot", height = 390)))
              ),
              fluidRow(
                column(6, app_panel("CLT simulation", "Phân phối trung bình mẫu khi lấy mẫu có hoàn lại", plotlyOutput("clt_plot", height = 360))),
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
              
              # Chat log and welcome message container
              div(
                class = "gemini-chat-container",
                uiOutput("gemini_chat_view")
              ),
              
              # Sticky floating input container
              div(
                class = "gemini-input-container-wrap",
                div(
                  class = "gemini-input-bar",
                  # Add attachment icon (decorative)
                  actionLink("assistant_add", label = icon("plus"), class = "gemini-input-btn"),
                  
                  # Textarea input
                  textAreaInput(
                    "assistant_question",
                    NULL,
                    value = "",
                    rows = 1,
                    placeholder = "Hỏi trợ lý BDS..."
                  ),
                  
                  # Controls on the right
                  div(
                    class = "gemini-input-right",
                    # Model selector dropdown badge (decorative)
                    div(
                      class = "gemini-model-badge",
                      span("R-Tools"),
                      icon("chevron-down")
                    ),
                    # Microphone icon (decorative)
                    actionLink("assistant_mic", label = icon("microphone"), class = "gemini-input-btn"),
                    # Send button (rounded circle with arrow up)
                    actionButton(
                      "assistant_send",
                      label = icon("arrow-up"),
                      class = "gemini-btn-send"
                    )
                  )
                )
              ),
              
              # Minimal footer note and clear chat option
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

# Hàm server: hỗ trợ xử lý dữ liệu trong script.
server <- function(input, output, session) {
  observeEvent(input$nav_overview, updateTabsetPanel(session, "tabs", selected = "overview"), ignoreInit = TRUE)
  observeEvent(input$nav_map, updateTabsetPanel(session, "tabs", selected = "map"), ignoreInit = TRUE)
  observeEvent(input$nav_analysis, updateTabsetPanel(session, "tabs", selected = "analysis"), ignoreInit = TRUE)
  observeEvent(input$nav_statistics, updateTabsetPanel(session, "tabs", selected = "statistics"), ignoreInit = TRUE)
  observeEvent(input$nav_predict, updateTabsetPanel(session, "tabs", selected = "predict"), ignoreInit = TRUE)
  observeEvent(input$nav_diagnostics, updateTabsetPanel(session, "tabs", selected = "diagnostics"), ignoreInit = TRUE)
  observeEvent(input$nav_clusters, updateTabsetPanel(session, "tabs", selected = "clusters"), ignoreInit = TRUE)
  observeEvent(input$nav_data, updateTabsetPanel(session, "tabs", selected = "data"), ignoreInit = TRUE)
  observeEvent(input$nav_assistant, updateTabsetPanel(session, "tabs", selected = "assistant"), ignoreInit = TRUE)

  listings <- reactive({
    load_data()
  })

  data_bounds <- reactive({
    df <- listings()
    list(
      price_max = nice_slider_max(df$price_b, step = 10, fallback = 10),
      area_max = nice_slider_max(df$area, step = 100, fallback = 1000)
    )
  })

  observe({
    bounds <- data_bounds()
    updateSliderInput(session, "price_range", max = bounds$price_max, value = c(0, bounds$price_max))
    updateSliderInput(session, "map_price_range", max = bounds$price_max, value = c(0, bounds$price_max))
    updateSliderInput(session, "data_price_range", max = bounds$price_max, value = c(0, bounds$price_max))
    updateSliderInput(session, "area_range", max = bounds$area_max, value = c(0, bounds$area_max))
    updateSliderInput(session, "map_area_range", max = bounds$area_max, value = c(0, bounds$area_max))
  })

  metrics <- reactive({
    load_metrics()
  })

  registry <- reactive({
    load_registry()
  })

  source_choices <- reactive(choice_values(listings()$source))
  transaction_choices <- reactive(choice_values(listings()$transaction_type))
  district_choices <- reactive(choice_values(listings()$district_name))
  category_choices <- reactive(choice_values(listings()$category_name))

  assistant_messages <- reactiveVal(list())
  assistant_context <- reactiveVal(assistant_empty_context())
  # Track the index of the last message to apply typing animation only to latest bot msg
  assistant_msg_count <- reactiveVal(0L)

  # Dynamic view switcher for Gemini UI
  output$gemini_chat_view <- renderUI({
    messages <- assistant_messages()
    if (length(messages) == 0) {
      # Welcome / Empty state
      div(
        class = "gemini-welcome-view",
        div(class = "gemini-greeting", "Xin chào, mình là BDS"),
        div(class = "gemini-sub-greeting", "Mình có thể giúp gì cho bạn hôm nay?"),
        div(
          class = "gemini-suggest-grid",
          # Card 1
          actionLink(
            "assistant_sample_1",
            class = "gemini-suggest-card",
            tagList(
              div(class = "gemini-suggest-card-title", "4 tỷ mua ở đâu?"),
              div(class = "gemini-suggest-card-desc", "Gợi ý khu vực phù hợp cho căn hộ quanh 60m²"),
              div(class = "gemini-suggest-card-icon", icon("chart-line"))
            )
          ),
          # Card 2
          actionLink(
            "assistant_sample_2",
            class = "gemini-suggest-card",
            tagList(
              div(class = "gemini-suggest-card-title", "So sánh Thủ Đức & Q7"),
              div(class = "gemini-suggest-card-desc", "So sánh giá bán, cho thuê giữa 2 khu vực"),
              div(class = "gemini-suggest-card-icon", icon("code-compare"))
            )
          ),
          # Card 3
          actionLink(
            "assistant_sample_3",
            class = "gemini-suggest-card",
            tagList(
              div(class = "gemini-suggest-card-title", "Tìm deal giá tốt"),
              div(class = "gemini-suggest-card-desc", "Lọc tin thấp hơn mặt bằng trong cùng khu vực"),
              div(class = "gemini-suggest-card-icon", icon("house-circle-check"))
            )
          ),
          # Card 4
          actionLink(
            "assistant_sample_4",
            class = "gemini-suggest-card",
            tagList(
              div(class = "gemini-suggest-card-title", "Dự đoán căn hộ 70m²"),
              div(class = "gemini-suggest-card-desc", "Dự toán giá căn hộ chung cư 70m² tại Thủ Đức"),
              div(class = "gemini-suggest-card-icon", icon("wand-magic-sparkles"))
            )
          )
        )
      )
    } else {
      # Active chat view showing logs
      div(
        class = "gemini-chat-log",
        uiOutput("assistant_chat")
      )
    }
  })

  # Hàm append_assistant_message: lưu hoặc cập nhật dữ liệu đầu ra.
  append_assistant_message <- function(role, html) {
    assistant_messages(append(assistant_messages(), list(assistant_message(role, html))))
  }

  # Hàm run_assistant_question: chạy toàn bộ bước xử lý chính.
  run_assistant_question <- function(question) {
    question <- trimws(as.character(question %||% ""))
    if (!nzchar(question)) return(invisible(FALSE))

    append_assistant_message("user", htmltools::htmlEscape(question))
    answer_bundle <- tryCatch(
      assistant_answer_bundle(question, listings(), assistant_context()),
      error = function(e) paste0(
        "<p>Mình gặp lỗi khi xử lý câu hỏi này: <b>",
        htmltools::htmlEscape(conditionMessage(e)),
        "</b></p>"
      )
    )
    if (is.list(answer_bundle) && !is.null(answer_bundle$html)) {
      answer <- answer_bundle$html
      assistant_context(answer_bundle$context %||% assistant_empty_context())
    } else {
      answer <- answer_bundle
    }
    append_assistant_message("assistant", answer)
    updateTextAreaInput(session, "assistant_question", value = "")
    invisible(TRUE)
  }

  output$assistant_chat <- renderUI({
    messages <- assistant_messages()
    n <- length(messages)
    if (n == 0) return(NULL)

    tagList(lapply(seq_along(messages), function(i) {
      message <- messages[[i]]
      is_user <- identical(message$role, "user")
      is_last_bot <- !is_user && (i == n)
      if (is_user) {
        div(
          class = "gemini-message user",
          div(class = "gemini-bubble", HTML(message$html))
        )
      } else {
        div(
          class = paste("gemini-message bot", if (is_last_bot) "typing" else ""),
          div(
            class = "gemini-avatar",
            gemini_star_svg()
          ),
          div(
            class = "gemini-bot-container",
            div(class = "gemini-text", HTML(message$html)),
            div(
              class = "gemini-msg-actions",
              tags$button(class = "gemini-action-btn speak-btn", icon("volume-high"), onclick = "speakText(this)"),
              tags$button(class = "gemini-action-btn", icon("thumbs-up")),
              tags$button(class = "gemini-action-btn", icon("thumbs-down")),
              tags$button(class = "gemini-action-btn", icon("rotate")),
              tags$button(class = "gemini-action-btn", icon("copy")),
              tags$button(class = "gemini-action-btn", icon("ellipsis"))
            )
          )
        )
      }
    }))
  })

  observeEvent(input$assistant_send, {
    run_assistant_question(input$assistant_question)
  }, ignoreInit = TRUE)

  observeEvent(input$assistant_sample_1, {
    run_assistant_question("4 tỷ mua căn hộ tầm 60m2 ở khu nào ổn?")
  }, ignoreInit = TRUE)

  observeEvent(input$assistant_sample_2, {
    run_assistant_question("So sánh Thủ Đức với Quận 7")
  }, ignoreInit = TRUE)

  observeEvent(input$assistant_sample_3, {
    run_assistant_question("Tìm tin giá tốt hơn mặt bằng ở Bình Tân dưới 4 tỷ")
  }, ignoreInit = TRUE)

  observeEvent(input$assistant_sample_4, {
    run_assistant_question("Dự đoán căn hộ 70m2 ở Thủ Đức")
  }, ignoreInit = TRUE)

  observeEvent(input$assistant_clear, {
    assistant_messages(list())
    assistant_context(assistant_empty_context())
  }, ignoreInit = TRUE)

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
    bounds <- data_bounds()
    price_range <- safe_range(input$price_range, c(0, bounds$price_max))
    area_range <- safe_range(input$area_range, c(0, bounds$area_max))
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
    bounds <- data_bounds()
    price_range <- safe_range(input$map_price_range, c(0, bounds$price_max))
    area_range <- safe_range(input$map_area_range, c(0, bounds$area_max))
    df %>%
      filter(
        price_b >= price_range[1], price_b <= price_range[2],
        area >= area_range[1], area <= area_range[2],
        !is.na(map_lat), !is.na(map_lon)
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
    bounds <- data_bounds()
    price_range <- safe_range(input$data_price_range, c(0, bounds$price_max))
    df %>%
      filter(price_b >= price_range[1], price_b <= price_range[2])
  })

  # Hàm chart_transaction: tạo thành phần giao diện.
  chart_transaction <- function(input_id) {
    value <- input[[input_id]]
    if (is.null(value) || length(value) == 0 || !(value %in% c("Bán", "Cho thuê"))) "Bán" else value[[1]]
  }

  # Hàm overview_chart_data: tạo thành phần giao diện.
  overview_chart_data <- function(input_id) {
    tx <- chart_transaction(input_id)
    listings() %>% filter(transaction_type == tx)
  }

  # ------------------------------------------------------------
  # SERVER DATA SCOPE - PHAN TICH GIA
  # Ham nay gom tat ca filter cua tab "Phan tich gia" de cac chart EDA
  # dung cung mot tap du lieu: scatter dien tich-gia, gia/m2, heatmap,
  # time trend, ECDF va correlation.
  # ------------------------------------------------------------
  # Hàm analysis_chart_data: tạo thành phần giao diện.
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

    bounds <- data_bounds()
    price_range <- safe_range(input$price_range, c(0, bounds$price_max))
    area_range <- safe_range(input$area_range, c(0, bounds$area_max))
    df %>%
      filter(
        price_b >= price_range[1], price_b <= price_range[2],
        area >= area_range[1], area <= area_range[2]
      )
  }

  # ------------------------------------------------------------
  # SERVER DATA SCOPE - SUY LUAN THONG KE
  # stat_base_data() la mau thong ke sau khi loc giao dich/loai BDS.
  # Cac output xac suat, CLT, bootstrap va hypothesis test deu dung scope nay
  # de tranh moi bieu do tinh tren mot tap du lieu khac nhau.
  # ------------------------------------------------------------
  stat_base_data <- reactive({
    tx <- input$stat_transaction %||% "Bán"
    df <- listings() %>%
      filter(transaction_type == tx, finite_positive(price_per_m2), finite_positive(price))

    if (is_selected_filter(input$stat_category)) {
      df <- df %>% filter(category_name %in% input$stat_category)
    }

    known_rows_or_all(df, "district_name")
  })

  stat_district_choices <- reactive({
    choices <- stat_base_data() %>%
      count(district_name, sort = TRUE) %>%
      filter(!is_missing_label(district_name)) %>%
      pull(district_name)
    if (length(choices) == 0) choices <- district_choices()
    unique(choices)
  })

  stat_selected_district_a <- reactive({
    choices <- stat_district_choices()
    if (length(choices) == 0) return(NA_character_)
    value <- input$stat_district_a
    if (!is.null(value) && length(value) > 0 && value[[1]] %in% choices) value[[1]] else choices[[1]]
  })

  stat_selected_district_b <- reactive({
    choices <- stat_district_choices()
    if (length(choices) == 0) return(NA_character_)
    value <- input$stat_district_b
    if (!is.null(value) && length(value) > 0 && value[[1]] %in% choices) {
      return(value[[1]])
    }
    if (length(choices) >= 2) choices[[2]] else choices[[1]]
  })

  diagnostic_data <- reactive({
    tx <- chart_transaction("diagnostic_tx")
    is_rent_diag <- identical(tx, "Cho thuê")
    model_path <- if (is_rent_diag) RENT_MODEL_PATH else SALE_MODEL_PATH
    if (!file.exists(model_path)) return(tibble())

    df <- listings() %>%
      filter(is_rent == !!is_rent_diag, finite_positive(price), finite_positive(price_per_m2))
    if (nrow(df) == 0) return(tibble())

    set.seed(2026)
    sample_n <- min(900, nrow(df))
    if (nrow(df) > sample_n) {
      df <- df[sample(seq_len(nrow(df)), sample_n), , drop = FALSE]
    }

    bundle <- readRDS(model_path)
    predicted <- predict_prices_for_rows(df, bundle)
    df %>%
      mutate(
        predicted_price = predicted,
        actual_price = price,
        residual_log = log1p(actual_price) - log1p(predicted_price),
        ape = abs(actual_price - predicted_price) / actual_price,
        model_name = model_label_vi(best_model_from_bundle(bundle))
      ) %>%
      filter(finite_positive(predicted_price), is.finite(residual_log), is.finite(ape))
  })

  output$kpi_cards <- renderUI({
    df <- listings()
    m <- metrics()
    div(
      class = "kpi-grid",
      kpi_card("Tin đăng đã thu thập", format(nrow(df), big.mark = ","), "sau làm sạch", "database", "default"),
      kpi_card("Giá trung vị", format_vnd(median(df$price, na.rm = TRUE)), "toàn TP.HCM", "coins", "warning"),
      kpi_card("Khu vực cũ có dữ liệu", paste0(n_distinct(df$district_name[!is_missing_label(df$district_name)]), " khu vực"), "độ phủ địa lý", "location-dot", "success"),
      kpi_card("Mô hình tốt nhất", best_model_name_only(m), "chọn theo MAPE/RMSE", "bullseye", "success", delta = best_model_mape_only(m), value_class = "text-mode")
    )
  })

  output$source_filter <- renderUI({
    filter_source_select("sources", source_choices(), "Tất cả nguồn")
  })

  output$district_filter <- renderUI({
    filter_select("districts", district_choices(), "Tất cả khu vực")
  })

  output$transaction_filter <- renderUI({
    filter_select("transactions", transaction_choices(), "Tất cả giao dịch")
  })

  output$category_filter <- renderUI({
    filter_select("categories", category_choices(), "Tất cả loại BĐS")
  })

  output$map_source_filter <- renderUI({
    filter_source_select("map_sources", source_choices(), "Tất cả nguồn")
  })

  output$map_district_filter <- renderUI({
    filter_select("map_districts", district_choices(), "Tất cả khu vực")
  })

  output$map_transaction_filter <- renderUI({
    filter_select("map_transactions", transaction_choices(), "Tất cả giao dịch")
  })

  output$map_category_filter <- renderUI({
    filter_select("map_categories", category_choices(), "Tất cả loại BĐS")
  })

  output$data_source_filter <- renderUI({
    filter_source_select("data_sources", source_choices(), "Tất cả nguồn")
  })

  output$data_district_filter <- renderUI({
    filter_select("data_districts", district_choices(), "Tất cả khu vực")
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

  output$stat_category_filter <- renderUI({
    tx <- input$stat_transaction %||% "Bán"
    choices <- listings() %>%
      filter(transaction_type == tx) %>%
      pull(category_name) %>%
      choice_values()
    selectInput(
      "stat_category",
      NULL,
      choices = c("Tất cả loại BĐS" = "__all__", setNames(choices, choices)),
      selected = "__all__",
      selectize = FALSE
    )
  })

  output$stat_district_a_filter <- renderUI({
    choices <- stat_district_choices()
    selected <- if (length(choices) > 0) choices[[1]] else ""
    selectInput("stat_district_a", NULL, choices = choices, selected = selected, selectize = FALSE)
  })

  output$stat_district_b_filter <- renderUI({
    choices <- stat_district_choices()
    selected <- if (length(choices) >= 2) choices[[2]] else if (length(choices) == 1) choices[[1]] else ""
    selectInput("stat_district_b", NULL, choices = choices, selected = selected, selectize = FALSE)
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
    exact_count <- sum(df$coord_status == "Tọa độ gốc từ nguồn", na.rm = TRUE)
    estimated_count <- sum(df$coord_status != "Tọa độ gốc từ nguồn", na.rm = TRUE)
    div(
      class = "filter-summary full",
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Marker hiển thị"),
          div(class = "filter-chip-value", format(nrow(df), big.mark = ","))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Độ chính xác vị trí"),
          div(class = "filter-chip-value", paste0(format_count_vi(exact_count), " gốc / ", format_count_vi(estimated_count), " ước lượng"))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Nguồn"),
          div(class = "filter-chip-value", active_source_or_all(input$map_sources))),
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
          div(class = "filter-chip-value", active_source_or_all(input$data_sources))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Giá trung vị"),
          div(class = "filter-chip-value", format_vnd(median(df$price, na.rm = TRUE))))
    )
  })

  observeEvent(input$reset_analysis_filters, {
    bounds <- data_bounds()
    updateSelectInput(session, "sources", selected = "__all__")
    updateSelectInput(session, "transactions", selected = "__all__")
    updateSelectInput(session, "districts", selected = "__all__")
    updateSelectInput(session, "categories", selected = "__all__")
    updateSliderInput(session, "price_range", value = c(0, bounds$price_max))
    updateSliderInput(session, "area_range", value = c(0, bounds$area_max))
  }, ignoreInit = TRUE)

  observeEvent(input$reset_map_filters, {
    bounds <- data_bounds()
    updateSelectInput(session, "map_sources", selected = "__all__")
    updateSelectInput(session, "map_transactions", selected = "__all__")
    updateSelectInput(session, "map_districts", selected = "__all__")
    updateSelectInput(session, "map_categories", selected = "__all__")
    updateSliderInput(session, "map_price_range", value = c(0, bounds$price_max))
    updateSliderInput(session, "map_area_range", value = c(0, bounds$area_max))
  }, ignoreInit = TRUE)

  observeEvent(input$reset_data_filters, {
    bounds <- data_bounds()
    updateSelectInput(session, "data_sources", selected = "__all__")
    updateSelectInput(session, "data_transactions", selected = "__all__")
    updateSelectInput(session, "data_districts", selected = "__all__")
    updateSelectInput(session, "data_categories", selected = "__all__")
    updateSliderInput(session, "data_price_range", value = c(0, bounds$price_max))
  }, ignoreInit = TRUE)

  output$district_plot <- renderPlotly({
    tx <- chart_transaction("district_plot_tx")
    df <- overview_chart_data("district_plot_tx")
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", tx, "phù hợp.")))
    p <- known_rows_or_all(df, "district_name") %>%
      count(district_name, sort = TRUE) %>%
      slice_head(n = 12) %>%
      mutate(
        tooltip = paste0("Giao dịch: ", tx, "<br>Khu vực cũ: ", district_name, "<br>Số tin: ", format_count_vi(n))
      ) %>%
      ggplot(aes(x = reorder(district_name, n), y = n, text = tooltip)) +
      geom_col(fill = "#0072bc", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "Số tin") +
      chart_theme()
    interactive_chart(p, tooltip = "text")
  })

  output$category_plot <- renderPlotly({
    tx <- chart_transaction("category_plot_tx")
    df <- overview_chart_data("category_plot_tx")
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", tx, "phù hợp.")))
    plot_df <- known_rows_or_all(df, "category_name") %>%
      count(category_name, sort = TRUE) %>%
      slice_head(n = 10) %>%
      mutate(
        tooltip = paste0("Giao dịch: ", tx, "<br>Loại BĐS: ", category_name, "<br>Số tin: ", format_count_vi(n))
      )
    fill_values <- setNames(chart_colors(nrow(plot_df)), plot_df$category_name)
    p <- plot_df %>%
      ggplot(aes(x = reorder(category_name, n), y = n, fill = category_name, text = tooltip)) +
      geom_col(width = 0.72) +
      scale_fill_manual(values = fill_values) +
      coord_flip() +
      guides(fill = "none") +
      labs(x = NULL, y = "Số tin") +
      chart_theme()
    interactive_chart(p, tooltip = "text")
  })

  output$metrics_table <- renderTable({
    m <- metrics()
    if (nrow(m) == 0) return(data.frame(Ghi_chu = paste0("Chưa có ", PATHS$metrics_csv)))
    segment_totals <- listings() %>%
      mutate(segment = if_else(transaction_type == "Cho thuê", "rent", "sale")) %>%
      count(segment, name = "total_listings")

    m %>%
      left_join(segment_totals, by = "segment") %>%
      mutate(
        total_listings = coalesce(total_listings, train_rows + test_rows),
        segment = recode(segment, sale = "Bán", rent = "Cho thuê", .default = segment),
        split_type = recode(
          split_type,
          stratified_random_by_source = "Validate 80/20 theo nguồn",
          time_based = "Validate theo thời gian",
          random_fallback = "Validate random",
          .default = split_type
        ),
        total_listings = format_count_vi(total_listings),
        train_rows = format_count_vi(train_rows),
        test_rows = format_count_vi(test_rows),
        rmse_vnd = format_vnd_full(rmse_vnd),
        mae_vnd = format_vnd_full(mae_vnd),
        mape = paste0(round(mape * 100, 1), "%"),
        r2 = format_metric(r2)
      ) %>%
      select(
        `Nhóm dữ liệu` = segment,
        `Tổng listings` = total_listings,
        `Mô hình` = model,
        `Cách đánh giá` = split_type,
        `Train validate` = train_rows,
        `Test validate` = test_rows,
        RMSE = rmse_vnd,
        MAE = mae_vnd,
        MAPE = mape,
        `R²` = r2
      )
  })

  output$listing_map <- renderLeaflet({
    df <- map_filtered()
    validate(need(nrow(df) > 0, "Không có điểm dữ liệu phù hợp bộ lọc."))
    price_cuts <- quantile(df$price, probs = c(1 / 3, 2 / 3), na.rm = TRUE)
    if (any(is.na(price_cuts)) || price_cuts[[1]] == price_cuts[[2]]) {
      price_cuts <- c(3e9, 8e9)
    }

    source_links <- listing_url(df$ad_url, df$source)
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
      "<span style='color:#64748b'>Tọa độ</span><b>", htmltools::htmlEscape(df$coord_status), "</b>",
      "</div>",
      source_link_html,
      "</div>"
    )

    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 106.70, lat = 10.78, zoom = 11) %>%
      addCircleMarkers(
        lng = ~map_lon, lat = ~map_lat,
        radius = ~ifelse(coord_status == "Tọa độ gốc từ nguồn", 5, 4),
        stroke = TRUE, weight = 1, color = "#ffffff",
        fillColor = ~price_color(price, price_cuts[[1]], price_cuts[[2]]),
        fillOpacity = ~ifelse(coord_status == "Tọa độ gốc từ nguồn", 0.82, 0.55),
        popup = popup,
        clusterOptions = markerClusterOptions()
      )
  })

  # ============================================================
  # OUTPUTS - PHAN TICH GIA / EDA
  # Nhom output nay chuyen data sach thanh insight thi truong:
  # - scatter dien tich vs gia
  # - ranking gia/m2 theo khu vuc
  # - khoang gia theo loai BDS
  # - histogram/log price distribution
  # - heatmap, sunburst, time trend, correlation va ECDF
  # ============================================================

  # EDA: quan he dien tich va gia, dung de nhin pattern phi tuyen/outlier.
  output$area_price_plot <- renderPlotly({
    tx <- chart_transaction("area_price_tx")
    price_info <- price_display_info(tx)
    df <- known_rows_or_all(analysis_chart_data("area_price_tx"), "category_name") %>%
      mutate(display_price = .data[[price_info$value_col]]) %>%
      filter(!is.na(area), area > 0, !is.na(display_price), display_price > 0)
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", tx, "phù hợp bộ lọc.")))
    price_cutoff <- quantile(df$display_price, 0.98, na.rm = TRUE)
    plot_df <- df %>%
      filter(display_price <= price_cutoff) %>%
      plot_sample(max_n = 1600) %>%
      mutate(
        tooltip = paste0(
          "Giao dịch: ", tx,
          "<br>Loại BĐS: ", category_name,
          "<br>Khu vực cũ: ", district_name,
          "<br>Diện tích: ", format_number_vi(area, 1), " m²",
          "<br>Giá: ", format_number_vi(display_price, price_info$digits), " ", price_info$unit
        )
      )
    color_values <- setNames(chart_colors(n_distinct(plot_df$category_name)), sort(unique(plot_df$category_name)))
    p <- plot_df %>%
      ggplot(aes(x = area, y = display_price, color = category_name, text = tooltip)) +
      geom_point(alpha = 0.55, size = 1.7) +
      scale_color_manual(values = color_values) +
      guides(color = "none") +
      labs(x = "Diện tích (m²)", y = price_info$axis) +
      chart_theme()
    interactive_chart(p, tooltip = "text") %>%
      layout(showlegend = FALSE, margin = list(l = 92, r = 28, t = 16, b = 78))
  })

  # EDA: top khu vuc theo median gia/m2, dung median de giam anh huong outlier.
  output$price_m2_plot <- renderPlotly({
    tx <- chart_transaction("price_m2_tx")
    m2_info <- price_m2_display_info(tx)
    df <- known_rows_or_all(analysis_chart_data("price_m2_tx"), "district_name")
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
          "<br>Khu vực cũ: ", district_name,
          "<br>Giá trung vị/m²: ", format_number_vi(display_price_m2, m2_info$digits), " ", m2_info$unit,
          "<br>Số tin: ", format_count_vi(n)
        )
      ) %>%
      ggplot(aes(x = reorder(district_name, display_price_m2), y = display_price_m2, text = tooltip)) +
      geom_col(fill = "#059669", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = m2_info$axis) +
      chart_theme()
    interactive_chart(p, tooltip = "text")
  })

  # EDA: so sanh khoang gia theo loai BDS bang median va IQR.
  output$price_category_plot <- renderPlotly({
    selected_transaction <- chart_transaction("price_category_tx")
    price_info <- price_display_info(selected_transaction)
    df <- known_rows_or_all(analysis_chart_data("price_category_tx"), "category_name") %>%
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
      slice_max(listing_count, n = 10) %>%
      arrange(median_price) %>%
      mutate(
        category_label = factor(category_name, levels = category_name),
        tooltip = paste0(
          "Giao dịch: ", selected_transaction,
          "<br>Loại BĐS: ", category_name,
          "<br>Giá trung vị: ", format_number_vi(median_price, price_info$digits), " ", price_info$unit,
          "<br>Vùng phổ biến: ", format_number_vi(q1_price, price_info$digits), " - ", format_number_vi(q3_price, price_info$digits), " ", price_info$unit,
          "<br>Số tin: ", format_count_vi(listing_count)
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
        margin = list(l = 150, r = 20, t = 36, b = 60),
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
        xaxis = list(title = price_info$axis, automargin = TRUE, gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb"),
        yaxis = list(title = "", automargin = TRUE, categoryorder = "array", categoryarray = levels(summary_df$category_label))
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  # EDA: histogram log(price) de xu ly phan phoi gia lech phai.
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
        margin = list(l = 62, r = 18, t = 16, b = 54),
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#d7e6f5", font = list(color = "#1f2937")),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        bargap = 0.04,
        xaxis = list(title = "Mức giá chuẩn hóa", automargin = TRUE, gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb"),
        yaxis = list(title = "Số tin", automargin = TRUE, gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  # EDA nang cao: heatmap median gia/m2 theo khu vuc va loai BDS.
  output$district_category_heatmap <- renderPlotly({
    tx <- chart_transaction("district_category_heatmap_tx")
    m2_info <- price_m2_display_info(tx)
    df <- analysis_chart_data("district_category_heatmap_tx") %>%
      filter(finite_positive(price_per_m2))
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", tx, "phù hợp bộ lọc.")))

    top_districts <- known_rows_or_all(df, "district_name") %>%
      count(district_name, sort = TRUE) %>%
      slice_head(n = 10) %>%
      pull(district_name)
    top_categories <- known_rows_or_all(df, "category_name") %>%
      count(category_name, sort = TRUE) %>%
      slice_head(n = 8) %>%
      pull(category_name)

    summary_df <- df %>%
      filter(district_name %in% top_districts, category_name %in% top_categories) %>%
      group_by(district_name, category_name) %>%
      summarise(display_m2 = median(price_per_m2, na.rm = TRUE) / m2_info$scale, n = n(), .groups = "drop")

    z <- matrix(NA_real_, nrow = length(top_categories), ncol = length(top_districts), dimnames = list(top_categories, top_districts))
    for (i in seq_len(nrow(summary_df))) {
      z[summary_df$category_name[[i]], summary_df$district_name[[i]]] <- summary_df$display_m2[[i]]
    }

    plot_ly(
      x = top_districts,
      y = top_categories,
      z = z,
      type = "heatmap",
      colorscale = "Viridis",
      hovertemplate = paste0(
        "Khu vực: %{x}<br>Loại BĐS: %{y}<br>Giá/m² trung vị: %{z:.1f} ",
        m2_info$unit,
        "<extra></extra>"
      )
    ) %>%
      layout(
        margin = list(l = 158, r = 24, t = 12, b = 120),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        xaxis = list(title = "", automargin = TRUE, tickangle = -35, tickfont = list(size = 10)),
        yaxis = list(title = "", automargin = TRUE)
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  # EDA nang cao: sunburst de thay co cau nguon -> giao dich -> loai BDS.
  output$source_sunburst_plot <- renderPlotly({
    tx <- chart_transaction("source_sunburst_tx")
    df <- overview_chart_data("source_sunburst_tx") %>%
      filter(!is_missing_label(source), !is_missing_label(category_name))
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", tx, "để vẽ sunburst.")))

    source_df <- df %>% count(source, sort = TRUE)
    category_df <- df %>%
      semi_join(source_df, by = "source") %>%
      count(source, category_name, sort = TRUE)

    root_id <- paste0("tx_", assistant_text_key(tx))
    source_ids <- paste0("source_", assistant_text_key(source_df$source))
    category_ids <- paste0("category_", assistant_text_key(category_df$source), "_", seq_len(nrow(category_df)))

    plot_df <- tibble::tibble(
      ids = c(root_id, source_ids, category_ids),
      labels = c(tx, source_label_vi(source_df$source), category_df$category_name),
      parents = c("", rep(root_id, nrow(source_df)), paste0("source_", assistant_text_key(category_df$source))),
      values = c(nrow(df), source_df$n, category_df$n)
    )

    plot_ly(
      plot_df,
      type = "sunburst",
      ids = ~ids,
      labels = ~labels,
      parents = ~parents,
      values = ~values,
      branchvalues = "total",
      maxdepth = 3,
      insidetextorientation = "radial",
      hovertemplate = "%{label}<br>Số tin: %{value}<extra></extra>"
    ) %>%
      layout(
        margin = list(l = 0, r = 0, t = 10, b = 10),
        paper_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  # EDA nang cao: time trend, loai ngay dang tuong lai de khong lam sai xu huong.
  output$time_trend_plot <- renderPlotly({
    tx <- chart_transaction("time_trend_tx")
    m2_info <- price_m2_display_info(tx)
    df <- analysis_chart_data("time_trend_tx") %>%
      mutate(posted_date = as.Date(posted_at)) %>%
      filter(!is.na(posted_date), posted_date <= Sys.Date(), finite_positive(price_per_m2))
    validate(need(nrow(df) > 0, paste("Không có dữ liệu ngày hợp lệ cho", tx)))

    trend_df <- df %>%
      mutate(posted_month = lubridate::floor_date(posted_date, unit = "month")) %>%
      group_by(posted_month) %>%
      summarise(
        n = n(),
        median_m2 = median(price_per_m2, na.rm = TRUE) / m2_info$scale,
        .groups = "drop"
      ) %>%
      arrange(posted_month)

    plot_ly(trend_df, x = ~posted_month) %>%
      add_bars(
        y = ~n,
        name = "Số tin",
        marker = list(color = "rgba(0,114,188,0.25)"),
        hovertemplate = "Tháng: %{x|%m/%Y}<br>Số tin: %{y}<extra></extra>"
      ) %>%
      add_lines(
        y = ~median_m2,
        name = paste0("Giá/m² trung vị (", m2_info$unit, ")"),
        yaxis = "y2",
        line = list(color = "#ef4444", width = 3),
        hovertemplate = paste0("Tháng: %{x|%m/%Y}<br>Giá/m²: %{y:.1f} ", m2_info$unit, "<extra></extra>")
      ) %>%
      layout(
        barmode = "overlay",
        margin = list(l = 82, r = 82, t = 12, b = 96),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        legend = list(orientation = "h", x = 0, y = -0.28, font = list(size = 11), itemwidth = 30),
        xaxis = list(title = "", automargin = TRUE, gridcolor = "#e5e7eb"),
        yaxis = list(title = "Số tin", gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb"),
        yaxis2 = list(title = m2_info$unit, overlaying = "y", side = "right", showgrid = FALSE)
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  # EDA nang cao: correlation matrix tren cac bien so chinh.
  output$correlation_plot <- renderPlotly({
    tx <- chart_transaction("correlation_tx")
    df <- analysis_chart_data("correlation_tx") %>%
      mutate(
        log_price = log1p(price),
        log_area = log1p(area),
        log_price_m2 = log1p(price_per_m2)
      )
    numeric_cols <- intersect(
      c("log_price", "log_price_m2", "log_area", "rooms", "distance_to_center", "listing_age_days", "title_token_count"),
      names(df)
    )
    validate(need(length(numeric_cols) >= 2, "Chưa đủ biến số để tính tương quan."))
    cor_df <- df[, numeric_cols, drop = FALSE]
    cor_df[] <- lapply(cor_df, function(x) suppressWarnings(as.numeric(x)))
    validate(need(sum(stats::complete.cases(cor_df)) >= 20, paste("Không đủ dữ liệu số cho", tx)))

    cor_mat <- stats::cor(cor_df, use = "pairwise.complete.obs")
    labels <- feature_label_vi(colnames(cor_mat))
    plot_ly(
      x = labels,
      y = labels,
      z = cor_mat,
      type = "heatmap",
      zmin = -1,
      zmax = 1,
      colorscale = list(c(0, "#b91c1c"), c(0.5, "#ffffff"), c(1, "#0072bc")),
      hovertemplate = "Biến X: %{x}<br>Biến Y: %{y}<br>Correlation: %{z:.2f}<extra></extra>"
    ) %>%
      layout(
        margin = list(l = 150, r = 24, t = 12, b = 120),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        xaxis = list(title = "", automargin = TRUE, tickangle = -35, tickfont = list(size = 10)),
        yaxis = list(title = "", automargin = TRUE)
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  # EDA/xac suat: ECDF gia/m2 de doc percentile va so sanh phan phoi khu vuc.
  output$price_ecdf_plot <- renderPlotly({
    tx <- chart_transaction("ecdf_tx")
    m2_info <- price_m2_display_info(tx)
    df <- analysis_chart_data("ecdf_tx") %>%
      filter(finite_positive(price_per_m2)) %>%
      mutate(display_m2 = price_per_m2 / m2_info$scale)
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", tx, "để vẽ ECDF.")))

    top_districts <- known_rows_or_all(df, "district_name") %>%
      count(district_name, sort = TRUE) %>%
      slice_head(n = 5) %>%
      pull(district_name)
    plot_df <- df %>%
      filter(district_name %in% top_districts) %>%
      mutate(tooltip = paste0("Khu vực: ", district_name, "<br>Giá/m²: ", format_number_vi(display_m2, m2_info$digits), " ", m2_info$unit))

    color_values <- setNames(chart_colors(n_distinct(plot_df$district_name)), sort(unique(plot_df$district_name)))
    p <- plot_df %>%
      ggplot(aes(x = display_m2, color = district_name, text = tooltip)) +
      stat_ecdf(linewidth = 0.9) +
      scale_color_manual(values = color_values) +
      labs(x = paste0("Giá/m² (", m2_info$unit, ")"), y = "Xác suất tích lũy", color = "Khu vực") +
      chart_theme()
    interactive_chart(p, tooltip = "text")
  })

  # ============================================================
  # OUTPUTS - SUY LUAN THONG KE
  # Nhom output nay gan voi cac bai ly thuyet trong docs:
  # - Probability: empirical probability, conditional probability
  # - LoLN/CLT: sampling distribution va standard error
  # - Bootstrap: bootstrap distribution va confidence interval
  # - Hypothesis: H0/H1, p-value, ket luan theo alpha
  # ============================================================

  # Xac suat thong ke mo ta: sample size, median, standard error, P(gia cao).
  output$stat_kpi_cards <- renderUI({
    df <- stat_base_data()
    tx <- input$stat_transaction %||% "Bán"
    m2_info <- price_m2_display_info(tx)
    values <- df$price_per_m2[finite_positive(df$price_per_m2)]
    q75 <- safe_quantile(values, 0.75)
    se_log <- stats::sd(log1p(values), na.rm = TRUE) / sqrt(length(values))
    div(
      class = "kpi-grid",
      kpi_card("Cỡ mẫu thống kê", format_count_vi(nrow(df)), "sau lọc giao dịch/loại BĐS", "database", "default"),
      kpi_card("Trung vị giá/m²", paste0(format_number_vi(median(values / m2_info$scale, na.rm = TRUE), m2_info$digits), " ", m2_info$unit), "sample statistic", "chart-line", "warning"),
      kpi_card("Standard error", format_number_vi(se_log, 4), "trên log(giá/m²)", "ruler", "success"),
      kpi_card("P(giá cao)", paste0(round(mean(values >= q75[[1]], na.rm = TRUE) * 100, 1), "%"), "ngưỡng Q3 của nhóm lọc", "percent", "danger")
    )
  })

  # Probability: conditional probability P(loai BDS | khu vuc).
  output$probability_heatmap <- renderPlotly({
    df <- stat_base_data() %>%
      filter(!is_missing_label(district_name), !is_missing_label(category_name))
    validate(need(nrow(df) > 0, "Không có dữ liệu để tính xác suất có điều kiện."))

    top_districts <- df %>% count(district_name, sort = TRUE) %>% slice_head(n = 10) %>% pull(district_name)
    top_categories <- df %>% count(category_name, sort = TRUE) %>% slice_head(n = 8) %>% pull(category_name)
    prob_df <- df %>%
      filter(district_name %in% top_districts, category_name %in% top_categories) %>%
      count(district_name, category_name) %>%
      group_by(district_name) %>%
      mutate(prob = n / sum(n)) %>%
      ungroup()

    z <- matrix(0, nrow = length(top_categories), ncol = length(top_districts), dimnames = list(top_categories, top_districts))
    for (i in seq_len(nrow(prob_df))) {
      z[prob_df$category_name[[i]], prob_df$district_name[[i]]] <- prob_df$prob[[i]]
    }

    plot_ly(
      x = top_districts,
      y = top_categories,
      z = z,
      type = "heatmap",
      colorscale = "Blues",
      hovertemplate = "Khu vực: %{x}<br>Loại BĐS: %{y}<br>P(loại | khu vực): %{z:.1%}<extra></extra>"
    ) %>%
      layout(
        margin = list(l = 158, r = 24, t = 12, b = 120),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        xaxis = list(title = "", automargin = TRUE, tickangle = -35, tickfont = list(size = 10)),
        yaxis = list(title = "", automargin = TRUE)
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  # Probability distribution: ECDF de so sanh phan phoi gia/m2 giua hai khu vuc.
  output$stat_distribution_plot <- renderPlotly({
    tx <- input$stat_transaction %||% "Bán"
    m2_info <- price_m2_display_info(tx)
    districts <- unique(c(stat_selected_district_a(), stat_selected_district_b()))
    df <- stat_base_data() %>%
      filter(district_name %in% districts, finite_positive(price_per_m2)) %>%
      mutate(display_m2 = price_per_m2 / m2_info$scale)
    validate(need(nrow(df) > 0, "Chưa có dữ liệu cho hai khu vực đã chọn."))

    color_values <- setNames(chart_colors(n_distinct(df$district_name)), sort(unique(df$district_name)))
    p <- df %>%
      ggplot(aes(x = display_m2, color = district_name)) +
      stat_ecdf(linewidth = 1) +
      scale_color_manual(values = color_values) +
      labs(x = paste0("Giá/m² (", m2_info$unit, ")"), y = "Xác suất tích lũy", color = "Khu vực") +
      chart_theme()
    interactive_chart(p)
  })

  # CLT: lay mau co hoan lai nhieu lan de tao sampling distribution cua mean.
  output$clt_plot <- renderPlotly({
    tx <- input$stat_transaction %||% "Bán"
    m2_info <- price_m2_display_info(tx)
    values <- stat_base_data()$price_per_m2
    means <- bootstrap_mean_distribution(values / m2_info$scale, input$stat_sample_size, input$stat_reps)
    validate(need(length(means) > 0, "Cần ít nhất vài dòng giá/m² hợp lệ để mô phỏng CLT."))
    observed_mean <- mean(values / m2_info$scale, na.rm = TRUE)

    plot_ly(x = means, type = "histogram", nbinsx = 34, marker = list(color = "#0072bc", line = list(color = "#ffffff", width = 0.5))) %>%
      layout(
        shapes = list(list(type = "line", x0 = observed_mean, x1 = observed_mean, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#ef4444", width = 3))),
        annotations = list(list(x = observed_mean, y = 1, yref = "paper", text = "Mean mẫu gốc", showarrow = FALSE, xanchor = "left", font = list(color = "#ef4444"))),
        margin = list(l = 62, r = 20, t = 12, b = 62),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        bargap = 0.04,
        xaxis = list(title = paste0("Trung bình mẫu giá/m² (", m2_info$unit, ")"), gridcolor = "#e5e7eb"),
        yaxis = list(title = "Số lần lặp", gridcolor = "#e5e7eb"),
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  # Bootstrap: tao bootstrap distribution cua median va lay CI theo confidence level.
  output$bootstrap_plot <- renderPlotly({
    tx <- input$stat_transaction %||% "Bán"
    m2_info <- price_m2_display_info(tx)
    district <- stat_selected_district_a()
    values <- stat_base_data() %>%
      filter(district_name == district, finite_positive(price_per_m2)) %>%
      pull(price_per_m2) / m2_info$scale
    boot <- bootstrap_median_ci(values, input$stat_reps, confidence_level_value(input$stat_confidence))
    validate(need(length(boot$distribution) > 0, "Cần tối thiểu 5 dòng cho bootstrap."))

    plot_ly(x = boot$distribution, type = "histogram", nbinsx = 34, marker = list(color = "#10b981", line = list(color = "#ffffff", width = 0.5))) %>%
      layout(
        shapes = list(
          list(type = "line", x0 = boot$lower, x1 = boot$lower, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#f59e0b", width = 2, dash = "dash")),
          list(type = "line", x0 = boot$upper, x1 = boot$upper, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#f59e0b", width = 2, dash = "dash")),
          list(type = "line", x0 = boot$observed, x1 = boot$observed, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#ef4444", width = 3))
        ),
        annotations = list(list(
          x = boot$observed, y = 1, yref = "paper",
          text = paste0("Median: ", format_number_vi(boot$observed, m2_info$digits), " ", m2_info$unit),
          showarrow = FALSE, xanchor = "left", font = list(color = "#ef4444")
        )),
        margin = list(l = 62, r = 20, t = 12, b = 62),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        bargap = 0.04,
        xaxis = list(title = paste0("Bootstrap median giá/m² (", m2_info$unit, ")"), gridcolor = "#e5e7eb"),
        yaxis = list(title = "Số lần lặp", gridcolor = "#e5e7eb"),
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  # Hypothesis testing: H0 la mean log(gia/m2) cua hai khu vuc bang nhau.
  # Dung t-test cho mean log-scale va Wilcoxon nhu phuong an robust hon voi du lieu lech.
  output$hypothesis_table <- renderTable({
    tx <- input$stat_transaction %||% "Bán"
    m2_info <- price_m2_display_info(tx)
    district_a <- stat_selected_district_a()
    district_b <- stat_selected_district_b()
    alpha <- 0.05
    test_df <- stat_base_data() %>%
      filter(district_name %in% c(district_a, district_b), finite_positive(price_per_m2)) %>%
      mutate(group = district_name, log_m2 = log1p(price_per_m2))

    if (length(unique(test_df$group)) < 2 || nrow(test_df) < 10) {
      return(data.frame(Ket_qua = "Chưa đủ dữ liệu để kiểm định hai nhóm."))
    }

    group_a <- test_df %>% filter(group == district_a)
    group_b <- test_df %>% filter(group == district_b)
    t_result <- tryCatch(stats::t.test(log_m2 ~ group, data = test_df), error = function(e) NULL)
    w_result <- tryCatch(stats::wilcox.test(log_m2 ~ group, data = test_df), error = function(e) NULL)
    diff_median <- median(group_a$price_per_m2, na.rm = TRUE) - median(group_b$price_per_m2, na.rm = TRUE)

    tibble::tibble(
      `Mục` = c("H0", "Nhóm A", "Nhóm B", "Chênh lệch median A-B", "t-test p-value", "Wilcoxon p-value", "Kết luận α=0,05"),
      `Giá trị` = c(
        "Mean log(giá/m²) hai khu vực bằng nhau",
        paste0(district_a, " · n=", format_count_vi(nrow(group_a))),
        paste0(district_b, " · n=", format_count_vi(nrow(group_b))),
        paste0(format_number_vi(diff_median / m2_info$scale, m2_info$digits), " ", m2_info$unit),
        if (is.null(t_result)) "NA" else p_value_label(t_result$p.value),
        if (is.null(w_result)) "NA" else p_value_label(w_result$p.value),
        if (is.null(t_result)) "Không đủ dữ liệu" else hypothesis_decision(t_result$p.value, alpha)
      )
    )
  })

  # Empirical probability: cac xac suat uoc luong truc tiep tu mau du lieu dang loc.
  output$empirical_probability_table <- renderTable({
    df <- stat_base_data()
    district <- stat_selected_district_a()
    category <- input$stat_category
    top_category <- df %>%
      filter(!is_missing_label(category_name)) %>%
      count(category_name, sort = TRUE) %>%
      slice(1) %>%
      pull(category_name)
    if (length(top_category) == 0) top_category <- "Không rõ"
    focus_category <- if (is_selected_filter(category)) category[[1]] else top_category
    q75 <- safe_quantile(df$price_per_m2, 0.75)

    tibble::tibble(
      `Xác suất thực nghiệm` = c(
        paste0("P(khu vực = ", district, ")"),
        paste0("P(loại BĐS = ", focus_category, ")"),
        paste0("P(giá/m² >= Q3)"),
        paste0("P(", focus_category, " | ", district, ")"),
        paste0("P(tọa độ gốc từ nguồn)")
      ),
      `Giá trị` = paste0(round(c(
        mean(df$district_name == district, na.rm = TRUE),
        mean(df$category_name == focus_category, na.rm = TRUE),
        mean(df$price_per_m2 >= q75[[1]], na.rm = TRUE),
        {
          district_df <- df %>% filter(district_name == district)
          if (nrow(district_df) == 0) NA_real_ else mean(district_df$category_name == focus_category, na.rm = TRUE)
        },
        mean(df$coord_status == "Tọa độ gốc từ nguồn", na.rm = TRUE)
      ) * 100, 1), "%")
    )
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
    if (is.null(pred) || is.na(pred)) "Chưa dự đoán được" else format_vnd_full(pred)
  })

  output$prediction_note <- renderText({
    pred <- prediction()
    if (is.null(pred) || is.na(pred)) {
      "Hãy chọn khu vực cũ và loại bất động sản có trong dữ liệu train."
    } else {
      paste("Giao dịch:", input$predict_transaction, "· Loại:", input$pred_category, "· Khu vực:", input$pred_district, "· Diện tích:", input$predict_area, "m²")
    }
  })

  output$prediction_model_note <- renderText({
    is_rent_pred <- identical(input$predict_transaction, "Cho thuê")
    paste0("Mô hình: ", prediction_model_label(is_rent_pred), " · giá trị mang tính tham khảo")
  })

  output$prediction_market_band <- renderUI({
    pred <- prediction()
    if (is.null(pred) || is.na(pred)) return(NULL)
    band <- prediction_market_band(
      listings(),
      input$pred_district,
      input$pred_category,
      input$predict_transaction,
      input$predict_area
    )
    div(
      class = "filter-summary full",
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Mẫu tương đồng"),
          div(class = "filter-chip-value", paste0(format_count_vi(band$n), " tin"))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Q1 thị trường"),
          div(class = "filter-chip-value", format_vnd(band$lower))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Median thị trường"),
          div(class = "filter-chip-value", format_vnd(band$median))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Q3 thị trường"),
          div(class = "filter-chip-value", format_vnd(band$upper)))
    )
  })

  output$importance_plot <- renderPlotly({
    is_rent_pred <- identical(input$predict_transaction, "Cho thuê")
    path <- if (is_rent_pred) {
      if (file.exists(RF_IMPORTANCE_RENT_PATH)) RF_IMPORTANCE_RENT_PATH else RF_IMPORTANCE_SALE_PATH
    } else {
      if (file.exists(RF_IMPORTANCE_SALE_PATH)) RF_IMPORTANCE_SALE_PATH else RF_IMPORTANCE_RENT_PATH
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
      chart_theme()
    interactive_chart(p, tooltip = "text")
  })

  output$model_card_ui <- renderUI({
    tx <- chart_transaction("diagnostic_tx")
    segment_key <- if (identical(tx, "Cho thuê")) "rent" else "sale"
    m <- metrics() %>% filter(segment == segment_key)
    r <- registry() %>% filter(segment == segment_key)
    diag <- diagnostic_data()
    best <- if (nrow(r) > 0) r$best_model[[1]] else best_model_name_only(m)
    best_mape <- if (nrow(r) > 0) paste0("MAPE ", round(r$mape[[1]] * 100, 1), "%") else best_model_mape_only(m)
    diag_mape <- if (nrow(diag) > 0) paste0(round(mean(diag$ape, na.rm = TRUE) * 100, 1), "%") else "NA"
    diag_residual <- if (nrow(diag) > 0) format_number_vi(stats::sd(diag$residual_log, na.rm = TRUE), 3) else "NA"

    div(
      class = "kpi-grid",
      kpi_card("Nhóm model", tx, "bán và thuê được train riêng", "tags", "default", value_class = "text-mode"),
      kpi_card("Best model", best, "chọn theo MAPE/RMSE validate", "bullseye", "success", delta = best_mape, value_class = "text-mode"),
      kpi_card("MAPE sanity check", diag_mape, "mẫu dự đoán lại từ dữ liệu hiện có", "chart-simple", "warning"),
      kpi_card("SD residual log", diag_residual, "độ phân tán sai số log", "ruler", "danger")
    )
  })

  output$diagnostic_scatter_plot <- renderPlotly({
    tx <- chart_transaction("diagnostic_tx")
    price_info <- price_display_info(tx)
    df <- diagnostic_data() %>%
      mutate(
        actual_display = actual_price / ifelse(identical(tx, "Cho thuê"), 1e6, 1e9),
        predicted_display = predicted_price / ifelse(identical(tx, "Cho thuê"), 1e6, 1e9),
        tooltip = paste0(
          "Khu vực: ", district_name,
          "<br>Loại BĐS: ", category_name,
          "<br>Actual: ", format_number_vi(actual_display, price_info$digits), " ", price_info$unit,
          "<br>Predicted: ", format_number_vi(predicted_display, price_info$digits), " ", price_info$unit,
          "<br>APE: ", round(ape * 100, 1), "%"
        )
      )
    validate(need(nrow(df) > 0, "Chưa có dữ liệu diagnostics."))

    max_axis <- safe_quantile(c(df$actual_display, df$predicted_display), 0.98)
    p <- df %>%
      filter(actual_display <= max_axis[[1]], predicted_display <= max_axis[[1]]) %>%
      ggplot(aes(x = actual_display, y = predicted_display, color = category_name, text = tooltip)) +
      geom_point(alpha = 0.55, size = 1.8) +
      geom_abline(slope = 1, intercept = 0, color = "#ef4444", linewidth = 0.9) +
      guides(color = "none") +
      labs(x = paste0("Actual (", price_info$unit, ")"), y = paste0("Predicted (", price_info$unit, ")")) +
      chart_theme()
    interactive_chart(p, tooltip = "text") %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 92, r = 28, t = 16, b = 78),
        xaxis = list(automargin = TRUE, title = list(standoff = 18), gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb"),
        yaxis = list(automargin = TRUE, title = list(standoff = 18), gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb")
      )
  })

  output$diagnostic_residual_plot <- renderPlotly({
    df <- diagnostic_data()
    validate(need(nrow(df) > 0, "Chưa có dữ liệu residual."))
    plot_ly(
      df,
      x = ~residual_log,
      type = "histogram",
      nbinsx = 36,
      marker = list(color = "#f59e0b", line = list(color = "#ffffff", width = 0.5)),
      hovertemplate = "Residual log: %{x:.3f}<br>Số dòng: %{y}<extra></extra>"
    ) %>%
      layout(
        shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#ef4444", width = 3))),
        margin = list(l = 82, r = 28, t = 16, b = 86),
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#d7e6f5", font = list(color = "#1f2937")),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        bargap = 0.04,
        xaxis = list(title = list(text = "Residual log(actual) - log(predicted)", standoff = 18), automargin = TRUE, gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb"),
        yaxis = list(title = list(text = "Số dòng", standoff = 18), automargin = TRUE, gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$diagnostic_error_group_plot <- renderPlotly({
    df <- diagnostic_data()
    validate(need(nrow(df) > 0, "Chưa có dữ liệu diagnostics theo khu vực."))
    plot_df <- df %>%
      group_by(district_name) %>%
      summarise(mape = mean(ape, na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n >= 10) %>%
      slice_max(mape, n = 12) %>%
      mutate(
        tooltip = paste0("Khu vực: ", district_name, "<br>MAPE sanity: ", round(mape * 100, 1), "%<br>Số dòng: ", format_count_vi(n))
      )
    validate(need(nrow(plot_df) > 0, "Cần ít nhất 10 dòng/khu vực để vẽ sai số nhóm."))
    p <- plot_df %>%
      ggplot(aes(x = reorder(district_name, mape), y = mape * 100, text = tooltip)) +
      geom_col(fill = "#ef4444", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "MAPE (%)") +
      chart_theme()
    interactive_chart(p, tooltip = "text") %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 178, r = 28, t = 16, b = 82),
        xaxis = list(automargin = TRUE, title = list(standoff = 18), gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb"),
        yaxis = list(automargin = TRUE, tickfont = list(size = 11))
      )
  })

  output$metrics_compare_plot <- renderPlotly({
    tx <- chart_transaction("diagnostic_tx")
    segment_key <- if (identical(tx, "Cho thuê")) "rent" else "sale"
    m <- metrics() %>% filter(segment == segment_key)
    validate(need(nrow(m) > 0, "Chưa có file metrics model."))
    min_rmse <- min(m$rmse_vnd, na.rm = TRUE)
    min_mae <- min(m$mae_vnd, na.rm = TRUE)
    plot_df <- bind_rows(
      m %>% transmute(model, metric = "MAPE (%)", value = mape * 100),
      m %>% transmute(model, metric = "R² (%)", value = pmax(r2, 0) * 100),
      m %>% transmute(model, metric = "RMSE index", value = rmse_vnd / min_rmse * 100),
      m %>% transmute(model, metric = "MAE index", value = mae_vnd / min_mae * 100)
    ) %>%
      mutate(
        model_short = dplyr::recode(
          model,
          "Linear Regression" = "Linear",
          "Random Forest" = "RF",
          "XGBoost" = "XGB",
          "RF + XGBoost Ensemble" = "RF+XGB",
          "Tuned RF/XGBoost Ensemble" = "Tuned Ens.",
          .default = model
        ),
        model_short = factor(model_short, levels = unique(model_short)),
        tooltip = paste0("Model: ", model, "<br>Metric: ", metric, "<br>Giá trị: ", format_number_vi(value, 1))
      )

    plot_ly(
      plot_df,
      x = ~model_short,
      y = ~value,
      color = ~metric,
      type = "bar",
      text = ~tooltip,
      hovertemplate = "%{text}<extra></extra>",
      colors = chart_colors(n_distinct(plot_df$metric))
    ) %>%
      layout(
        barmode = "group",
        margin = list(l = 82, r = 28, t = 16, b = 120),
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#d7e6f5", font = list(color = "#1f2937")),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        legend = list(orientation = "h", x = 0, y = -0.30, font = list(size = 11), itemwidth = 30),
        xaxis = list(title = "", automargin = TRUE, tickangle = 0, tickfont = list(size = 11)),
        yaxis = list(title = list(text = "Giá trị / index", standoff = 18), automargin = TRUE, gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$cluster_plot <- renderPlotly({
    validate(need(file.exists(CLUSTER_PATH), paste0("Chưa có ", PATHS$clusters_csv, ". Hãy chạy scripts/models/huan_luyen_mo_hinh.R.")))
    tx <- chart_transaction("cluster_tx")
    m2_info <- price_m2_display_info(tx)
    cluster_df <- read_csv(CLUSTER_PATH, show_col_types = FALSE)
    if (!"transaction_type" %in% names(cluster_df)) {
      cluster_df$transaction_type <- "Bán"
    }
    cluster_df <- cluster_df %>%
      mutate(
        district_name = clean_display_label(district_name),
        category_name = clean_display_label(category_name)
      ) %>%
      filter(transaction_type == tx)
    validate(need(nrow(cluster_df) > 0, paste("Chưa có dữ liệu phân cụm cho giao dịch", tx)))

    plot_df <- known_rows_or_all(cluster_df, "district_name") %>%
      known_rows_or_all("category_name") %>%
      mutate(
        cluster = as.factor(cluster),
        display_price_m2 = median_price_per_m2 / m2_info$scale,
        tooltip = paste0(
          "Giao dịch: ", transaction_type,
          "<br>Khu vực cũ: ", district_name,
          "<br>Loại BĐS: ", category_name,
          "<br>Cụm: ", cluster,
          "<br>Diện tích trung vị: ", format_number_vi(median_area, 1), " m²",
          "<br>Giá/m² trung vị: ", format_number_vi(display_price_m2, m2_info$digits), " ", m2_info$unit,
          "<br>Số tin: ", format_count_vi(listing_count)
        )
      )
    color_values <- setNames(chart_colors(n_distinct(plot_df$cluster)), sort(unique(plot_df$cluster)))
    p <- plot_df %>%
      ggplot(aes(x = median_area, y = display_price_m2, color = cluster, size = listing_count, text = tooltip)) +
      geom_point(alpha = 0.78) +
      scale_color_manual(values = color_values) +
      labs(x = "Diện tích trung vị (m²)", y = paste0("Giá/m² trung vị (", m2_info$unit, ")"), color = "Cụm", size = "Số tin") +
      chart_theme()
    interactive_chart(p, tooltip = "text")
  })

  output$data_quality_cards <- renderUI({
    df <- data_filtered()
    quality <- build_data_quality_summary(df)
    exact_rate <- mean(df$coord_status == "Tọa độ gốc từ nguồn", na.rm = TRUE)
    future_rows <- quality %>% filter(nhom == "Ngày đăng tương lai") %>% pull(so_dong)
    missing_rows <- quality %>% filter(nhom == "Thiếu khu vực/loại BĐS") %>% pull(so_dong)
    duplicate_rows <- quality %>% filter(nhom == "Trùng lặp nghi ngờ") %>% pull(so_dong)
    div(
      class = "kpi-grid",
      kpi_card("Dòng sau lọc", format_count_vi(nrow(df)), "đang hiển thị trong bảng", "table", "default"),
      kpi_card("Tọa độ gốc", paste0(round(exact_rate * 100, 1), "%"), "phần còn lại là ước lượng", "map-location-dot", "success"),
      kpi_card("Ngày tương lai", format_count_vi(future_rows), "cần rà format/crawl", "calendar-days", ifelse(future_rows > 0, "danger", "success")),
      kpi_card("Trùng nghi ngờ", format_count_vi(duplicate_rows), paste0("thiếu nhãn: ", format_count_vi(missing_rows)), "copy", "warning")
    )
  })

  output$data_quality_table <- renderTable({
    build_data_quality_summary(data_filtered()) %>%
      transmute(
        `Nhóm kiểm tra` = nhom,
        `Số dòng` = format_count_vi(so_dong),
        `Mức độ` = muc_do,
        `Ghi chú` = ghi_chu
      )
  })

  output$data_quality_plot <- renderPlotly({
    df <- data_filtered() %>%
      mutate(coord_exact = coord_status == "Tọa độ gốc từ nguồn") %>%
      group_by(source) %>%
      summarise(
        listings = n(),
        exact_rate = mean(coord_exact, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(listings)) %>%
      mutate(
        source_label = source_label_vi(source),
        tooltip = paste0(
          "Nguồn: ", source_label,
          "<br>Số dòng: ", format_count_vi(listings),
          "<br>Tọa độ gốc: ", round(exact_rate * 100, 1), "%"
        )
      )
    validate(need(nrow(df) > 0, "Không có dữ liệu để vẽ độ phủ nguồn."))
    p <- df %>%
      ggplot(aes(x = reorder(source_label, listings), y = listings, fill = exact_rate, text = tooltip)) +
      geom_col(width = 0.72) +
      coord_flip() +
      scale_fill_gradient(low = "#f59e0b", high = "#10b981", labels = function(x) paste0(round(x * 100), "%")) +
      labs(x = NULL, y = "Số dòng", fill = "Tọa độ gốc") +
      chart_theme()
    interactive_chart(p, tooltip = "text")
  })

  output$data_table <- renderDT({
    data_filtered() %>%
      mutate(source_link = listing_url(ad_url, source)) %>%
      transmute(
        `Nguồn` = source_label_vi(source),
        `Giao dịch` = transaction_type,
        `Tiêu đề` = title,
        `Khu vực cũ` = district_name,
        `Phường/xã` = ward,
        `Loại BĐS` = category_name,
        `Giá` = format_vnd_full(price),
        `Diện tích` = paste0(round(area, 1), " m²"),
        `Giá/m²` = format_vnd_full(price_per_m2),
        `Link` = ifelse(
          !is.na(source_link),
          paste0('<a href="', htmltools::htmlEscape(source_link), '" target="_blank" rel="noopener noreferrer">Xem tin</a>'),
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
