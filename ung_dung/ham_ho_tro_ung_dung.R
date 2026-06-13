# ============================================================
# APP HELPERS - DATA, MODEL, EDA, STATISTICS, ASSISTANT
# File này được source từ app.R để app.R chỉ còn vai trò điều phối UI/server.
# ============================================================

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

normalize_district_key <- function(x) {
  x <- strip_vietnamese(x)
  x <- gsub("^(quan|huyen|thanh pho)\\s+", "", x)
  x <- gsub("^q\\.?\\s*", "", x)
  x <- gsub("^tp\\.?\\s*", "", x)
  x <- gsub("\\s+", " ", trimws(x))
  x <- gsub("\\s+cu$", "", x)
  ifelse(x %in% c("", "khong ro", "unknown", "na"), NA_character_, x)
}

is_missing_label <- function(x) {
  key <- strip_vietnamese(trimws(as.character(x)))
  is.na(x) | key %in% c("", "unknown", "khong ro", "na", "nan", "null")
}

clean_display_label <- function(x, fallback = "Không rõ") {
  x <- trimws(as.character(x))
  ifelse(is_missing_label(x), fallback, x)
}

known_rows_or_all <- function(df, col) {
  values <- df[[col]]
  known <- df[!is_missing_label(values), , drop = FALSE]
  if (nrow(known) > 0) known else df
}

choice_values <- function(x) {
  x <- unique(clean_display_label(x))
  known <- sort(x[!is_missing_label(x)])
  missing <- sort(x[is_missing_label(x)])
  c(known, missing)
}

nice_slider_max <- function(x, step, fallback) {
  x <- suppressWarnings(max(as.numeric(x), na.rm = TRUE))
  if (!is.finite(x) || is.na(x) || x <= 0) return(fallback)
  max(fallback, ceiling(x / step) * step)
}

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

load_metrics <- function() {
  if (!file.exists(METRICS_PATH)) return(tibble())
  read_csv(METRICS_PATH, show_col_types = FALSE)
}

load_registry <- function() {
  if (!file.exists(REGISTRY_PATH)) return(tibble())
  read_csv(REGISTRY_PATH, show_col_types = FALSE)
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

format_number_vi <- function(x, digits = 1) {
  format(round(x, digits), big.mark = ".", decimal.mark = ",", nsmall = digits, trim = TRUE)
}

format_count_vi <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
}

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

format_metric <- function(x) {
  vapply(x, function(value) {
    if (is.na(value) || !is.finite(value)) return("NA")
    format(round(value, 3), nsmall = 3)
  }, character(1))
}

active_or_all <- function(x, all_label = "Tất cả") {
  if (is.null(x) || length(x) == 0 || identical(x, "__all__")) all_label else paste(x, collapse = ", ")
}

active_source_or_all <- function(x, all_label = "Tất cả") {
  if (is.null(x) || length(x) == 0 || identical(x, "__all__")) {
    all_label
  } else {
    paste(source_label_vi(x), collapse = ", ")
  }
}

safe_range <- function(x, default) {
  if (is.null(x) || length(x) < 2) default else x
}

is_selected_filter <- function(x) {
  !is.null(x) && length(x) > 0 && !identical(x, "__all__")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

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
    identical(model_name, "Tuned RF/XGBoost Ensemble") ~ "Tuned RF/XGBoost Ensemble",
    TRUE ~ as.character(model_name)
  )
}

add_prediction_encoding_keys <- function(input_row) {
  input_row %>%
    mutate(
      district_category_key = paste(as.character(district_name), as.character(category_name), sep = " | "),
      source_category_key = paste(as.character(source), as.character(category_name), sep = " | ")
    )
}

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

# Cache để giữ model trong bộ nhớ tránh đọc đĩa liên tục
GLOBAL_MODEL_CACHE <- new.env(parent = emptyenv())

load_model_cached <- function(model_path) {
  if (!file.exists(model_path)) return(NULL)

  # Tạo key duy nhất dựa trên đường dẫn và thời gian sửa đổi file
  mtime <- file.info(model_path)$mtime
  cache_key <- paste0(model_path, "_", as.character(as.numeric(mtime)))

  if (exists(cache_key, envir = GLOBAL_MODEL_CACHE)) {
    return(get(cache_key, envir = GLOBAL_MODEL_CACHE))
  }

  # Đọc model mới và lưu vào cache
  message("Đang tải model từ đĩa: ", model_path)
  bundle <- readRDS(model_path)

  # Xóa các cache cũ của file này để tránh rò rỉ bộ nhớ
  existing_keys <- ls(envir = GLOBAL_MODEL_CACHE)
  old_keys <- existing_keys[startsWith(existing_keys, paste0(model_path, "_"))]
  if (length(old_keys) > 0) {
    rm(list = old_keys, envir = GLOBAL_MODEL_CACHE)
  }

  assign(cache_key, bundle, envir = GLOBAL_MODEL_CACHE)
  bundle
}

predict_price <- function(input_row, is_rent) {
  model_path <- if (is_rent) RENT_MODEL_PATH else SALE_MODEL_PATH
  if (!file.exists(model_path)) return(NA_real_)

  bundle <- load_model_cached(model_path)
  if (is.null(bundle)) return(NA_real_)

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

prediction_model_label <- function(is_rent) {
  model_path <- if (is_rent) RENT_MODEL_PATH else SALE_MODEL_PATH
  if (!file.exists(model_path)) return("Chưa có model")
  bundle <- load_model_cached(model_path)
  if (is.null(bundle)) return("Chưa có model")
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
  median_num <- function(x, default = 0) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[!is.na(x)]
    if (length(x) == 0) default else median(x)
  }
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

# ============================================================
# ASSISTANT - SHARED TEXT HELPERS
# Các hàm nhỏ bên dưới được Assistant V2 dùng lại để:
# - chuẩn hóa câu hỏi tiếng Việt
# - dò keyword/regex
# - nhận diện quận/huyện
# - dựng các block HTML giải thích ngắn
# ============================================================

# Chuẩn hóa text về dạng không dấu, lowercase để match keyword ổn định hơn.
assistant_text_key <- function(x) {
  x <- strip_vietnamese(as.character(x))
  x <- gsub("[^a-z0-9 ]+", " ", x)
  gsub("\\s+", " ", trimws(x))
}

# Kiểm tra text có khớp ít nhất một pattern không.
assistant_contains_any <- function(text, patterns) {
  any(vapply(patterns, function(pattern) grepl(pattern, text), logical(1)))
}

# Nhận diện khu vực được nhắc trong câu hỏi của người dùng.
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

# Lấy số đầu tiên trong một đoạn text, dùng cho ngân sách/diện tích/phòng.
assistant_extract_number <- function(text) {
  value <- gsub(",", ".", text)
  number <- regmatches(value, regexpr("[0-9]+([.][0-9]+)?", value))
  if (length(number) == 0 || !nzchar(number)) return(NA_real_)
  suppressWarnings(as.numeric(number))
}

# Gắn nhãn độ tin cậy theo số lượng mẫu mà bot đang dùng.
assistant_confidence_label <- function(n) {
  if (n >= 80) "Cao"
  else if (n >= 20) "Vừa"
  else "Thấp"
}

# Diễn giải một giá trị cao/thấp hơn benchmark bao nhiêu phần trăm.
assistant_relative_text <- function(value, baseline, label = "mặt bằng chung") {
  if (!is.finite(value) || !is.finite(baseline) || baseline <= 0) return("chưa đủ dữ liệu để so chuẩn")
  pct <- (value / baseline - 1) * 100
  if (abs(pct) < 6) return(paste0("gần ngang ", label))
  paste0(ifelse(pct > 0, "cao hơn ", "thấp hơn "), label, " khoảng ", round(abs(pct), 1), "%")
}

# Dựng một block HTML ngắn để bot trả lời có cấu trúc dễ đọc.
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

# ============================================================
# ASSISTANT V2 - LOCAL TOOL-BASED ASSISTANT
# Luồng chính:
# 1. trích tiêu chí từ câu hỏi
# 2. gộp với context câu trước nếu là follow-up
# 3. nhận diện intent
# 4. gọi tool local: lọc data, so sánh, gợi ý listing hoặc gọi model dự đoán
# ============================================================

assistant_escape <- function(x) {
  htmltools::htmlEscape(as.character(x))
}

assistant_has_value <- function(x) {
  !is.null(x) && length(x) > 0 && !all(is.na(x)) && any(nzchar(trimws(as.character(x))))
}

assistant_is_finite <- function(x) {
  !is.null(x) && length(x) > 0 && !is.na(x[[1]]) && is.finite(as.numeric(x[[1]]))
}

assistant_number_label <- function(x, digits = 1, suffix = "") {
  if (!assistant_is_finite(x)) return("Chưa rõ")
  paste0(format_number_vi(as.numeric(x[[1]]), digits), suffix)
}

assistant_format_area <- function(x) {
  vapply(x, function(value) {
    if (is.na(value) || !is.finite(value) || value <= 0) return("Chưa rõ diện tích")
    paste0(format_number_vi(value, 1), " m²")
  }, character(1))
}

assistant_squish_number_text <- function(text) {
  text <- strip_vietnamese(text)
  text <- gsub(",", ".", text, fixed = TRUE)
  gsub("([0-9]+)\\s+([0-9]+)\\s*(ty|ti|trieu|tr|m2|m vuong|met vuong)", "\\1.\\2 \\3", text, perl = TRUE)
}

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

assistant_extract_budget <- function(question) {
  assistant_extract_budget_info(question)$budget
}

assistant_extract_area <- function(question) {
  assistant_extract_area_info(question)$area
}

assistant_extract_rooms <- function(question) {
  key <- assistant_text_key(question)
  room_match <- regmatches(key, regexpr("[0-9]+\\s*(pn|p n|phong ngu|phong|ngu)", key, perl = TRUE))
  if (length(room_match) == 0 || !nzchar(room_match)) return(NA_real_)
  assistant_extract_number(room_match)
}

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

assistant_extract_preference <- function(question) {
  key <- assistant_text_key(question)
  if (assistant_contains_any(key, c("re hon", "mem hon", "gia tot", "hoi", "duoi gia", "thap hon mat bang", "deal"))) return("value")
  if (assistant_contains_any(key, c("rong hon", "dien tich lon", "to hon", "nhieu m2"))) return("larger")
  if (assistant_contains_any(key, c("moi hon", "tin moi", "dang gan day"))) return("recent")
  if (assistant_contains_any(key, c("gan trung tam", "trung tam"))) return("central")
  NULL
}

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

assistant_empty_context <- function() {
  list(criteria = NULL, intent = NULL, pending = FALSE, last_answer_at = NULL)
}

assistant_has_context <- function(context) {
  is.list(context) && !is.null(context$criteria)
}

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

assistant_budget_text <- function(criteria) {
  has_min <- assistant_is_finite(criteria$budget_min)
  has_max <- assistant_is_finite(criteria$budget_max)
  if (has_min && has_max) return(paste0("ngân sách ", format_vnd(criteria$budget_min), " - ", format_vnd(criteria$budget_max)))
  if (has_max) return(paste0("ngân sách dưới ", format_vnd(criteria$budget_max)))
  if (has_min) return(paste0("ngân sách trên ", format_vnd(criteria$budget_min)))
  NULL
}

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

assistant_criteria_text <- function(criteria) {
  parts <- c()
  if (!is.null(criteria$transaction)) parts <- c(parts, criteria$transaction)
  if (length(criteria$districts) > 0) parts <- c(parts, paste(criteria$districts, collapse = ", "))
  if (length(criteria$categories) > 0) parts <- c(parts, paste(criteria$categories, collapse = ", "))
  parts <- c(parts, assistant_budget_text(criteria), assistant_area_text(criteria))
  if (assistant_is_finite(criteria$rooms)) parts <- c(parts, paste0("từ ", as.integer(criteria$rooms), " phòng"))
  if (length(parts) == 0) "toàn bộ dữ liệu" else paste(parts, collapse = " · ")
}

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

assistant_scope_note <- function(criteria) {
  paste0("<p class='assistant-lead'>Mình đang hiểu tiêu chí là <b>", assistant_escape(assistant_criteria_text(criteria)), "</b>.</p>")
}

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

assistant_clarify_response <- function(intent, criteria, questions) {
  questions <- unique(as.character(questions))
  paste0(
    assistant_scope_note(criteria),
    assistant_insight_block("Mình cần thêm một chút", questions),
    "<p>Bạn trả lời ngắn cũng được, ví dụ: <b>mua, 4 tỷ, căn hộ</b> hoặc <b>thuê, dưới 15 triệu, Thủ Đức</b>.</p>"
  )
}

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

assistant_answer <- function(question, df, context = assistant_empty_context()) {
  assistant_answer_bundle(question, df, context)$html
}

assistant_message <- function(role, html) {
  list(role = role, html = html)
}

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

price_color <- function(price, low_cutoff = 3e9, high_cutoff = 8e9) {
  dplyr::case_when(
    is.na(price) ~ "#64748b",
    price < low_cutoff ~ "#059669",
    price < high_cutoff ~ "#d97706",
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

filter_source_select <- function(input_id, choices, all_label) {
  selectInput(
    input_id,
    NULL,
    choices = c(setNames("__all__", all_label), setNames(choices, source_label_vi(choices))),
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

chart_colors <- function(n) {
  if (n <= length(chart_palette)) return(chart_palette[seq_len(n)])
  grDevices::hcl.colors(n, palette = "Dark 3")
}

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

finite_positive <- function(x) {
  !is.na(x) & is.finite(x) & x > 0
}

safe_quantile <- function(x, probs, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return(rep(default, length(probs)))
  as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE))
}

confidence_level_value <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  if (!is.finite(value) || !(value %in% c(0.90, 0.95, 0.99))) 0.95 else value
}

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

bootstrap_mean_distribution <- function(x, sample_size = 30, reps = 600, seed = 2026) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  reps <- max(100, min(3000, as.integer(reps %||% 600)))
  sample_size <- max(2, min(length(x), as.integer(sample_size %||% 30)))
  if (length(x) < 5) return(numeric())
  set.seed(seed)
  replicate(reps, mean(sample(x, size = sample_size, replace = TRUE), na.rm = TRUE))
}

p_value_label <- function(p_value) {
  if (!is.finite(p_value)) return("NA")
  if (p_value < 0.001) return("< 0,001")
  format_number_vi(p_value, 3)
}

hypothesis_decision <- function(p_value, alpha = 0.05) {
  if (!is.finite(p_value)) return("Không đủ dữ liệu")
  if (p_value < alpha) {
    "Bác bỏ H0"
  } else {
    "Chưa đủ bằng chứng bác bỏ H0"
  }
}

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


