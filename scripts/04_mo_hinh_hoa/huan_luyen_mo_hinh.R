source("scripts/config/duong_dan_du_an.R")
use_local_r_libs()
source(PATHS$display_labels_script)
source(PATHS$model_feature_script)

required_packages <- c("readr", "dplyr", "lubridate", "randomForest", "xgboost", "Matrix", "ggplot2", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Thieu package: ", paste(missing_packages, collapse = ", "),
    "\nCai bang: install.packages(c(",
    paste(sprintf('\"%s\"', missing_packages), collapse = ", "), "))"
  )
}

library(readr)
library(dplyr)
library(lubridate)
library(randomForest)
library(xgboost)
library(Matrix)
library(ggplot2)

FEATURED_PATH <- PATHS$featured_csv
RAW_PATH <- PATHS$combined_raw_csv
MODEL_DIR <- PATHS$model_dir
PLOT_DIR <- PATHS$plot_dir
METADATA_PATH <- PATHS$model_metadata_rds
REGISTRY_PATH <- PATHS$registry_csv
dir.create(MODEL_DIR, showWarnings = FALSE)
dir.create(PLOT_DIR, showWarnings = FALSE)

if (file.exists(FEATURED_PATH)) {
  DATA_PATH <- FEATURED_PATH
} else if (file.exists(RAW_PATH)) {
  DATA_PATH <- RAW_PATH
} else {
  stop("Chua co file data. Hay chay scripts/02_xu_ly_du_lieu/gop_nguon_du_lieu.R va scripts/02_xu_ly_du_lieu/tao_dac_trung.R truoc.")
}

set.seed(42)
MODEL_FAST <- identical(Sys.getenv("MODEL_FAST", "0"), "1")
RF_NTREE <- as.integer(Sys.getenv("MODEL_RF_TREES", if (MODEL_FAST) "80" else "500"))
if (is.na(RF_NTREE) || RF_NTREE < 10) RF_NTREE <- 80L

raw_df <- read_csv(DATA_PATH, show_col_types = FALSE)
if (!"price_per_m2" %in% names(raw_df)) raw_df$price_per_m2 <- NA_real_
if (!"posted_hour" %in% names(raw_df)) raw_df$posted_hour <- NA_integer_
if (!"posted_wday" %in% names(raw_df)) raw_df$posted_wday <- NA_character_
if (!"scraped_at" %in% names(raw_df)) raw_df$scraped_at <- NA_character_
if (!"is_rent" %in% names(raw_df)) raw_df$is_rent <- NA
if (!"log_price" %in% names(raw_df)) raw_df$log_price <- NA_real_
if (!"log_area" %in% names(raw_df)) raw_df$log_area <- NA_real_
if (!"distance_to_center" %in% names(raw_df)) raw_df$distance_to_center <- NA_real_
if (!"ward_price_encoded" %in% names(raw_df)) raw_df$ward_price_encoded <- NA_real_
if (!"source" %in% names(raw_df)) raw_df$source <- "unknown"
if (!"transaction_type" %in% names(raw_df)) raw_df$transaction_type <- NA_character_

now_ref <- lubridate::now(tzone = "Asia/Ho_Chi_Minh")

df <- raw_df %>%
  mutate(
    category_id = as.character(category_id),
    source = as.factor(if_else(is.na(source) | source == "", "unknown", as.character(source))),
    district_name = as.factor(district_name),
    category_name = as.factor(category_name),
    ward = as.factor(if_else(is.na(ward) | ward == "" | tolower(as.character(ward)) == "unknown", "Không rõ", as.character(ward))),
    rooms = if_else(is.na(rooms), 0, as.numeric(rooms)),
    area = if_else(is.na(area), median(area, na.rm = TRUE), as.numeric(area)),
    posted_at = coalesce(
      suppressWarnings(as_datetime(posted_at, tz = "Asia/Ho_Chi_Minh")),
      suppressWarnings(as_datetime(scraped_at, tz = "Asia/Ho_Chi_Minh")),
      now_ref
    ),
    posted_hour = coalesce(as.integer(posted_hour), hour(posted_at), 0L),
    posted_wday = as.character(posted_wday),
    posted_wday = if_else(
      is.na(posted_wday) | posted_wday == "",
      as.character(wday(posted_at, label = TRUE)),
      posted_wday
    ),
    posted_wday = as.factor(posted_wday),
    is_rent = if_else(is.na(as.logical(is_rent)), category_id %in% c("1030", "1050"), as.logical(is_rent)),
    transaction_type = as.factor(if_else(is_rent, "Cho thuê", "Bán")),
    log_price = if_else(is.na(log_price), log1p(price), log_price),
    log_area = if_else(is.na(log_area), log1p(area), log_area),
    price_per_m2 = if_else(!is.na(price_per_m2), price_per_m2,
                           if_else(!is.na(area) & area > 0, price / area, NA_real_)),
    distance_to_center = if_else(is.na(distance_to_center), median(distance_to_center, na.rm = TRUE), distance_to_center),
    ward_price_encoded = if_else(is.na(ward_price_encoded), log1p(price), ward_price_encoded)
  ) %>%
  filter(!is.na(price), price > 0, !is.na(area), area > 0, !is.na(log_price)) %>%
  filter(
    (is_rent & price >= 300000 & price <= 2e9) |
      (!is_rent & price >= 300000000 & price <= 500e9)
  ) %>%
  add_model_quality_features()

if (all(is.na(df$distance_to_center))) {
  df$distance_to_center <- 0
}

model_scope_audit_rows <- tibble()

safe_quantile <- function(x, probs, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return(rep(default, length(probs)))
  as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE))
}

mark_model_scope <- function(data, segment_name) {
  data <- add_model_quality_features(data)
  data$model_row_id <- seq_len(nrow(data))
  data$log_price_per_m2_for_scope <- log1p(data$price_per_m2)

  global_ppm2 <- safe_quantile(data$log_price_per_m2_for_scope, c(0.01, 0.99))
  global_price <- safe_quantile(data$log_price, c(0.005, 0.995))

  category_bounds <- data %>%
    group_by(model_category) %>%
    summarise(
      scope_n = n(),
      ppm2_low_raw = safe_quantile(log_price_per_m2_for_scope, 0.01),
      ppm2_high_raw = safe_quantile(log_price_per_m2_for_scope, 0.99),
      price_low_raw = safe_quantile(log_price, 0.005),
      price_high_raw = safe_quantile(log_price, 0.995),
      .groups = "drop"
    ) %>%
    mutate(
      ppm2_low = if_else(scope_n >= 80, ppm2_low_raw, global_ppm2[[1]]),
      ppm2_high = if_else(scope_n >= 80, ppm2_high_raw, global_ppm2[[2]]),
      price_low = if_else(scope_n >= 80, price_low_raw, global_price[[1]]),
      price_high = if_else(scope_n >= 80, price_high_raw, global_price[[2]])
    ) %>%
    select(model_category, scope_n, ppm2_low, ppm2_high, price_low, price_high)

  scoped <- data %>%
    left_join(category_bounds, by = "model_category") %>%
    mutate(
      model_exclusion_reason = case_when(
        is.na(price) | price <= 0 | is.na(area) | area <= 0 ~ "Giá/diện tích không hợp lệ",
        is.na(price_per_m2) | price_per_m2 <= 0 ~ "Không có giá/m² hợp lệ",
        log_price_per_m2_for_scope < ppm2_low | log_price_per_m2_for_scope > ppm2_high ~ "Ngoại lệ giá/m² theo nhóm sản phẩm",
        log_price < price_low | log_price > price_high ~ "Ngoại lệ giá tuyệt đối theo nhóm sản phẩm",
        TRUE ~ NA_character_
      ),
      model_in_scope = is.na(model_exclusion_reason)
    )

  segment_label <- if (identical(segment_name, "sale")) "ban" else "thue"
  dropped_path <- file.path(MODEL_DIR, paste0("dong_loai_khoi_model_", segment_label, ".csv"))
  scoped %>%
    filter(!model_in_scope) %>%
    select(
      segment = transaction_type,
      model_exclusion_reason,
      source,
      title,
      price,
      area,
      price_per_m2,
      district_name,
      category_name,
      model_category
    ) %>%
    write_csv(dropped_path)

  audit <- scoped %>%
    mutate(segment = segment_name) %>%
    count(segment, model_in_scope, model_exclusion_reason, name = "rows") %>%
    mutate(model_exclusion_reason = coalesce(model_exclusion_reason, "Giữ lại để train"))
  model_scope_audit_rows <<- bind_rows(model_scope_audit_rows, audit)

  scoped %>%
    filter(model_in_scope) %>%
    select(
      -model_row_id,
      -log_price_per_m2_for_scope,
      -scope_n,
      -ppm2_low,
      -ppm2_high,
      -price_low,
      -price_high,
      -model_exclusion_reason,
      -model_in_scope
    )
}

make_split <- function(data, train_ratio = 0.8) {
  data <- data %>% mutate(row_id_split = row_number())
  split_df <- data %>%
    group_by(source) %>%
    mutate(
      split_rand = runif(n()),
      split_rank = rank(split_rand, ties.method = "first"),
      split_group_n = n(),
      split_train_n = floor(split_group_n * train_ratio),
      split_train_n = if_else(split_group_n >= 5, pmax(1L, pmin(split_train_n, split_group_n - 1L)), split_train_n),
      is_train_split = if_else(split_group_n >= 5, split_rank <= split_train_n, split_rand <= train_ratio)
    ) %>%
    ungroup()

  if (sum(split_df$is_train_split) >= 10 && sum(!split_df$is_train_split) >= 2) {
    return(list(
      train = split_df %>% filter(is_train_split) %>% select(-row_id_split, -split_rand, -split_rank, -split_group_n, -split_train_n, -is_train_split),
      test = split_df %>% filter(!is_train_split) %>% select(-row_id_split, -split_rand, -split_rank, -split_group_n, -split_train_n, -is_train_split),
      split_type = "stratified_random_by_source"
    ))
  }

  idx <- sample(seq_len(nrow(data)), size = floor(nrow(data) * train_ratio))
  list(
    train = data[idx, ] %>% select(-row_id_split),
    test = data[-idx, ] %>% select(-row_id_split),
    split_type = "random_fallback"
  )
}

rmse <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))

mae <- function(actual, pred) mean(abs(actual - pred), na.rm = TRUE)

r2_score <- function(actual, pred) {
  1 - sum((actual - pred)^2, na.rm = TRUE) /
    sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
}

eval_price <- function(actual_log, pred_log) {
  actual <- expm1(actual_log)
  pred <- expm1(pred_log)
  ape <- abs((actual - pred) / actual)
  tibble(
    rmse_vnd = rmse(actual, pred),
    mae_vnd = mae(actual, pred),
    mape = mean(ape, na.rm = TRUE),
    median_ape = median(ape, na.rm = TRUE),
    p90_ape = as.numeric(quantile(ape, 0.90, na.rm = TRUE)),
    r2 = r2_score(actual, pred)
  )
}

fit_target_encoding <- function(train, group_col, target_col = "price", min_count = 5, smoothing = 10) {
  train[[group_col]] <- as.character(train[[group_col]])
  global <- median(train[[target_col]], na.rm = TRUE)
  enc <- train %>%
    group_by(.data[[group_col]]) %>%
    summarise(
      n_encoding = n(),
      median_price = median(.data[[target_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      encoded_value = log1p((n_encoding * median_price + smoothing * global) / (n_encoding + smoothing))
    )

  list(group_col = group_col, global = log1p(global), table = enc)
}

apply_target_encoding <- function(data, encoding, output_col = "ward_price_encoded") {
  group_col <- encoding$group_col
  data[[group_col]] <- as.character(data[[group_col]])
  encoding$table[[group_col]] <- as.character(encoding$table[[group_col]])
  data <- data %>%
    select(-any_of(output_col)) %>%
    left_join(
      encoding$table %>% select(all_of(group_col), encoded_value),
      by = group_col
    ) %>%
    mutate("{output_col}" := coalesce(encoded_value, encoding$global)) %>%
    select(-encoded_value)
  data
}

add_encoding_keys <- function(data) {
  data %>%
    add_model_quality_features() %>%
    mutate(
      district_category_key = paste(as.character(district_name), as.character(category_name), sep = " | "),
      source_category_key = paste(as.character(source), as.character(category_name), sep = " | "),
      district_model_category_key = paste(as.character(district_name), as.character(model_category), sep = " | ")
    )
}

fit_price_encodings <- function(train) {
  train <- add_encoding_keys(train)
  list(
    ward = fit_target_encoding(train, "ward"),
    district = fit_target_encoding(train, "district_name"),
    category = fit_target_encoding(train, "category_name"),
    source = fit_target_encoding(train, "source"),
    district_category = fit_target_encoding(train, "district_category_key"),
    source_category = fit_target_encoding(train, "source_category_key"),
    model_category = fit_target_encoding(train, "model_category"),
    district_model_category = fit_target_encoding(train, "district_model_category_key")
  )
}

apply_price_encodings <- function(data, encodings) {
  data <- add_encoding_keys(data)
  encoded_cols <- c(
    "ward_price_encoded", "district_price_encoded", "category_price_encoded",
    "source_price_encoded", "district_category_price_encoded",
    "source_category_price_encoded", "model_category_price_encoded",
    "district_model_category_price_encoded"
  )
  encoding_names <- c(
    "ward", "district", "category", "source", "district_category",
    "source_category", "model_category", "district_model_category"
  )

  for (i in seq_along(encoding_names)) {
    data <- apply_target_encoding(data, encodings[[encoding_names[[i]]]], encoded_cols[[i]])
  }

  data
}

make_formula <- function(data) {
  numeric_candidates <- intersect(
    c(
      "area", "log_area", "rooms", "inferred_rooms", "frontage_width_m", "frontage_length_m",
      "frontage_ratio", "inferred_floors", "title_has_frontage", "title_has_alley",
      "title_has_car_access", "title_has_corner", "title_has_elevator",
      "title_has_furnished", "title_has_legal", "title_has_income_info",
      "title_has_building", "title_has_business", "title_has_land_signal",
      "title_has_apartment_signal", "title_has_room_signal", "title_has_luxury",
      "title_has_urgent", "title_token_count", "posted_hour", "distance_to_center",
      "log_distance_to_center", "floor_area_proxy", "log_floor_area_proxy",
      "room_density", "area_per_room", "log_area_per_room",
      "ward_price_encoded", "district_price_encoded", "category_price_encoded",
      "source_price_encoded", "district_category_price_encoded",
      "source_category_price_encoded", "model_category_price_encoded",
      "district_model_category_price_encoded", "listing_age_days"
    ),
    names(data)
  )
  numeric_terms <- numeric_candidates[
    vapply(data[numeric_candidates], function(x) sum(!is.na(x)) > 0, logical(1))
  ]
  factor_candidates <- intersect(c("source", "district_name", "category_name", "model_category", "area_band", "distance_band", "posted_wday"), names(data))
  factor_terms <- factor_candidates[
    vapply(data[factor_candidates], function(x) dplyr::n_distinct(x, na.rm = TRUE) >= 2, logical(1))
  ]
  as.formula(paste("log_price ~", paste(c(numeric_terms, factor_terms), collapse = " + ")))
}

prepare_model_frame <- function(data, formula, factor_levels = NULL) {
  factor_cols <- intersect(c("source", "district_name", "category_name", "model_category", "area_band", "distance_band", "posted_wday"), all.vars(formula))
  if (is.null(factor_levels)) {
    factor_levels <- lapply(factor_cols, function(col) levels(droplevels(as.factor(data[[col]]))))
    names(factor_levels) <- factor_cols
  }

  for (col in factor_cols) {
    data[[col]] <- factor(as.character(data[[col]]), levels = factor_levels[[col]])
  }

  list(data = data, factor_levels = factor_levels)
}

make_xgb_matrix <- function(formula, data, feature_names = NULL) {
  rhs_formula <- delete.response(terms(formula))
  matrix <- sparse.model.matrix(rhs_formula, data = data)[, -1, drop = FALSE]
  if (!is.null(feature_names)) {
    missing_cols <- setdiff(feature_names, colnames(matrix))
    if (length(missing_cols) > 0) {
      zeros <- Matrix::sparseMatrix(
        i = integer(0),
        j = integer(0),
        dims = c(nrow(matrix), length(missing_cols))
      )
      colnames(zeros) <- missing_cols
      matrix <- cbind(matrix, zeros)
    }
    extra_cols <- setdiff(colnames(matrix), feature_names)
    if (length(extra_cols) > 0) {
      matrix <- matrix[, setdiff(colnames(matrix), extra_cols), drop = FALSE]
    }
    matrix <- matrix[, feature_names, drop = FALSE]
  }
  matrix
}

xgb_param_candidates <- list(
  list(nrounds = 220, learning_rate = 0.08, max_depth = 5, min_child_weight = 1, subsample = 0.85, colsample_bytree = 0.85),
  list(nrounds = 300, learning_rate = 0.05, max_depth = 6, min_child_weight = 1, subsample = 0.80, colsample_bytree = 0.80),
  list(nrounds = 420, learning_rate = 0.03, max_depth = 5, min_child_weight = 1, subsample = 0.85, colsample_bytree = 0.85),
  list(nrounds = 260, learning_rate = 0.06, max_depth = 4, min_child_weight = 3, subsample = 0.90, colsample_bytree = 0.90),
  list(nrounds = 240, learning_rate = 0.05, max_depth = 7, min_child_weight = 2, subsample = 0.80, colsample_bytree = 0.80),
  list(nrounds = 180, learning_rate = 0.10, max_depth = 4, min_child_weight = 1, subsample = 0.90, colsample_bytree = 0.75),
  list(nrounds = 500, learning_rate = 0.025, max_depth = 6, min_child_weight = 2, subsample = 0.85, colsample_bytree = 0.85),
  list(nrounds = 360, learning_rate = 0.04, max_depth = 7, min_child_weight = 1, subsample = 0.78, colsample_bytree = 0.82)
)

if (MODEL_FAST) {
  xgb_param_candidates <- lapply(xgb_param_candidates[seq_len(min(2, length(xgb_param_candidates)))], function(params) {
    params$nrounds <- min(params$nrounds, 80)
    params
  })
}

train_xgb_model <- function(x, y, params) {
  xgboost(
    x = x,
    y = y,
    nrounds = params$nrounds,
    objective = "reg:squarederror",
    learning_rate = params$learning_rate,
    max_depth = params$max_depth,
    min_child_weight = params$min_child_weight,
    subsample = params$subsample,
    colsample_bytree = params$colsample_bytree,
    nthread = 1,
    verbosity = 0
  )
}

tune_xgb_model <- function(x_train, y_train, x_test, y_test, pred_bounds) {
  best <- NULL
  for (params in xgb_param_candidates) {
    model <- train_xgb_model(x_train, y_train, params)
    pred <- predict(model, x_test)
    pred <- pmin(pmax(pred, pred_bounds[[1]]), pred_bounds[[2]])
    actual <- expm1(y_test)
    predicted <- expm1(pred)
    mape_score <- mean(abs((actual - predicted) / actual), na.rm = TRUE)
    rmse_score <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
    if (is.null(best) || mape_score < best$mape || (identical(mape_score, best$mape) && rmse_score < best$rmse)) {
      best <- list(model = model, pred = pred, params = params, mape = mape_score, rmse = rmse_score)
    }
  }
  best
}

tune_rf_xgb_ensemble <- function(rf_pred, xgb_pred, actual_log) {
  best <- NULL
  for (weight_rf in seq(0, 1, by = 0.05)) {
    pred <- weight_rf * rf_pred + (1 - weight_rf) * xgb_pred
    actual <- expm1(actual_log)
    predicted <- expm1(pred)
    mape_score <- mean(abs((actual - predicted) / actual), na.rm = TRUE)
    rmse_score <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
    if (is.null(best) || mape_score < best$mape || (identical(mape_score, best$mape) && rmse_score < best$rmse)) {
      best <- list(weight_rf = weight_rf, pred = pred, mape = mape_score, rmse = rmse_score)
    }
  }
  best
}

train_regression_set <- function(data, segment_name) {
  if (nrow(data) < 20) {
    warning("Bo qua ", segment_name, ": qua it dong de train.")
    return(NULL)
  }

  data <- mark_model_scope(data, segment_name)
  if (nrow(data) < 20) {
    warning("Bo qua ", segment_name, ": qua it dong sau khi loc ngoai le cho model.")
    return(NULL)
  }

  split <- make_split(data)
  train <- droplevels(split$train)
  test <- droplevels(split$test)
  price_encodings <- fit_price_encodings(train)
  train <- apply_price_encodings(train, price_encodings)
  test <- apply_price_encodings(test, price_encodings)
  formula <- make_formula(train)
  prepared <- prepare_model_frame(train, formula)
  train <- droplevels(prepared$data)
  factor_levels <- prepared$factor_levels
  test <- prepare_model_frame(test, formula, factor_levels)$data
  factor_cols <- names(factor_levels)
  valid_terms <- all.vars(formula)[-1]
  invalid_factors <- factor_cols[
    vapply(factor_cols, function(col) nlevels(train[[col]]) < 2, logical(1))
  ]
  valid_terms <- setdiff(valid_terms, invalid_factors)
  if (length(valid_terms) == 0) {
    warning("Bo qua ", segment_name, ": khong con bien hop le de train.")
    return(NULL)
  }
  formula <- as.formula(paste("log_price ~", paste(valid_terms, collapse = " + ")))
  model_vars <- all.vars(formula)
  train <- train[complete.cases(train[, model_vars]), ]
  test <- test[complete.cases(test[, model_vars]), ]
  train <- droplevels(train)
  prepared <- prepare_model_frame(train, formula)
  train <- droplevels(prepared$data)
  factor_levels <- prepared$factor_levels
  test <- prepare_model_frame(test, formula, factor_levels)$data
  factor_cols <- names(factor_levels)
  valid_terms <- all.vars(formula)[-1]
  invalid_factors <- factor_cols[
    vapply(factor_cols, function(col) nlevels(train[[col]]) < 2, logical(1))
  ]
  valid_terms <- setdiff(valid_terms, invalid_factors)
  if (length(valid_terms) == 0) {
    warning("Bo qua ", segment_name, ": khong con bien hop le de train sau khi loc NA.")
    return(NULL)
  }
  formula <- as.formula(paste("log_price ~", paste(valid_terms, collapse = " + ")))
  model_vars <- all.vars(formula)
  train <- train[complete.cases(train[, model_vars]), ]
  test <- test[complete.cases(test[, model_vars]), ]
  if (nrow(train) < 10 || nrow(test) < 2) {
    warning("Bo qua ", segment_name, ": train/test sau khi loc qua it dong.")
    return(NULL)
  }

  lm_model <- lm(formula, data = train)
  lm_pred <- suppressWarnings(predict(lm_model, newdata = test))

  rf_model <- randomForest(
    formula,
    data = train,
    ntree = RF_NTREE,
    importance = TRUE
  )
  rf_pred <- predict(rf_model, newdata = test)

  x_train <- make_xgb_matrix(formula, train)
  x_test <- make_xgb_matrix(formula, test, colnames(x_train))

  pred_bounds <- quantile(train$log_price, probs = c(0.01, 0.99), na.rm = TRUE)
  tuned_xgb <- tune_xgb_model(x_train, train$log_price, x_test, test$log_price, pred_bounds)
  xgb_model <- tuned_xgb$model
  xgb_pred <- tuned_xgb$pred
  xgb_params <- tuned_xgb$params

  clamp_pred <- function(x) pmin(pmax(x, pred_bounds[[1]]), pred_bounds[[2]])
  lm_pred <- clamp_pred(lm_pred)
  rf_pred <- clamp_pred(rf_pred)
  ensemble_pred <- (rf_pred + xgb_pred) / 2
  tuned_ensemble <- tune_rf_xgb_ensemble(rf_pred, xgb_pred, test$log_price)
  tuned_ensemble_pred <- tuned_ensemble$pred

  metrics <- bind_rows(
    eval_price(test$log_price, lm_pred) %>% mutate(model = "Linear Regression"),
    eval_price(test$log_price, rf_pred) %>% mutate(model = "Random Forest"),
    eval_price(test$log_price, xgb_pred) %>% mutate(model = "XGBoost"),
    eval_price(test$log_price, ensemble_pred) %>% mutate(model = "RF + XGBoost Ensemble"),
    eval_price(test$log_price, tuned_ensemble_pred) %>% mutate(model = "Tuned RF/XGBoost Ensemble")
  ) %>%
    mutate(
      segment = segment_name,
      split_type = split$split_type,
      train_rows = nrow(train),
      test_rows = nrow(test),
      .before = 1
    ) %>%
    select(segment, model, everything())

  best_model <- metrics %>% arrange(mape, rmse_vnd) %>% slice(1) %>% pull(model)
  prediction_lookup <- list(
    "Linear Regression" = lm_pred,
    "Random Forest" = rf_pred,
    "XGBoost" = xgb_pred,
    "RF + XGBoost Ensemble" = ensemble_pred,
    "Tuned RF/XGBoost Ensemble" = tuned_ensemble_pred
  )
  best_pred <- prediction_lookup[[best_model]]

  final_train <- droplevels(data)
  final_price_encodings <- fit_price_encodings(final_train)
  final_train <- apply_price_encodings(final_train, final_price_encodings)
  final_formula <- make_formula(final_train)
  final_prepared <- prepare_model_frame(final_train, final_formula)
  final_train <- droplevels(final_prepared$data)
  final_factor_levels <- final_prepared$factor_levels

  final_factor_cols <- names(final_factor_levels)
  final_valid_terms <- all.vars(final_formula)[-1]
  final_invalid_factors <- final_factor_cols[
    vapply(final_factor_cols, function(col) nlevels(final_train[[col]]) < 2, logical(1))
  ]
  final_valid_terms <- setdiff(final_valid_terms, final_invalid_factors)
  if (length(final_valid_terms) == 0) {
    warning("Bo qua refit ", segment_name, ": khong con bien hop le de train full.")
    return(metrics)
  }

  final_formula <- as.formula(paste("log_price ~", paste(final_valid_terms, collapse = " + ")))
  final_model_vars <- all.vars(final_formula)
  final_train <- final_train[complete.cases(final_train[, final_model_vars]), ]
  final_train <- droplevels(final_train)
  final_prepared <- prepare_model_frame(final_train, final_formula)
  final_train <- droplevels(final_prepared$data)
  final_factor_levels <- final_prepared$factor_levels

  lm_model <- lm(final_formula, data = final_train)
  rf_model <- randomForest(
    final_formula,
    data = final_train,
    ntree = RF_NTREE,
    importance = TRUE
  )
  x_train <- make_xgb_matrix(final_formula, final_train)
  xgb_model <- train_xgb_model(x_train, final_train$log_price, xgb_params)
  pred_bounds <- quantile(final_train$log_price, probs = c(0.01, 0.99), na.rm = TRUE)
  formula <- final_formula
  factor_levels <- final_factor_levels
  price_encodings <- final_price_encodings

  rf_importance <- randomForest::importance(rf_model) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("feature") %>%
    arrange(desc(IncNodePurity))

  importance_path <- if (identical(segment_name, "rent")) {
    PATHS$rf_importance_rent_csv
  } else {
    PATHS$rf_importance_sale_csv
  }
  write_csv(rf_importance, importance_path)
  segment_file_label <- if (identical(segment_name, "sale")) "ban" else "thue"

  segment_label <- if (identical(segment_name, "sale")) "bán" else "cho thuê"

  p_importance <- rf_importance %>%
    slice_head(n = 12) %>%
    mutate(feature_label = feature_label_vi(feature)) %>%
    ggplot(aes(x = reorder(feature_label, IncNodePurity), y = IncNodePurity)) +
    geom_col(fill = "#2E75B6") +
    coord_flip() +
    labs(title = paste("Mức ảnh hưởng biến - nhóm", segment_label), x = NULL, y = "Mức ảnh hưởng") +
    theme_minimal(base_size = 12)
  ggsave(file.path(PLOT_DIR, paste0("do_quan_trong_bien_", segment_file_label, ".png")),
         p_importance, width = 9, height = 6, dpi = 160)

  actual_price <- expm1(test$log_price)
  predicted_price <- pmax(expm1(best_pred), 1)
  validation_predictions <- tibble(
    segment = segment_name,
    best_model = best_model,
    actual_price = actual_price,
    predicted_price = predicted_price,
    abs_error_vnd = abs(actual_price - predicted_price),
    ape = abs(actual_price - predicted_price) / actual_price,
    source = as.character(test$source),
    district_name = as.character(test$district_name),
    category_name = as.character(test$category_name),
    model_category = as.character(test$model_category),
    ward = as.character(test$ward),
    area = as.numeric(test$area),
    rooms = as.numeric(test$rooms),
    price_per_m2 = as.numeric(test$price_per_m2)
  )
  validation_path <- file.path(MODEL_DIR, paste0("du_doan_kiem_dinh_", segment_file_label, ".csv"))
  write_csv(validation_predictions, validation_path)

  pred_plot <- validation_predictions %>%
    ggplot(aes(x = actual_price / 1e6, y = predicted_price / 1e6)) +
    geom_point(alpha = 0.45, color = "#2E75B6") +
    geom_abline(slope = 1, intercept = 0, color = "#C0504D") +
    scale_x_log10(labels = function(x) format(round(x, 1), big.mark = ".", decimal.mark = ",", trim = TRUE)) +
    scale_y_log10(labels = function(x) format(round(x, 1), big.mark = ".", decimal.mark = ",", trim = TRUE)) +
    labs(title = paste("Giá thực tế và giá dự đoán - nhóm", segment_label),
         subtitle = paste("Mô hình được chọn:", model_label_vi(best_model)),
         x = "Giá thực tế (triệu VND)", y = "Giá dự đoán (triệu VND)") +
    theme_minimal(base_size = 12)
  ggsave(file.path(PLOT_DIR, paste0("du_doan_so_voi_thuc_te_", segment_file_label, ".png")),
         pred_plot, width = 8, height = 6, dpi = 160)

  model_output_path <- if (identical(segment_name, "sale")) {
    PATHS$sale_model_rds
  } else {
    PATHS$rent_model_rds
  }

  saveRDS(
    list(
      segment = segment_name,
      formula = formula,
      lm = lm_model,
      random_forest = rf_model,
      xgboost = xgb_model,
      xgb_params = xgb_params,
      best_model = best_model,
      ensemble_weight_rf = tuned_ensemble$weight_rf,
      factor_levels = factor_levels,
      ward_encoding = price_encodings$ward,
      target_encodings = price_encodings,
      xgb_feature_names = colnames(x_train),
      train_log_bounds = as.numeric(pred_bounds),
      validation_predictions_path = validation_path,
      model_scope = "core_market_after_group_outlier_filter",
      trained_at = as.character(Sys.time()),
      split_type = split$split_type,
      train_rows = nrow(final_train),
      validation_train_rows = nrow(train),
      validation_test_rows = nrow(test),
      metrics = metrics
    ),
    model_output_path
  )

  metrics
}

run_train_models <- function() {
sale_metrics <- train_regression_set(df %>% filter(!is_rent), "sale")
rent_metrics <- train_regression_set(df %>% filter(is_rent), "rent")

cluster_df <- df %>%
  filter(!is.na(price_per_m2), price_per_m2 > 0) %>%
  group_by(transaction_type, district_name, category_name) %>%
  summarise(
    median_price_per_m2 = median(price_per_m2, na.rm = TRUE),
    median_area = median(area, na.rm = TRUE),
    listing_count = n(),
    .groups = "drop"
  ) %>%
  mutate(min_cluster_listing = if_else(as.character(transaction_type) == "Cho thuê", 2L, 5L)) %>%
  filter(listing_count >= min_cluster_listing) %>%
  select(-min_cluster_listing)

cluster_results <- list()
cluster_models <- list()
for (tx in unique(as.character(cluster_df$transaction_type))) {
  tx_df <- cluster_df %>% filter(as.character(transaction_type) == tx)
  if (nrow(tx_df) < 4) next

  cluster_input <- scale(tx_df %>%
                           select(median_price_per_m2, median_area, listing_count))
  k <- min(4, nrow(tx_df))
  kmeans_model <- kmeans(cluster_input, centers = k, nstart = 25)
  cluster_models[[tx]] <- kmeans_model
  cluster_results[[tx]] <- tx_df %>%
    mutate(cluster = as.factor(kmeans_model$cluster))
}

if (length(cluster_results) > 0) {
  cluster_result <- bind_rows(cluster_results)
  saveRDS(cluster_models, PATHS$kmeans_model_rds)
  write_csv(cluster_result, PATHS$clusters_csv)
}

all_metrics <- bind_rows(sale_metrics, rent_metrics)
write_csv(all_metrics, PATHS$metrics_csv)

if (nrow(model_scope_audit_rows) > 0) {
  write_csv(model_scope_audit_rows, file.path(MODEL_DIR, "pham_vi_mo_hinh_audit.csv"))
}

registry <- all_metrics %>%
  group_by(segment) %>%
  arrange(mape, rmse_vnd, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    segment,
    best_model = model,
    mape,
    median_ape,
    p90_ape,
    rmse_vnd,
    mae_vnd,
    r2,
    split_type,
    train_rows,
    test_rows,
    trained_at = as.character(Sys.time())
  )
write_csv(registry, REGISTRY_PATH)

saveRDS(
  list(
    trained_at = as.character(Sys.time()),
    data_path = DATA_PATH,
    total_rows = nrow(df),
    sale_rows = sum(!df$is_rent, na.rm = TRUE),
    rent_rows = sum(df$is_rent, na.rm = TRUE),
    model_scope_audit = model_scope_audit_rows,
    registry = registry
  ),
  METADATA_PATH
)

cat("\nĐã huấn luyện xong mô hình.\n")
cat("Chỉ số lưu tại: ", PATHS$metrics_csv, "\n", sep = "")
print(all_metrics)

if (exists("cluster_result")) {
  cat("\nCụm K-means lưu tại: ", PATHS$clusters_csv, "\n", sep = "")
  print(cluster_result)
}
invisible(list(metrics = all_metrics, registry = registry))
}

if (sys.nframe() == 0) {
  run_train_models()
}
