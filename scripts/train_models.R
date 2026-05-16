# ============================================================
# TRAIN MODELS - BDS TP.HCM
# Chay sau khi co data:
#   Rscript feature_engineering.R
#   Rscript train_models.R
# Output: models/*.rds, models/model_metrics.csv, plots/*.png
# ============================================================

if (dir.exists("R_libs")) .libPaths(c(normalizePath("R_libs"), .libPaths()))

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

FEATURED_PATH <- "data/hcmc_bds_featured.csv"
RAW_PATH <- "data/hcmc_bds_raw.csv"
MODEL_DIR <- "models"
PLOT_DIR <- "plots"
METADATA_PATH <- file.path(MODEL_DIR, "model_metadata.rds")
REGISTRY_PATH <- file.path(MODEL_DIR, "model_registry.csv")
dir.create(MODEL_DIR, showWarnings = FALSE)
dir.create(PLOT_DIR, showWarnings = FALSE)

if (file.exists(FEATURED_PATH)) {
  DATA_PATH <- FEATURED_PATH
} else if (file.exists(RAW_PATH)) {
  DATA_PATH <- RAW_PATH
} else {
  stop("Chua co file data. Hay chay chotot_scraper_v3.R va feature_engineering.R truoc.")
}

set.seed(42)

raw_df <- read_csv(DATA_PATH, show_col_types = FALSE)
if (!"price_per_m2" %in% names(raw_df)) raw_df$price_per_m2 <- NA_real_
if (!"posted_hour" %in% names(raw_df)) raw_df$posted_hour <- NA_integer_
if (!"posted_wday" %in% names(raw_df)) raw_df$posted_wday <- NA_character_
if (!"is_rent" %in% names(raw_df)) raw_df$is_rent <- NA
if (!"log_price" %in% names(raw_df)) raw_df$log_price <- NA_real_
if (!"distance_to_center" %in% names(raw_df)) raw_df$distance_to_center <- NA_real_
if (!"ward_price_encoded" %in% names(raw_df)) raw_df$ward_price_encoded <- NA_real_
if (!"source" %in% names(raw_df)) raw_df$source <- "unknown"
if (!"transaction_type" %in% names(raw_df)) raw_df$transaction_type <- NA_character_

df <- raw_df %>%
  mutate(
    category_id = as.character(category_id),
    source = as.factor(if_else(is.na(source) | source == "", "unknown", as.character(source))),
    district_name = as.factor(district_name),
    category_name = as.factor(category_name),
    ward = as.factor(if_else(is.na(ward) | ward == "", "Unknown", as.character(ward))),
    rooms = if_else(is.na(rooms), 0, as.numeric(rooms)),
    area = if_else(is.na(area), median(area, na.rm = TRUE), as.numeric(area)),
    posted_at = suppressWarnings(as_datetime(posted_at)),
    posted_hour = if_else(is.na(as.integer(posted_hour)), hour(posted_at), as.integer(posted_hour)),
    posted_wday = if_else(is.na(as.character(posted_wday)), as.character(wday(posted_at, label = TRUE)), as.character(posted_wday)),
    posted_wday = as.factor(posted_wday),
    is_rent = if_else(is.na(as.logical(is_rent)), category_id %in% c("1030", "1050"), as.logical(is_rent)),
    transaction_type = as.factor(if_else(is_rent, "Cho thuê", "Bán")),
    log_price = if_else(is.na(log_price), log1p(price), log_price),
    price_per_m2 = if_else(!is.na(price_per_m2), price_per_m2,
                           if_else(!is.na(area) & area > 0, price / area, NA_real_)),
    distance_to_center = if_else(is.na(distance_to_center), median(distance_to_center, na.rm = TRUE), distance_to_center),
    ward_price_encoded = if_else(is.na(ward_price_encoded), log1p(price), ward_price_encoded)
  ) %>%
  filter(!is.na(price), price > 0, !is.na(area), area > 0, !is.na(log_price))

if (all(is.na(df$distance_to_center))) {
  df$distance_to_center <- 0
}

make_split <- function(data, train_ratio = 0.8) {
  data <- data %>%
    mutate(
      split_time = coalesce(
        suppressWarnings(as_datetime(posted_at)),
        if ("scraped_at" %in% names(.)) suppressWarnings(as_datetime(scraped_at)) else as_datetime(NA)
      )
    )

  if (sum(!is.na(data$split_time)) >= 20) {
    data <- data %>% arrange(split_time)
    train_n <- floor(nrow(data) * train_ratio)
    return(list(
      train = data[seq_len(train_n), ],
      test = data[(train_n + 1):nrow(data), ],
      split_type = "time_based"
    ))
  }

  idx <- sample(seq_len(nrow(data)), size = floor(nrow(data) * train_ratio))
  list(train = data[idx, ], test = data[-idx, ], split_type = "random_fallback")
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
  tibble(
    rmse_vnd = rmse(actual, pred),
    mae_vnd = mae(actual, pred),
    mape = mean(abs((actual - pred) / actual), na.rm = TRUE),
    r2 = r2_score(actual, pred)
  )
}

fit_target_encoding <- function(train, group_col, target_col = "price", min_count = 5, smoothing = 10) {
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

make_formula <- function(data) {
  numeric_candidates <- intersect(
    c("area", "rooms", "posted_hour", "distance_to_center", "ward_price_encoded", "listing_age_days"),
    names(data)
  )
  numeric_terms <- numeric_candidates[
    vapply(data[numeric_candidates], function(x) sum(!is.na(x)) > 0, logical(1))
  ]
  factor_candidates <- intersect(c("district_name", "category_name", "posted_wday", "source"), names(data))
  factor_terms <- factor_candidates[
    vapply(data[factor_candidates], function(x) dplyr::n_distinct(x, na.rm = TRUE) >= 2, logical(1))
  ]
  as.formula(paste("log_price ~", paste(c(numeric_terms, factor_terms), collapse = " + ")))
}

prepare_model_frame <- function(data, formula, factor_levels = NULL) {
  factor_cols <- intersect(c("district_name", "category_name", "posted_wday", "source"), all.vars(formula))
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

train_regression_set <- function(data, segment_name) {
  if (nrow(data) < 20) {
    warning("Bo qua ", segment_name, ": qua it dong de train.")
    return(NULL)
  }

  split <- make_split(data)
  train <- droplevels(split$train)
  test <- droplevels(split$test)
  ward_encoding <- fit_target_encoding(train, "ward")
  train <- apply_target_encoding(train, ward_encoding)
  test <- apply_target_encoding(test, ward_encoding)
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
  lm_pred <- predict(lm_model, newdata = test)

  rf_model <- randomForest(
    formula,
    data = train,
    ntree = 300,
    importance = TRUE
  )
  rf_pred <- predict(rf_model, newdata = test)

  x_train <- make_xgb_matrix(formula, train)
  x_test <- make_xgb_matrix(formula, test, colnames(x_train))

  xgb_model <- xgboost(
    x = x_train,
    y = train$log_price,
    nrounds = 250,
    objective = "reg:squarederror",
    learning_rate = 0.05,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8,
    nthread = 1,
    verbosity = 0
  )
  xgb_pred <- predict(xgb_model, x_test)

  pred_bounds <- quantile(train$log_price, probs = c(0.01, 0.99), na.rm = TRUE)
  clamp_pred <- function(x) pmin(pmax(x, pred_bounds[[1]]), pred_bounds[[2]])
  lm_pred <- clamp_pred(lm_pred)
  rf_pred <- clamp_pred(rf_pred)
  xgb_pred <- clamp_pred(xgb_pred)
  ensemble_pred <- (rf_pred + xgb_pred) / 2

  metrics <- bind_rows(
    eval_price(test$log_price, lm_pred) %>% mutate(model = "Linear Regression"),
    eval_price(test$log_price, rf_pred) %>% mutate(model = "Random Forest"),
    eval_price(test$log_price, xgb_pred) %>% mutate(model = "XGBoost"),
    eval_price(test$log_price, ensemble_pred) %>% mutate(model = "RF + XGBoost Ensemble")
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

  rf_importance <- randomForest::importance(rf_model) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("feature") %>%
    arrange(desc(IncNodePurity))

  write_csv(rf_importance, file.path(MODEL_DIR, paste0("rf_importance_", segment_name, ".csv")))

  p_importance <- rf_importance %>%
    slice_head(n = 12) %>%
    ggplot(aes(x = reorder(feature, IncNodePurity), y = IncNodePurity)) +
    geom_col(fill = "#2E75B6") +
    coord_flip() +
    labs(title = paste("Random Forest importance -", segment_name), x = NULL, y = "IncNodePurity") +
    theme_minimal(base_size = 12)
  ggsave(file.path(PLOT_DIR, paste0("rf_importance_", segment_name, ".png")),
         p_importance, width = 9, height = 6, dpi = 160)

  pred_plot <- tibble(actual = expm1(test$log_price), predicted = expm1(xgb_pred)) %>%
    ggplot(aes(x = actual / 1e6, y = predicted / 1e6)) +
    geom_point(alpha = 0.45, color = "#2E75B6") +
    geom_abline(slope = 1, intercept = 0, color = "#C0504D") +
    labs(title = paste("Actual vs Predicted -", segment_name),
         x = "Actual (trieu VND)", y = "Predicted (trieu VND)") +
    theme_minimal(base_size = 12)
  ggsave(file.path(PLOT_DIR, paste0("actual_vs_predicted_", segment_name, ".png")),
         pred_plot, width = 8, height = 6, dpi = 160)

  saveRDS(
    list(
      segment = segment_name,
      formula = formula,
      lm = lm_model,
      random_forest = rf_model,
      xgboost = xgb_model,
      best_model = best_model,
      factor_levels = factor_levels,
      ward_encoding = ward_encoding,
      xgb_feature_names = colnames(x_train),
      train_log_bounds = as.numeric(pred_bounds),
      trained_at = as.character(Sys.time()),
      split_type = split$split_type,
      train_rows = nrow(train),
      test_rows = nrow(test),
      metrics = metrics
    ),
    file.path(MODEL_DIR, paste0("price_models_", segment_name, ".rds"))
  )

  metrics
}

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
  saveRDS(cluster_models, file.path(MODEL_DIR, "kmeans_area_price.rds"))
  write_csv(cluster_result, file.path(MODEL_DIR, "kmeans_area_price.csv"))
}

all_metrics <- bind_rows(sale_metrics, rent_metrics)
write_csv(all_metrics, file.path(MODEL_DIR, "model_metrics.csv"))

registry <- all_metrics %>%
  group_by(segment) %>%
  arrange(mape, rmse_vnd, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    segment,
    best_model = model,
    mape,
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
    registry = registry
  ),
  METADATA_PATH
)

cat("\nDa train xong model.\n")
cat("Metrics luu tai: models/model_metrics.csv\n")
print(all_metrics)

if (exists("cluster_result")) {
  cat("\nK-means clusters luu tai: models/kmeans_area_price.csv\n")
  print(cluster_result)
}
