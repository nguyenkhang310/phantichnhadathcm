# ============================================================
# PPT CODE - MO HINH HOA THUC NGHIEM
# Ban rut gon de chup slide, tom tat tu pipeline R/Shiny chinh.
# ============================================================

# 1. Tien xu ly du lieu dau vao
df_model <- raw_df %>%
  mutate(
    is_rent = category_id %in% c("1030", "1050"),
    transaction_type = if_else(is_rent, "Cho thue", "Ban"),
    log_price = log1p(price),
    log_area = log1p(area),
    price_per_m2 = price / area
  ) %>%
  filter(price > 0, area > 0, !is.na(log_price)) %>%
  filter(
    (is_rent & price >= 300000 & price <= 2e9) |
      (!is_rent & price >= 300000000 & price <= 500e9)
  )

# 2. Chia train/test va tinh chi so danh gia
set.seed(42)
split <- make_split(df_model, train_ratio = 0.8)
train <- split$train
test <- split$test

eval_price <- function(actual_log, pred_log) {
  actual <- expm1(actual_log)
  pred <- expm1(pred_log)
  tibble(
    rmse_vnd = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
    mae_vnd = mean(abs(actual - pred), na.rm = TRUE),
    mape = mean(abs((actual - pred) / actual), na.rm = TRUE)
  )
}

# 3. Huan luyen cac mo hinh hoi quy
formula <- log_price ~ log_area + rooms + district_name + category_name + source

lm_model <- lm(formula, data = train)
rf_model <- randomForest(formula, data = train, ntree = 500, importance = TRUE)

x_train <- make_xgb_matrix(formula, train)
x_test <- make_xgb_matrix(formula, test, colnames(x_train))
xgb_model <- train_xgb_model(x_train, train$log_price, xgb_params)

pred_lm <- predict(lm_model, test)
pred_rf <- predict(rf_model, test)
pred_xgb <- predict(xgb_model, x_test)
pred_ensemble <- 0.5 * pred_rf + 0.5 * pred_xgb

# 4. So sanh va chon model tot nhat
metrics <- bind_rows(
  eval_price(test$log_price, pred_lm) %>% mutate(model = "Linear Regression"),
  eval_price(test$log_price, pred_rf) %>% mutate(model = "Random Forest"),
  eval_price(test$log_price, pred_xgb) %>% mutate(model = "XGBoost"),
  eval_price(test$log_price, pred_ensemble) %>% mutate(model = "RF + XGBoost Ensemble")
) %>%
  arrange(mape, rmse_vnd)

best_model <- metrics %>% slice(1) %>% pull(model)
write_csv(metrics, "models/chi_so_mo_hinh.csv")

# 5. Luu model de dashboard su dung lai
saveRDS(
  list(
    formula = formula,
    lm = lm_model,
    random_forest = rf_model,
    xgboost = xgb_model,
    best_model = best_model,
    metrics = metrics
  ),
  "models/mo_hinh_gia_ban.rds"
)

# 6. Dashboard goi model va tra ve gia du doan
predict_price <- function(input_row, model_path) {
  bundle <- readRDS(model_path)

  pred_log <- switch(
    bundle$best_model,
    "Linear Regression" = predict(bundle$lm, input_row),
    "Random Forest" = predict(bundle$random_forest, input_row),
    "XGBoost" = predict(bundle$xgboost, make_xgb_matrix(bundle$formula, input_row)),
    "RF + XGBoost Ensemble" = {
      rf <- predict(bundle$random_forest, input_row)
      xgb <- predict(bundle$xgboost, make_xgb_matrix(bundle$formula, input_row))
      0.5 * rf + 0.5 * xgb
    }
  )

  expm1(as.numeric(pred_log))
}
