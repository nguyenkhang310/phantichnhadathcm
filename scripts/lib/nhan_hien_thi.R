DISPLAY_FEATURE_LABELS_VI <- c(
  log_price = "Giá niêm yết (log)",
  log_price_m2 = "Giá/m² (log)",
  log_area = "Diện tích (log)",
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
  title_has_building = "Tín hiệu tòa nhà/văn phòng",
  title_has_business = "Tín hiệu kinh doanh",
  title_has_land_signal = "Tín hiệu đất/nền",
  title_has_apartment_signal = "Tín hiệu căn hộ",
  title_has_room_signal = "Tín hiệu phòng trọ",
  title_has_luxury = "Tín hiệu cao cấp",
  title_has_urgent = "Tín hiệu bán/thuê gấp",
  title_token_count = "Độ chi tiết tin",
  posted_hour = "Giờ đăng tin",
  distance_to_center = "Cách trung tâm",
  log_distance_to_center = "Cách trung tâm (log)",
  floor_area_proxy = "Diện tích sàn ước lượng",
  log_floor_area_proxy = "Diện tích sàn ước lượng (log)",
  room_density = "Mật độ phòng/diện tích",
  area_per_room = "Diện tích trên mỗi phòng",
  log_area_per_room = "Diện tích/phòng (log)",
  ward_price_encoded = "Giá phường/xã",
  district_price_encoded = "Giá khu vực",
  category_price_encoded = "Giá loại BĐS",
  source_price_encoded = "Giá theo nguồn",
  district_category_price_encoded = "Giá khu vực + loại",
  source_category_price_encoded = "Giá nguồn + loại",
  model_category_price_encoded = "Giá nhóm sản phẩm mô hình",
  district_model_category_price_encoded = "Giá khu vực + nhóm sản phẩm",
  listing_age_days = "Tuổi tin đăng",
  district_name = "Khu vực",
  category_name = "Loại bất động sản",
  model_category = "Nhóm sản phẩm mô hình",
  area_band = "Nhóm diện tích",
  distance_band = "Nhóm khoảng cách",
  posted_wday = "Thứ đăng tin",
  source = "Nguồn dữ liệu"
)

DISPLAY_MODEL_LABELS_VI <- c(
  "Linear Regression" = "Hồi quy tuyến tính",
  "Random Forest" = "Random Forest",
  "XGBoost" = "XGBoost",
  "RF + XGBoost Ensemble" = "Tổ hợp RF + XGBoost",
  "Tuned RF/XGBoost Ensemble" = "Tổ hợp RF/XGBoost tối ưu"
)

label_from_lookup <- function(x, lookup) {
  x_chr <- as.character(x)
  labels <- unname(lookup[x_chr])
  missing_label <- is.na(labels)
  labels[missing_label] <- x_chr[missing_label]
  labels
}

feature_label_vi <- function(feature) {
  label_from_lookup(feature, DISPLAY_FEATURE_LABELS_VI)
}

model_label_vi <- function(model_name) {
  label_from_lookup(model_name, DISPLAY_MODEL_LABELS_VI)
}
