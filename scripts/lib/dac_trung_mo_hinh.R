model_strip_vietnamese <- function(x) {
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

model_text_key <- function(x) {
  x <- model_strip_vietnamese(x)
  x <- gsub("[^a-z0-9 ]+", " ", x)
  gsub("\\s+", " ", trimws(x))
}

model_has_pattern <- function(text, pattern) {
  as.integer(grepl(pattern, text, perl = TRUE))
}

model_pick_column <- function(data, col, default) {
  if (col %in% names(data)) data[[col]] else rep(default, nrow(data))
}

add_model_quality_features <- function(data) {
  if (nrow(data) == 0) return(data)

  title <- model_pick_column(data, "title", "")
  address <- model_pick_column(data, "address", "")
  category_name <- model_pick_column(data, "category_name", "Không rõ")
  transaction_type <- model_pick_column(data, "transaction_type", NA_character_)
  is_rent <- suppressWarnings(as.logical(model_pick_column(data, "is_rent", NA)))
  is_rent <- ifelse(is.na(is_rent), as.character(transaction_type) == "Cho thuê", is_rent)

  text_key <- model_text_key(paste(title, address, category_name))
  category_key <- model_text_key(category_name)
  area <- suppressWarnings(as.numeric(model_pick_column(data, "area", NA_real_)))
  rooms <- suppressWarnings(as.numeric(model_pick_column(data, "rooms", NA_real_)))
  inferred_rooms <- suppressWarnings(as.numeric(model_pick_column(data, "inferred_rooms", rooms)))
  inferred_floors <- suppressWarnings(as.numeric(model_pick_column(data, "inferred_floors", NA_real_)))
  distance_to_center <- suppressWarnings(as.numeric(model_pick_column(data, "distance_to_center", NA_real_)))

  title_has_building <- model_has_pattern(
    text_key,
    "\\b(toa nha|nguyen toa|ca toa|building|cao oc|van phong|office|san van phong|mat bang|mbkd|shophouse|shop house)\\b"
  )
  title_has_business <- model_has_pattern(
    text_key,
    "\\b(kinh doanh|khach san|hotel|spa|nha hang|cua hang|mat tien kinh doanh|cho thue lai|dong tien)\\b"
  )
  title_has_land_signal <- model_has_pattern(
    paste(text_key, category_key),
    "\\b(dat|dat nen|lo dat|nen dat|khuon vien)\\b"
  )
  title_has_apartment_signal <- model_has_pattern(
    paste(text_key, category_key),
    "\\b(can ho|chung cu|studio|duplex|penthouse|officetel|condotel)\\b"
  )
  title_has_room_signal <- model_has_pattern(
    paste(text_key, category_key),
    "\\b(phong tro|phong cho thue|nha tro|o ghep|ky tuc|ktx)\\b"
  )
  title_has_luxury <- model_has_pattern(
    paste(text_key, category_key),
    "\\b(biet thu|villa|penthouse|hang sang|cao cap|view song|di san|sieu pham|compound)\\b"
  )
  title_has_urgent <- model_has_pattern(
    text_key,
    "\\b(ban gap|can ban gap|can tien|giam gia|gia tot|gia re|ngop|cat lo)\\b"
  )

  category_is_office <- grepl("van phong|mat bang|m(a|ă)t b(a|ằ)ng|shop|kho|xuong|khach san", category_key, perl = TRUE)
  category_is_apartment <- grepl("can ho|chung cu|officetel|condotel", category_key, perl = TRUE)
  category_is_land <- grepl("\\bdat\\b|dat nen", category_key, perl = TRUE)
  category_is_villa <- grepl("biet thu|villa", category_key, perl = TRUE)
  category_is_room <- grepl("phong tro|nha tro|phong cho thue", category_key, perl = TRUE)
  category_is_warehouse <- grepl("kho|xuong|nha xuong", category_key, perl = TRUE)
  category_is_house <- grepl("nha|nha pho", category_key, perl = TRUE)

  model_category <- dplyr::case_when(
    title_has_building == 1 | category_is_office ~ "Tòa nhà/văn phòng/mặt bằng",
    category_is_warehouse ~ "Kho xưởng",
    title_has_room_signal == 1 | (is_rent & category_is_room & (is.na(area) | area <= 90)) ~ "Phòng trọ",
    title_has_apartment_signal == 1 | category_is_apartment ~ "Căn hộ",
    title_has_land_signal == 1 | category_is_land ~ "Đất/đất nền",
    title_has_luxury == 1 | category_is_villa ~ "Biệt thự/cao cấp",
    category_is_house | grepl("\\bnha\\b|nha pho|mat tien|hem|hxh", text_key, perl = TRUE) ~ "Nhà phố/nhà ở",
    TRUE ~ "Khác"
  )

  area_band <- cut(
    area,
    breaks = c(-Inf, 40, 70, 120, 250, Inf),
    labels = c("<=40m2", "40-70m2", "70-120m2", "120-250m2", ">250m2"),
    right = TRUE
  )
  area_band <- addNA(area_band)
  levels(area_band)[is.na(levels(area_band))] <- "Không rõ"

  distance_band <- cut(
    distance_to_center,
    breaks = c(-Inf, 3, 7, 12, 20, Inf),
    labels = c("0-3km", "3-7km", "7-12km", "12-20km", ">20km"),
    right = TRUE
  )
  distance_band <- addNA(distance_band)
  levels(distance_band)[is.na(levels(distance_band))] <- "Không rõ"

  safe_area <- ifelse(is.na(area) | area <= 0, NA_real_, area)
  safe_rooms <- ifelse(is.na(inferred_rooms) | inferred_rooms <= 0, 1, inferred_rooms)
  safe_floors <- ifelse(is.na(inferred_floors) | inferred_floors <= 0, 1, inferred_floors)
  floor_area_proxy <- safe_area * pmin(safe_floors, 20)

  data %>%
    dplyr::mutate(
      title_has_building = title_has_building,
      title_has_business = title_has_business,
      title_has_land_signal = title_has_land_signal,
      title_has_apartment_signal = title_has_apartment_signal,
      title_has_room_signal = title_has_room_signal,
      title_has_luxury = title_has_luxury,
      title_has_urgent = title_has_urgent,
      model_category = factor(model_category),
      area_band = factor(area_band),
      distance_band = factor(distance_band),
      log_distance_to_center = log1p(ifelse(is.na(distance_to_center) | distance_to_center < 0, 0, distance_to_center)),
      floor_area_proxy = ifelse(is.na(floor_area_proxy) | floor_area_proxy <= 0, safe_area, floor_area_proxy),
      log_floor_area_proxy = log1p(ifelse(is.na(floor_area_proxy) | floor_area_proxy <= 0, safe_area, floor_area_proxy)),
      room_density = ifelse(!is.na(safe_area) & safe_area > 0, pmin(safe_rooms / safe_area, 0.30), 0),
      area_per_room = ifelse(safe_rooms > 0 & !is.na(safe_area), pmin(safe_area / safe_rooms, 1000), safe_area),
      log_area_per_room = log1p(ifelse(is.na(area_per_room) | area_per_room < 0, 0, area_per_room))
    )
}
