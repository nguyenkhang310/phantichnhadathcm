# ============================================================
# CHUẨN HÓA KHU VỰC TP.HCM
# Nhãn khu vực chuẩn dùng chung cho dashboard, model và bộ lọc.
# Tên phường mới năm 2025 được quy về khu vực thị trường cũ.
# ============================================================

# Hàm district_strip_vietnamese: loại dấu tiếng Việt để so khớp văn bản.
district_strip_vietnamese <- function(x) {
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

# Hàm district_missing_label: chuẩn hóa tên quận huyện.
district_missing_label <- function(x) {
  key <- district_strip_vietnamese(trimws(as.character(x)))
  is.na(x) | key %in% c("", "unknown", "khong ro", "na", "nan", "null")
}

# Hàm district_clean_label: chuẩn hóa tên quận huyện.
district_clean_label <- function(x, fallback = "Không rõ") {
  x <- trimws(as.character(x))
  ifelse(district_missing_label(x), fallback, x)
}

canonical_district_labels <- c(
  "Thành phố Thủ Đức",
  paste0("Quận ", c(1, 3, 4, 5, 6, 7, 8, 10, 11, 12)),
  "Quận Bình Tân", "Quận Bình Thạnh", "Quận Gò Vấp",
  "Quận Phú Nhuận", "Quận Tân Bình", "Quận Tân Phú",
  "Huyện Bình Chánh", "Huyện Cần Giờ", "Huyện Củ Chi",
  "Huyện Hóc Môn", "Huyện Nhà Bè",
  "TP Bến Cát cũ", "TP Vũng Tàu cũ", "Bình Dương cũ"
)

district_number_map <- c(
  "12" = "Quận 12",
  "11" = "Quận 11",
  "10" = "Quận 10",
  "8" = "Quận 8",
  "7" = "Quận 7",
  "6" = "Quận 6",
  "5" = "Quận 5",
  "4" = "Quận 4",
  "3" = "Quận 3",
  "1" = "Quận 1"
)

district_name_patterns <- c(
  "huyen\\s+binh\\s+chanh|\\bbinh\\s+chanh\\b" = "Huyện Bình Chánh",
  "huyen\\s+can\\s+gio|\\bcan\\s+gio\\b" = "Huyện Cần Giờ",
  "huyen\\s+cu\\s+chi|\\bcu\\s+chi\\b" = "Huyện Củ Chi",
  "huyen\\s+hoc\\s+mon|\\bhoc\\s+mon\\b" = "Huyện Hóc Môn",
  "huyen\\s+nha\\s+be|\\bnha\\s+be\\b" = "Huyện Nhà Bè",
  "quan\\s+binh\\s+thanh|q\\s*binh\\s+thanh|\\bbinh\\s+thanh\\b" = "Quận Bình Thạnh",
  "quan\\s+binh\\s+tan|q\\s*binh\\s+tan|\\bbinh\\s+tan\\b" = "Quận Bình Tân",
  "quan\\s+go\\s+vap|q\\s*go\\s+vap|\\bgo\\s+vap\\b" = "Quận Gò Vấp",
  "quan\\s+phu\\s+nhuan|q\\s*phu\\s+nhuan|\\bphu\\s+nhuan\\b" = "Quận Phú Nhuận",
  "quan\\s+tan\\s+binh|q\\s*tan\\s+binh|\\btan\\s+binh\\b" = "Quận Tân Bình",
  "quan\\s+tan\\s+phu|q\\s*tan\\s+phu|\\btan\\s+phu\\b" = "Quận Tân Phú"
)

new_ward_old_district_patterns <- c(
  "phuong\\s+(sai\\s+gon|ben\\s+thanh|tan\\s+dinh|cau\\s+ong\\s+lanh)" = "Quận 1",
  "phuong\\s+(khanh\\s+hoi|xom\\s+chieu)" = "Quận 4",
  "phuong\\s+(cho\\s+lon|cho\\s+quan|an\\s+dong)" = "Quận 5",
  "phuong\\s+(binh\\s+tay|binh\\s+phu|phu\\s+lam|binh\\s+tien)" = "Quận 6",
  "phuong\\s+(binh\\s+dong|phu\\s+dinh)" = "Quận 8",
  "phuong\\s+dien\\s+hong" = "Quận 10",
  "phuong\\s+phu\\s+tho" = "Quận 11",
  "phuong\\s+(tay\\s+thanh|tan\\s+son\\s+nhi|phu\\s+tho\\s+hoa|phu\\s+thanh)" = "Quận Tân Phú",
  "phuong\\s+(bay\\s+hien|tan\\s+son\\s+hoa|tan\\s+hoa)" = "Quận Tân Bình",
  "phuong\\s+(hanh\\s+thong|an\\s+hoi|thong\\s+tay\\s+hoi|an\\s+nhon)" = "Quận Gò Vấp",
  "(xa|phuong)\\s+tan\\s+vinh\\s+loc" = "Huyện Bình Chánh",
  "phuong\\s+(an\\s+khanh|thu\\s+thiem|thao\\s+dien|cat\\s+lai|binh\\s+trung|hiep\\s+binh|linh\\s+xuan|long\\s+binh|long\\s+phuoc|phuoc\\s+long|tam\\s+hiep|truong\\s+thanh|thu\\s+duc)" = "Thành phố Thủ Đức",
  "phuong\\s+(thoi\\s+hoa|ben\\s+cat|my\\s+phuoc)" = "TP Bến Cát cũ",
  "phuong\\s+(vung\\s+tau|tam\\s+thang|rach\\s+dua|phuoc\\s+thang)" = "TP Vũng Tàu cũ",
  "phuong\\s+binh\\s+duong" = "Bình Dương cũ"
)

# Hàm canonical_hcmc_district_one: chuẩn hóa tên quận huyện.
canonical_hcmc_district_one <- function(...) {
  parts <- list(...)
  parts <- unlist(parts, use.names = FALSE)
  parts <- parts[!is.na(parts) & trimws(as.character(parts)) != ""]
  if (length(parts) == 0) return("Không rõ")

  text <- district_strip_vietnamese(paste(parts, collapse = " "))
  text <- gsub("[^a-z0-9 ]+", " ", text)
  text <- gsub("\\s+", " ", trimws(text))

  if (district_missing_label(text)) return("Không rõ")

  if (grepl("\\b(tp|thanh\\s+pho)\\s+thu\\s+duc\\b|\\bquan\\s+(2|9|thu\\s+duc)\\s*(cu)?\\b|\\bq\\s*(2|9)\\s*(cu)?\\b", text)) {
    return("Thành phố Thủ Đức")
  }

  for (district_no in names(district_number_map)) {
    pattern <- paste0("\\b(quan|q)\\s*0?", district_no, "\\s*(cu)?\\b")
    if (grepl(pattern, text)) return(district_number_map[[district_no]])
  }

  for (pattern in names(district_name_patterns)) {
    if (grepl(pattern, text)) return(district_name_patterns[[pattern]])
  }

  for (pattern in names(new_ward_old_district_patterns)) {
    if (grepl(pattern, text)) return(new_ward_old_district_patterns[[pattern]])
  }

  current <- district_clean_label(parts[[1]])
  if (current %in% canonical_district_labels) current else "Không rõ"
}

# Hàm canonical_hcmc_district: chuẩn hóa tên quận huyện.
canonical_hcmc_district <- function(current, address = "", title = "", url = "") {
  current <- trimws(as.character(current))
  current[is.na(current)] <- "Không rõ"

  is_canonical <- current %in% canonical_district_labels | current == "Không rõ"

  res <- current

  needs_normalization <- !is_canonical | current == "Không rõ"
  if (any(needs_normalization)) {
    len <- length(current)
    address <- rep_len(address, len)
    title <- rep_len(title, len)
    url <- rep_len(url, len)

    res[needs_normalization] <- mapply(
      canonical_hcmc_district_one,
      current[needs_normalization],
      address[needs_normalization],
      title[needs_normalization],
      url[needs_normalization],
      USE.NAMES = FALSE
    )
  }
  res
}
