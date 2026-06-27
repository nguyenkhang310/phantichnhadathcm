`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

STANDARD_LISTING_COLS <- c(
  "source", "source_group", "source_id", "ad_id", "title", "price", "price_str",
  "area", "rooms", "address", "ward", "district_id", "district_name",
  "category_id", "category_name", "lat", "lon", "image", "ad_url", "source_url",
  "posted_at", "scraped_at", "page_fetched", "price_m", "price_per_m2",
  "has_coord", "is_rent"
)

STANDARD_CHAR_COLS <- c(
  "source", "source_group", "source_id", "ad_id", "title", "price_str",
  "address", "ward", "district_id", "district_name", "category_id",
  "category_name", "image", "ad_url", "source_url", "posted_at", "scraped_at"
)

STANDARD_NUMERIC_COLS <- c(
  "price", "area", "rooms", "lat", "lon", "price_m", "price_per_m2"
)

STANDARD_INTEGER_COLS <- c("page_fetched", "has_coord", "is_rent")

normalize_blank <- function(x, fallback = NA_character_) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == "" | tolower(x) %in% c("na", "nan", "null")] <- fallback
  x
}

replace_html_entities <- function(x) {
  x <- gsub("&nbsp;|&#160;", " ", x, ignore.case = TRUE)
  x <- gsub("&amp;", "&", x, ignore.case = TRUE)
  x <- gsub("&quot;|&#34;", "\"", x, ignore.case = TRUE)
  x <- gsub("&#39;|&apos;", "'", x, ignore.case = TRUE)
  x <- gsub("&lt;", "<", x, ignore.case = TRUE)
  x <- gsub("&gt;", ">", x, ignore.case = TRUE)
  x
}

drop_icon_chars_one <- function(x) {
  if (is.na(x) || !nzchar(x)) return(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  codes <- utf8ToInt(x)
  if (length(codes) == 0) return("")

  decorative_codes <- c(
    8205L, 65039L, 8419L, 8226L, 9679L, 9675L, 9632L, 9633L,
    9733L, 9734L, 9742L, 9745L, 9746L, 9757L, 9989L, 10003L,
    10004L, 10060L, 10062L, 10067L, 10068L, 11088L
  )
  drop <- is.na(codes) |
    codes < 32L |
    codes == 127L |
    codes %in% decorative_codes |
    (codes >= 0x1F000L & codes <= 0x1FAFFL) |
    (codes >= 0x2600L & codes <= 0x27BFL) |
    (codes >= 0x2B00L & codes <= 0x2BFFL) |
    (codes >= 0xE000L & codes <= 0xF8FFL)

  kept <- codes[!drop]
  if (length(kept) == 0) "" else intToUtf8(kept)
}

drop_icon_chars <- function(x) {
  vapply(as.character(x), drop_icon_chars_one, character(1), USE.NAMES = FALSE)
}

clean_text_field <- function(x, fallback = NA_character_) {
  x <- as.character(x)
  x <- replace_html_entities(x)
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("[\r\n\t]+", " ", x)
  x <- drop_icon_chars(x)
  x <- gsub("[[:space:]]+", " ", x)
  normalize_blank(x, fallback = fallback)
}

clean_listing_title <- function(x, fallback = NA_character_) {
  x <- clean_text_field(x, fallback = fallback)
  x <- gsub("^[[:space:]]*[-–—•·*~!,.|/\\\\]+[[:space:]]*", "", x)
  x <- gsub("[[:space:]]*[-–—•·*~!,.|/\\\\]+[[:space:]]*$", "", x)
  x <- gsub("[!]{2,}", "!", x)
  x <- gsub("[.]{3,}", "...", x)
  x <- gsub("[[:space:]]+", " ", x)
  normalize_blank(x, fallback = fallback)
}

normalize_listing_url <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- NA_character_
  x <- sub("/+$", "", x)
  tolower(x)
}

standardize_listing_schema <- function(df, default_source = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    empty <- as.data.frame(setNames(replicate(length(STANDARD_LISTING_COLS), logical(0), simplify = FALSE), STANDARD_LISTING_COLS))
    return(empty)
  }

  missing <- setdiff(STANDARD_LISTING_COLS, names(df))
  for (col in missing) df[[col]] <- NA

  df <- df[, STANDARD_LISTING_COLS, drop = FALSE]

  for (col in intersect(STANDARD_CHAR_COLS, names(df))) {
    df[[col]] <- as.character(df[[col]])
  }
  for (col in intersect(STANDARD_NUMERIC_COLS, names(df))) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  for (col in intersect(STANDARD_INTEGER_COLS, names(df))) {
    df[[col]] <- suppressWarnings(as.integer(df[[col]]))
  }

  if (!is.null(default_source)) {
    df$source <- ifelse(is.na(df$source) | df$source == "", default_source, df$source)
  }

  df$title <- clean_listing_title(df$title)
  df$price_str <- clean_text_field(df$price_str)
  df$address <- clean_text_field(df$address)
  df$ward <- clean_text_field(df$ward, fallback = "Không rõ")
  df$district_name <- clean_text_field(df$district_name, fallback = "Không rõ")
  df$category_name <- clean_text_field(df$category_name, fallback = "Bất động sản")
  df$source_id <- ifelse(
    is.na(df$source_id) | df$source_id == "",
    paste(df$source %||% default_source %||% "unknown", df$ad_id %||% seq_len(nrow(df)), sep = "_"),
    df$source_id
  )
  df$source_url <- ifelse(is.na(df$source_url) | df$source_url == "", df$ad_url, df$source_url)
  df$price_m <- ifelse(is.na(df$price_m) & !is.na(df$price), df$price / 1e6, df$price_m)
  df$price_per_m2 <- ifelse(
    is.na(df$price_per_m2) & !is.na(df$price) & !is.na(df$area) & df$area > 0,
    df$price / df$area,
    df$price_per_m2
  )
  df$has_coord <- ifelse(is.na(df$has_coord), as.integer(!is.na(df$lat) & !is.na(df$lon)), df$has_coord)
  df
}

dedupe_by_key <- function(df, key_col) {
  if (!key_col %in% names(df) || nrow(df) == 0) return(df)
  key <- trimws(as.character(df[[key_col]]))
  keep <- is.na(key) | key == "" | !duplicated(key)
  df[keep, , drop = FALSE]
}

dedupe_by_url <- function(df, url_col = "ad_url") {
  if (!url_col %in% names(df) || nrow(df) == 0) return(df)
  key <- normalize_listing_url(df[[url_col]])
  keep <- is.na(key) | key == "" | !duplicated(key)
  df[keep, , drop = FALSE]
}

combine_listing_history <- function(new_df, existing_path = NULL, default_source = NULL) {
  new_df <- standardize_listing_schema(new_df, default_source = default_source)
  pieces <- list(new_df)

  if (!is.null(existing_path) && file.exists(existing_path) && requireNamespace("readr", quietly = TRUE)) {
    old <- suppressWarnings(readr::read_csv(existing_path, show_col_types = FALSE))
    pieces <- c(list(standardize_listing_schema(old, default_source = default_source)), pieces)
  }

  combined <- do.call(rbind, pieces)
  if (nrow(combined) == 0) return(combined)

  order_key <- suppressWarnings(as.POSIXct(combined$scraped_at, tz = "Asia/Ho_Chi_Minh"))
  combined <- combined[order(order_key, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  combined <- dedupe_by_key(combined, "source_id")
  combined <- dedupe_by_url(combined, "ad_url")
  rownames(combined) <- NULL
  combined
}

write_listing_csv <- function(df, path, default_source = NULL, keep_existing_if_empty = TRUE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  df <- standardize_listing_schema(df, default_source = default_source)

  if (nrow(df) == 0 && keep_existing_if_empty && file.exists(path) && requireNamespace("readr", quietly = TRUE)) {
    message("Khong co du lieu moi hop le; giu nguyen file hien co: ", path)
    return(invisible(readr::read_csv(path, show_col_types = FALSE)))
  }

  readr::write_csv(df, path, na = "")
  invisible(df)
}
