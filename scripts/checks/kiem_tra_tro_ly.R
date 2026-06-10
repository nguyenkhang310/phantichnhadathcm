#!/usr/bin/env Rscript

# Smoke test for the local BDS assistant engine.
# It sources app.R without launching a browser, then checks intent parsing,
# entity extraction, memory merge, and response generation on representative questions.

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/checks/kiem_tra_tro_ly.R"
project_root <- normalizePath(file.path(dirname(script_file), ".."), mustWork = FALSE)
if (!dir.exists(file.path(project_root, "scripts"))) project_root <- getwd()
setwd(project_root)

env <- new.env(parent = globalenv())
sys.source("app.R", envir = env)

df <- env$load_data()
if (nrow(df) == 0) stop("No listing data loaded; assistant smoke test cannot run.")

failures <- character()

# Hàm expect: hỗ trợ xử lý dữ liệu trong script.
expect <- function(label, ok, detail = "") {
  if (isTRUE(ok)) {
    cat("[PASS]", label, "\n")
  } else {
    message <- paste("[FAIL]", label, detail)
    cat(message, "\n")
    failures <<- c(failures, message)
  }
}

# Hàm parse_case: phân tích chuỗi đầu vào thành giá trị chuẩn.
parse_case <- function(question, context = env$assistant_empty_context()) {
  criteria <- env$assistant_extract_criteria(question, df)
  criteria <- env$assistant_merge_criteria(criteria, context)
  intent <- env$assistant_detect_intent(question, criteria, context)
  list(criteria = criteria, intent = intent)
}

case1 <- parse_case("4 tỷ mua căn hộ tầm 60m2 ở khu nào ổn?")
expect("scout intent for open location budget question", identical(case1$intent, "scout"))
expect("sale transaction inferred from 4 tỷ", identical(case1$criteria$transaction, "Bán"))
expect("budget parsed near 4 tỷ", isTRUE(abs(case1$criteria$budget_max - 4e9) < 1))
expect("area target parsed near 60m2", isTRUE(abs(case1$criteria$area - 60) < 0.01))
expect("apartment category matched", any(grepl("Căn hộ|Chung cư", case1$criteria$categories)))

case2 <- parse_case("So sánh Thủ Đức với Quận 7 cho căn hộ bán")
expect("compare intent for two districts", identical(case2$intent, "compare"))
expect("two districts extracted", length(case2$criteria$districts) >= 2)
expect("Thủ Đức extracted", any(grepl("Thủ Đức", case2$criteria$districts)))
expect("Quận 7 extracted", any(grepl("Quận 7", case2$criteria$districts)))

case3 <- parse_case("Tìm tin giá tốt hơn mặt bằng ở Bình Tân dưới 4 tỷ")
expect("undervalued intent for giá tốt hơn mặt bằng", identical(case3$intent, "undervalued"))
expect("Bình Tân extracted", any(grepl("Bình Tân", case3$criteria$districts)))
expect("budget under 4 tỷ parsed", isTRUE(abs(case3$criteria$budget_max - 4e9) < 1))

case4 <- parse_case("Dự đoán căn hộ 70m2 2PN ở Thủ Đức")
expect("predict intent for valuation question", identical(case4$intent, "predict"))
expect("prediction area parsed", isTRUE(abs(case4$criteria$area - 70) < 0.01))
expect("rooms parsed", isTRUE(case4$criteria$rooms == 2))
expect("prediction district parsed", any(grepl("Thủ Đức", case4$criteria$districts)))

case5 <- parse_case("các tin ở quận tân phú")
expect("listing intent for 'các tin' question", identical(case5$intent, "recommend"))
expect("Tân Phú extracted", any(grepl("Tân Phú", case5$criteria$districts)))

context <- env$assistant_empty_context()
bundle1 <- env$assistant_answer_bundle("Ngân sách 4 tỷ mua nhà ở Bình Tân", df, context)
bundle2 <- env$assistant_answer_bundle("Còn Quận 7 thì sao?", df, bundle1$context)
expect("follow-up keeps prior Bình Tân and adds Quận 7", all(c(
  any(grepl("Bình Tân", bundle2$context$criteria$districts)),
  any(grepl("Quận 7", bundle2$context$criteria$districts))
)))
expect("follow-up produces non-empty HTML", is.character(bundle2$html) && nzchar(bundle2$html))

responses <- c(
  env$assistant_answer_bundle("4 tỷ mua căn hộ tầm 60m2 ở khu nào ổn?", df, env$assistant_empty_context())$html,
  env$assistant_answer_bundle("Tìm tin giá tốt hơn mặt bằng ở Bình Tân dưới 4 tỷ", df, env$assistant_empty_context())$html,
  env$assistant_answer_bundle("Dự đoán căn hộ 70m2 2PN ở Thủ Đức", df, env$assistant_empty_context())$html,
  env$assistant_answer_bundle("các tin ở quận tân phú", df, env$assistant_empty_context())$html
)
expect("core responses render HTML", all(nzchar(responses)))
expect("listing request renders listing cards", grepl("assistant-listing", responses[[4]], fixed = TRUE))
expect("mixed listing request separates sale/rent", grepl("Tin bán nổi bật|Tin thuê nổi bật", responses[[4]]))

if (length(failures) > 0) {
  cat("\nAssistant smoke test failed:\n")
  cat(paste(failures, collapse = "\n"), "\n")
  quit(status = 1)
}

cat("\nAssistant smoke test passed.\n")
