# ============================================================
# PPT CODE - TRO LY AO BDS
# Ban rut gon de chup slide, tom tat tu dashboard R/Shiny chinh.
# ============================================================

# 1. Giao dien chat trong Shiny
assistant_ui <- div(
  class = "gemini-wrapper",
  div(class = "gemini-chat-container", uiOutput("gemini_chat_view")),
  div(
    class = "gemini-input-bar",
    textAreaInput("assistant_question", NULL, placeholder = "Hoi tro ly BDS..."),
    actionButton("assistant_send", label = icon("arrow-up"))
  )
)

# 2. Nhan cau hoi va luu hoi thoai
assistant_messages <- reactiveVal(list())
assistant_context <- reactiveVal(assistant_empty_context())

run_assistant_question <- function(question) {
  req(nzchar(question))
  assistant_messages(append(assistant_messages(), list(
    assistant_message("user", htmltools::htmlEscape(question))
  )))

  answer <- assistant_answer_bundle(
    question = question,
    df = listings(),
    context = assistant_context()
  )

  assistant_context(answer$context)
  assistant_messages(append(assistant_messages(), list(
    assistant_message("assistant", answer$html)
  )))
}

observeEvent(input$assistant_send, {
  run_assistant_question(input$assistant_question)
})

# 3. Trich xuat tieu chi tu cau hoi
assistant_extract_criteria <- function(question, df) {
  list(
    key = assistant_text_key(question),
    budget = assistant_extract_budget(question),
    area = assistant_extract_area(question),
    rooms = assistant_extract_rooms(question),
    transaction = assistant_detect_transaction(question),
    districts = assistant_match_districts(question, unique(df$district_name)),
    categories = assistant_match_categories(question, unique(df$category_name))
  )
}

# 4. Nhan dien y dinh nguoi dung
assistant_detect_intent <- function(question, criteria) {
  key <- criteria$key

  case_when(
    str_detect(key, "du doan|uoc tinh|dinh gia") ~ "predict",
    str_detect(key, "so sanh|khac nhau| vs ") ~ "compare",
    str_detect(key, "goi y|tim|tin dang|phu hop") ~ "recommend",
    str_detect(key, "re hon|duoi gia|gia tot") ~ "undervalued",
    str_detect(key, "vi sao|giai thich|yeu to") ~ "explain",
    TRUE ~ "stats"
  )
}

# 5. Dinh tuyen cau hoi sang tool local
assistant_answer_bundle <- function(question, df, context) {
  criteria <- assistant_extract_criteria(question, df)
  criteria <- assistant_merge_criteria(criteria, context)
  intent <- assistant_detect_intent(question, criteria)

  html <- switch(
    intent,
    predict = assistant_predict_response(df, criteria),
    compare = assistant_compare_response(df, criteria),
    recommend = assistant_recommend_response(df, criteria),
    undervalued = assistant_undervalued_response(df, criteria),
    explain = assistant_explain_response(df, criteria),
    stats = assistant_stats_response(df, criteria)
  )

  list(
    html = html,
    context = list(criteria = criteria, intent = intent, last_answer_at = Sys.time())
  )
}

# 6. Tra loi dang du doan gia
assistant_predict_response <- function(df, criteria) {
  input_row <- build_prediction_row(
    df = df,
    district = criteria$districts[[1]],
    category = criteria$categories[[1]],
    area = criteria$area,
    rooms = criteria$rooms,
    transaction_type = criteria$transaction %||% "Ban"
  )

  pred <- predict_price(input_row, is_rent = identical(criteria$transaction, "Cho thue"))

  paste0(
    "<div class='assistant-prediction'>", format_vnd_full(pred), "</div>",
    "<p>Ket qua duoc tinh tu model local va du lieu thi truong hien co.</p>"
  )
}

# 7. Goi y tin dang phu hop
assistant_recommend_response <- function(df, criteria) {
  scoped <- assistant_apply_criteria(df, criteria)

  if (nrow(scoped) == 0) {
    return(assistant_no_data_response(criteria))
  }

  top_listings <- scoped %>%
    assistant_sort_recommendations(criteria) %>%
    slice_head(n = 6)

  paste0(
    assistant_scope_note(criteria),
    assistant_listing_cards(top_listings, limit = 6)
  )
}
