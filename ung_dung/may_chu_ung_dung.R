server <- function(input, output, session) {
  map_marker_limit <- performance_limit("BDS_MAP_MAX_MARKERS", 35000)

  nav_tabs <- c(
    "overview", "map", "analysis", "statistics", "predict",
    "diagnostics", "clusters", "data", "assistant"
  )

  invisible(lapply(nav_tabs, function(tab_id) {
    observeEvent(
      input[[paste0("nav_", tab_id)]],
      updateTabsetPanel(session, "tabs", selected = tab_id),
      ignoreInit = TRUE
    )
  }))

  listings <- reactive({
    load_data()
  })

  data_bounds <- reactive({
    df <- listings()
    list(
      price_max = nice_slider_max(df$price_b, step = 10, fallback = 10),
      area_max = nice_slider_max(df$area, step = 100, fallback = 1000)
    )
  })

  observe({
    bounds <- data_bounds()
    updateSliderInput(session, "price_range", max = bounds$price_max, value = c(0, bounds$price_max))
    updateSliderInput(session, "map_price_range", max = bounds$price_max, value = c(0, bounds$price_max))
    updateSliderInput(session, "data_price_range", max = bounds$price_max, value = c(0, bounds$price_max))
    updateSliderInput(session, "area_range", max = bounds$area_max, value = c(0, bounds$area_max))
    updateSliderInput(session, "map_area_range", max = bounds$area_max, value = c(0, bounds$area_max))
  })

  metrics <- reactive({
    load_metrics()
  })

  registry <- reactive({
    load_registry()
  })

  source_choices <- reactive(choice_values(listings()$source))
  transaction_choices <- reactive(choice_values(listings()$transaction_type))
  district_choices <- reactive(choice_values(listings()$district_name))
  category_choices <- reactive(choice_values(listings()$category_name))
  report_district_choices <- reactive(district_report_choices(listings()))

  selected_report_district <- reactive({
    district_report_selected(listings(), input$report_district)
  })

  assistant_messages <- reactiveVal(list())
  assistant_context <- reactiveVal(assistant_empty_context())

  output$gemini_chat_view <- renderUI({
    messages <- assistant_messages()
    if (length(messages) == 0) {
      div(
        class = "gemini-welcome-view",
        div(class = "gemini-greeting", "Xin chào, mình là BDS"),
        div(class = "gemini-sub-greeting", "Mình có thể giúp gì cho bạn hôm nay?"),
        div(
          class = "gemini-suggest-grid",
          actionLink(
            "assistant_sample_1",
            class = "gemini-suggest-card",
            tagList(
              div(class = "gemini-suggest-card-title", "4 tỷ mua ở đâu?"),
              div(class = "gemini-suggest-card-desc", "Gợi ý khu vực phù hợp cho căn hộ quanh 60m²"),
              div(class = "gemini-suggest-card-icon", icon("chart-line"))
            )
          ),
          actionLink(
            "assistant_sample_2",
            class = "gemini-suggest-card",
            tagList(
              div(class = "gemini-suggest-card-title", "So sánh Thủ Đức & Q7"),
              div(class = "gemini-suggest-card-desc", "So sánh giá bán, cho thuê giữa 2 khu vực"),
              div(class = "gemini-suggest-card-icon", icon("code-compare"))
            )
          ),
          actionLink(
            "assistant_sample_3",
            class = "gemini-suggest-card",
            tagList(
              div(class = "gemini-suggest-card-title", "Tìm deal giá tốt"),
              div(class = "gemini-suggest-card-desc", "Lọc tin thấp hơn mặt bằng trong cùng khu vực"),
              div(class = "gemini-suggest-card-icon", icon("house-circle-check"))
            )
          ),
          actionLink(
            "assistant_sample_4",
            class = "gemini-suggest-card",
            tagList(
              div(class = "gemini-suggest-card-title", "Dự đoán căn hộ 70m²"),
              div(class = "gemini-suggest-card-desc", "Dự toán giá căn hộ chung cư 70m² tại Thủ Đức"),
              div(class = "gemini-suggest-card-icon", icon("wand-magic-sparkles"))
            )
          )
        )
      )
    } else {
      div(
        class = "gemini-chat-log",
        uiOutput("assistant_chat")
      )
    }
  })
  append_assistant_message <- function(role, html) {
    assistant_messages(append(assistant_messages(), list(assistant_message(role, html))))
  }
  run_assistant_question <- function(question) {
    question <- trimws(as.character(question %||% ""))
    if (!nzchar(question)) return(invisible(FALSE))

    append_assistant_message("user", htmltools::htmlEscape(question))
    answer_bundle <- tryCatch(
      assistant_answer_bundle(question, listings(), assistant_context()),
      error = function(e) paste0(
        "<p>Mình gặp lỗi khi xử lý câu hỏi này: <b>",
        htmltools::htmlEscape(conditionMessage(e)),
        "</b></p>"
      )
    )
    if (is.list(answer_bundle) && !is.null(answer_bundle$html)) {
      answer <- answer_bundle$html
      assistant_context(answer_bundle$context %||% assistant_empty_context())
    } else {
      answer <- answer_bundle
    }
    append_assistant_message("assistant", answer)
    updateTextAreaInput(session, "assistant_question", value = "")
    invisible(TRUE)
  }

  output$assistant_chat <- renderUI({
    messages <- assistant_messages()
    n <- length(messages)
    if (n == 0) return(NULL)

    tagList(lapply(seq_along(messages), function(i) {
      message <- messages[[i]]
      is_user <- identical(message$role, "user")
      is_last_bot <- !is_user && (i == n)
      if (is_user) {
        div(
          class = "gemini-message user",
          div(class = "gemini-bubble", HTML(message$html))
        )
      } else {
        div(
          class = paste("gemini-message bot", if (is_last_bot) "typing" else ""),
          div(
            class = "gemini-avatar",
            gemini_star_svg()
          ),
          div(
            class = "gemini-bot-container",
            div(class = "gemini-text", HTML(message$html)),
            div(
              class = "gemini-msg-actions",
              tags$button(class = "gemini-action-btn speak-btn", icon("volume-high"), onclick = "speakText(this)"),
              tags$button(class = "gemini-action-btn", icon("thumbs-up")),
              tags$button(class = "gemini-action-btn", icon("thumbs-down")),
              tags$button(class = "gemini-action-btn", icon("rotate")),
              tags$button(class = "gemini-action-btn", icon("copy")),
              tags$button(class = "gemini-action-btn", icon("ellipsis"))
            )
          )
        )
      }
    }))
  })

  observeEvent(input$assistant_send, {
    run_assistant_question(input$assistant_question)
  }, ignoreInit = TRUE)

  observeEvent(input$assistant_sample_1, {
    run_assistant_question("4 tỷ mua căn hộ tầm 60m2 ở khu nào ổn?")
  }, ignoreInit = TRUE)

  observeEvent(input$assistant_sample_2, {
    run_assistant_question("So sánh Thủ Đức với Quận 7")
  }, ignoreInit = TRUE)

  observeEvent(input$assistant_sample_3, {
    run_assistant_question("Tìm tin giá tốt hơn mặt bằng ở Bình Tân dưới 4 tỷ")
  }, ignoreInit = TRUE)

  observeEvent(input$assistant_sample_4, {
    run_assistant_question("Dự đoán căn hộ 70m2 ở Thủ Đức")
  }, ignoreInit = TRUE)

  observeEvent(input$assistant_clear, {
    assistant_messages(list())
    assistant_context(assistant_empty_context())
  }, ignoreInit = TRUE)

  filtered <- reactive({
    df <- listings()
    if (is_selected_filter(input$sources)) {
      df <- df %>% filter(source %in% input$sources)
    }
    if (is_selected_filter(input$transactions)) {
      df <- df %>% filter(transaction_type %in% input$transactions)
    }
    if (is_selected_filter(input$districts)) {
      df <- df %>% filter(district_name %in% input$districts)
    }
    if (is_selected_filter(input$categories)) {
      df <- df %>% filter(category_name %in% input$categories)
    }
    bounds <- data_bounds()
    price_range <- safe_range(input$price_range, c(0, bounds$price_max))
    area_range <- safe_range(input$area_range, c(0, bounds$area_max))
    df %>%
      filter(
        price_b >= price_range[1], price_b <= price_range[2],
        area >= area_range[1], area <= area_range[2]
      )
  })

  map_filtered <- reactive({
    df <- listings()
    if (is_selected_filter(input$map_sources)) {
      df <- df %>% filter(source %in% input$map_sources)
    }
    if (is_selected_filter(input$map_transactions)) {
      df <- df %>% filter(transaction_type %in% input$map_transactions)
    }
    if (is_selected_filter(input$map_districts)) {
      df <- df %>% filter(district_name %in% input$map_districts)
    }
    if (is_selected_filter(input$map_categories)) {
      df <- df %>% filter(category_name %in% input$map_categories)
    }
    bounds <- data_bounds()
    price_range <- safe_range(input$map_price_range, c(0, bounds$price_max))
    area_range <- safe_range(input$map_area_range, c(0, bounds$area_max))
    df %>%
      filter(
        price_b >= price_range[1], price_b <= price_range[2],
        area >= area_range[1], area <= area_range[2],
        !is.na(map_lat), !is.na(map_lon)
      )
  })

  map_displayed <- reactive({
    cap_render_rows(map_filtered(), map_marker_limit) %>%
      mutate(.map_layer_id = as.character(row_number()))
  })

  map_listing_popups <- function(df) {
    source_links <- listing_url(df$ad_url, df$source)
    has_source_link <- !is.na(source_links) & source_links != ""
    source_link_html <- ifelse(
      has_source_link,
      paste0(
        "<a href='", htmltools::htmlEscape(source_links), "' target='_blank' rel='noopener noreferrer' ",
        "style='display:inline-flex;align-items:center;justify-content:center;margin-top:10px;",
        "padding:7px 10px;border-radius:6px;background:#0072bc;color:#ffffff;",
        "font-weight:700;text-decoration:none'>Xem tin gốc</a>"
      ),
      "<div style='margin-top:10px;color:#94a3b8;font-size:12px'>Tin này chưa có link gốc</div>"
    )
    paste0(
      "<div style='font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Arial,sans-serif;font-size:12px;min-width:220px'>",
      "<div style='font-weight:700;color:#0072bc;margin-bottom:4px'>", htmltools::htmlEscape(df$title), "</div>",
      "<div style='color:#64748b'>", htmltools::htmlEscape(df$district_name), " · ", htmltools::htmlEscape(df$ward), "</div>",
      "<div style='margin-top:7px;display:grid;grid-template-columns:auto 1fr;gap:3px 10px'>",
      "<span style='color:#64748b'>Giá</span><b>", format_vnd_full(df$price), "</b>",
      "<span style='color:#64748b'>Diện tích</span><b>", round(df$area, 1), " m²</b>",
      "<span style='color:#64748b'>Giá/m²</span><b>", format_vnd_full(df$price_per_m2), "/m²</b>",
      "<span style='color:#64748b'>Loại</span><b>", htmltools::htmlEscape(df$category_name), "</b>",
      "<span style='color:#64748b'>Tọa độ</span><b>", htmltools::htmlEscape(df$coord_status), "</b>",
      "</div>",
      source_link_html,
      "</div>"
    )
  }

  map_price_cuts <- reactive({
    df <- map_filtered()
    price_cuts <- quantile(df$price, probs = c(1 / 3, 2 / 3), na.rm = TRUE)
    if (any(is.na(price_cuts)) || price_cuts[[1]] == price_cuts[[2]]) {
      c(3e9, 8e9)
    } else {
      price_cuts
    }
  })

  data_filtered <- reactive({
    df <- listings()
    if (is_selected_filter(input$data_sources)) {
      df <- df %>% filter(source %in% input$data_sources)
    }
    if (is_selected_filter(input$data_transactions)) {
      df <- df %>% filter(transaction_type %in% input$data_transactions)
    }
    if (is_selected_filter(input$data_districts)) {
      df <- df %>% filter(district_name %in% input$data_districts)
    }
    if (is_selected_filter(input$data_categories)) {
      df <- df %>% filter(category_name %in% input$data_categories)
    }
    bounds <- data_bounds()
    price_range <- safe_range(input$data_price_range, c(0, bounds$price_max))
    df %>%
      filter(price_b >= price_range[1], price_b <= price_range[2])
  })
  chart_transaction <- function(input_id) {
    value <- input[[input_id]]
    if (is.null(value) || length(value) == 0 || !(value %in% c("Bán", "Cho thuê"))) "Bán" else value[[1]]
  }
  overview_chart_data <- function(input_id) {
    tx <- chart_transaction(input_id)
    listings() %>% filter(transaction_type == tx)
  }

  analysis_chart_data <- function(input_id) {
    tx <- chart_transaction(input_id)
    df <- listings() %>% filter(transaction_type == tx)

    if (is_selected_filter(input$sources)) {
      df <- df %>% filter(source %in% input$sources)
    }
    if (is_selected_filter(input$districts)) {
      df <- df %>% filter(district_name %in% input$districts)
    }
    if (is_selected_filter(input$categories)) {
      df <- df %>% filter(category_name %in% input$categories)
    }

    bounds <- data_bounds()
    price_range <- safe_range(input$price_range, c(0, bounds$price_max))
    area_range <- safe_range(input$area_range, c(0, bounds$area_max))
    df %>%
      filter(
        price_b >= price_range[1], price_b <= price_range[2],
        area >= area_range[1], area <= area_range[2]
      )
  }

  stat_base_data <- reactive({
    tx <- input$stat_transaction %||% "Bán"
    df <- listings() %>%
      filter(transaction_type == tx, finite_positive(price_per_m2), finite_positive(price))

    if (is_selected_filter(input$stat_sources)) {
      df <- df %>% filter(source %in% input$stat_sources)
    }
    if (is_selected_filter(input$stat_category)) {
      df <- df %>% filter(category_name %in% input$stat_category)
    }

    known_rows_or_all(df, "district_name")
  })

  stat_district_choices <- reactive({
    choices <- stat_base_data() %>%
      count(district_name, sort = TRUE) %>%
      filter(!is_missing_label(district_name)) %>%
      pull(district_name)
    if (length(choices) == 0) choices <- district_choices()
    unique(choices)
  })

  stat_selected_district_a <- reactive({
    choices <- stat_district_choices()
    if (length(choices) == 0) return(NA_character_)
    value <- input$stat_district_a
    if (!is.null(value) && length(value) > 0 && value[[1]] %in% choices) value[[1]] else choices[[1]]
  })

  stat_selected_district_b <- reactive({
    choices <- stat_district_choices()
    if (length(choices) == 0) return(NA_character_)
    value <- input$stat_district_b
    if (!is.null(value) && length(value) > 0 && value[[1]] %in% choices) {
      return(value[[1]])
    }
    if (length(choices) >= 2) choices[[2]] else choices[[1]]
  })

  diagnostic_data <- reactive({
    tx <- chart_transaction("diagnostic_tx")
    is_rent_diag <- identical(tx, "Cho thuê")
    model_path <- if (is_rent_diag) RENT_MODEL_PATH else SALE_MODEL_PATH
    if (!file.exists(model_path)) return(tibble())

    df <- listings() %>%
      filter(is_rent == !!is_rent_diag, finite_positive(price), finite_positive(price_per_m2))
    if (nrow(df) == 0) return(tibble())

    set.seed(2026)
    sample_n <- min(900, nrow(df))
    if (nrow(df) > sample_n) {
      df <- df[sample(seq_len(nrow(df)), sample_n), , drop = FALSE]
    }

    bundle <- load_model_cached(model_path)
    if (is.null(bundle)) return(tibble())
    predicted <- predict_prices_for_rows(df, bundle)
    df %>%
      mutate(
        predicted_price = predicted,
        actual_price = price,
        residual_log = log1p(actual_price) - log1p(predicted_price),
        ape = abs(actual_price - predicted_price) / actual_price,
        model_name = model_label_vi(best_model_from_bundle(bundle))
      ) %>%
      filter(finite_positive(predicted_price), is.finite(residual_log), is.finite(ape))
  })

  output$kpi_cards <- renderUI({
    df <- listings()
    m <- metrics()
    div(
      class = "kpi-grid",
      kpi_card("Tin đăng đã thu thập", format(nrow(df), big.mark = ","), "sau làm sạch", "database", "default"),
      kpi_card("Giá trung vị", median_price_stack(df, compact = TRUE), "tách riêng bán và cho thuê", "coins", "warning", value_class = "median-split"),
      kpi_card("Khu vực cũ có dữ liệu", paste0(n_distinct(df$district_name[!is_missing_label(df$district_name)]), " khu vực"), "độ phủ địa lý", "location-dot", "success"),
      kpi_card("Mô hình tốt nhất", best_model_name_only(m), "chọn theo sai số kiểm định", "bullseye", "success", delta = best_model_mape_only(m), value_class = "text-mode")
    )
  })

  output$report_district_picker <- renderUI({
    choices <- report_district_choices()
    if (length(choices) == 0) {
      return(div(class = "report-empty", "Chưa có khu vực"))
    }

    selectizeInput(
      "report_district",
      NULL,
      choices = setNames(choices, choices),
      selected = district_report_selected(listings()),
      width = "100%",
      options = list(
        placeholder = "Chọn khu vực...",
        maxOptions = 50,
        create = FALSE
      )
    )
  })

  output$report_quick_insight <- renderUI({
    district <- selected_report_district()
    req(!is.na(district))

    profile <- build_district_report_profile(listings(), district)
    focus <- profile$focus_tx
    if (nrow(focus) == 0) return(NULL)

    count_text <- format_count_vi(profile$total)

    div(
      class = "report-stats",
      div(
        class = "report-stat",
        title = paste0(count_text, " tin tại ", profile$district),
        icon("layer-group"),
        div(
          class = "report-stat-content",
          span(class = "report-stat-value", count_text),
          span(class = "report-stat-label", "tin đăng")
        )
      ),
      div(
        class = "report-stat",
        title = paste0("Giá trung vị bán và cho thuê tại ", profile$district),
        icon("coins"),
        div(
          class = "report-stat-content report-median-content",
          div(class = "report-stat-value report-median-value", median_price_tiles(profile$scoped))
        )
      )
    )
  })

  output$download_district_report <- downloadHandler(
    filename = function() {
      district_report_filename(selected_report_district())
    },
    content = function(file) {
      district <- selected_report_district()
      req(!is.na(district))
      build_district_report_pdf(file, listings(), district)
    }
  )

  output$source_filter <- renderUI({
    filter_source_select("sources", source_choices(), "Tất cả nguồn")
  })

  output$district_filter <- renderUI({
    filter_select("districts", district_choices(), "Tất cả khu vực")
  })

  output$transaction_filter <- renderUI({
    filter_select("transactions", transaction_choices(), "Tất cả giao dịch")
  })

  output$category_filter <- renderUI({
    filter_select("categories", category_choices(), "Tất cả loại BĐS")
  })

  output$map_source_filter <- renderUI({
    filter_source_select("map_sources", source_choices(), "Tất cả nguồn")
  })

  output$map_district_filter <- renderUI({
    filter_select("map_districts", district_choices(), "Tất cả khu vực")
  })

  output$map_transaction_filter <- renderUI({
    filter_select("map_transactions", transaction_choices(), "Tất cả giao dịch")
  })

  output$map_category_filter <- renderUI({
    filter_select("map_categories", category_choices(), "Tất cả loại BĐS")
  })

  output$data_source_filter <- renderUI({
    filter_source_select("data_sources", source_choices(), "Tất cả nguồn")
  })

  output$data_district_filter <- renderUI({
    filter_select("data_districts", district_choices(), "Tất cả khu vực")
  })

  output$data_transaction_filter <- renderUI({
    filter_select("data_transactions", transaction_choices(), "Tất cả giao dịch")
  })

  output$data_category_filter <- renderUI({
    filter_select("data_categories", category_choices(), "Tất cả loại BĐS")
  })

  output$predict_district <- renderUI({
    selectInput("pred_district", NULL, choices = district_choices(), selectize = FALSE)
  })

  output$predict_category <- renderUI({
    is_rent_pred <- identical(input$predict_transaction, "Cho thuê")
    choices <- listings() %>%
      filter(is_rent == !!is_rent_pred) %>%
      pull(category_name) %>%
      unique() %>%
      sort()
    if (length(choices) == 0) choices <- category_choices()
    selectInput("pred_category", NULL, choices = choices, selectize = FALSE)
  })

  output$stat_category_filter <- renderUI({
    tx <- input$stat_transaction %||% "Bán"
    df <- listings() %>% filter(transaction_type == tx)
    if (is_selected_filter(input$stat_sources)) {
      df <- df %>% filter(source %in% input$stat_sources)
    }
    choices <- df %>%
      pull(category_name) %>%
      choice_values()
    selectInput(
      "stat_category",
      NULL,
      choices = c("Tất cả loại BĐS" = "__all__", setNames(choices, choices)),
      selected = "__all__",
      selectize = FALSE
    )
  })

  output$stat_source_filter <- renderUI({
    filter_source_select("stat_sources", source_choices(), "Tất cả nguồn")
  })

  output$stat_district_a_filter <- renderUI({
    choices <- stat_district_choices()
    selected <- if (length(choices) > 0) choices[[1]] else ""
    selectInput("stat_district_a", NULL, choices = choices, selected = selected, selectize = FALSE)
  })

  output$stat_district_b_filter <- renderUI({
    choices <- stat_district_choices()
    selected <- if (length(choices) >= 2) choices[[2]] else if (length(choices) == 1) choices[[1]] else ""
    selectInput("stat_district_b", NULL, choices = choices, selected = selected, selectize = FALSE)
  })

  output$stat_filter_summary <- renderUI({
    df <- stat_base_data()
    tx <- input$stat_transaction %||% "Bán"
    div(
      class = "filter-summary full stat-filter-summary",
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Mẫu thống kê"),
          div(class = "filter-chip-value", format_count_vi(nrow(df)))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Nguồn"),
          div(class = "filter-chip-value", active_source_or_all(input$stat_sources))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Giao dịch"),
          div(class = "filter-chip-value", tx)),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Nguồn tọa độ"),
          div(class = "filter-chip-value", coordinate_source_label(df, compact = TRUE)))
    )
  })

  output$filter_summary <- renderUI({
    df <- filtered()
    div(
      class = "filter-summary",
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Số tin"),
          div(class = "filter-chip-value", format(nrow(df), big.mark = ","))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Giá trung vị"),
          div(class = "filter-chip-value median-chip-value", median_price_stack(df, compact = TRUE)))
    )
  })

  output$map_filter_summary <- renderUI({
    df <- map_filtered()
    display_df <- map_displayed()
    div(
      class = "filter-summary full",
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Điểm hiển thị"),
          div(class = "filter-chip-value", format_rendered_total(nrow(display_df), nrow(df)))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Nguồn tọa độ"),
          div(class = "filter-chip-value", coordinate_source_label(df, compact = TRUE))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Nguồn"),
          div(class = "filter-chip-value", active_source_or_all(input$map_sources))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Khu vực"),
          div(class = "filter-chip-value", active_or_all(input$map_districts)))
    )
  })

  output$data_filter_summary <- renderUI({
    df <- data_filtered()
    div(
      class = "filter-summary full",
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Dòng dữ liệu"),
          div(class = "filter-chip-value", format(nrow(df), big.mark = ","))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Nguồn"),
          div(class = "filter-chip-value", active_source_or_all(input$data_sources))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Giá trung vị"),
          div(class = "filter-chip-value median-chip-value", median_price_stack(df, compact = TRUE)))
    )
  })

  observeEvent(input$reset_analysis_filters, {
    bounds <- data_bounds()
    updateSelectInput(session, "sources", selected = "__all__")
    updateSelectInput(session, "transactions", selected = "__all__")
    updateSelectInput(session, "districts", selected = "__all__")
    updateSelectInput(session, "categories", selected = "__all__")
    updateSliderInput(session, "price_range", value = c(0, bounds$price_max))
    updateSliderInput(session, "area_range", value = c(0, bounds$area_max))
  }, ignoreInit = TRUE)

  observeEvent(input$reset_map_filters, {
    bounds <- data_bounds()
    updateSelectInput(session, "map_sources", selected = "__all__")
    updateSelectInput(session, "map_transactions", selected = "__all__")
    updateSelectInput(session, "map_districts", selected = "__all__")
    updateSelectInput(session, "map_categories", selected = "__all__")
    updateSliderInput(session, "map_price_range", value = c(0, bounds$price_max))
    updateSliderInput(session, "map_area_range", value = c(0, bounds$area_max))
  }, ignoreInit = TRUE)

  observeEvent(input$reset_data_filters, {
    bounds <- data_bounds()
    updateSelectInput(session, "data_sources", selected = "__all__")
    updateSelectInput(session, "data_transactions", selected = "__all__")
    updateSelectInput(session, "data_districts", selected = "__all__")
    updateSelectInput(session, "data_categories", selected = "__all__")
    updateSliderInput(session, "data_price_range", value = c(0, bounds$price_max))
  }, ignoreInit = TRUE)

  output$district_plot <- renderPlotly({
    tx <- chart_transaction("district_plot_tx")
    df <- overview_chart_data("district_plot_tx")
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", tx, "phù hợp.")))
    p <- known_rows_or_all(df, "district_name") %>%
      count(district_name, sort = TRUE) %>%
      slice_head(n = 12) %>%
      mutate(
        tooltip = paste0("Giao dịch: ", tx, "<br>Khu vực cũ: ", district_name, "<br>Số tin: ", format_count_vi(n))
      ) %>%
      ggplot(aes(x = reorder(district_name, n), y = n, text = tooltip)) +
      geom_col(fill = "#0072bc", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "Số tin") +
      chart_theme()
    interactive_chart(p, tooltip = "text")
  })

  output$category_plot <- renderPlotly({
    tx <- chart_transaction("category_plot_tx")
    df <- overview_chart_data("category_plot_tx")
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", tx, "phù hợp.")))
    plot_df <- known_rows_or_all(df, "category_name") %>%
      count(category_name, sort = TRUE) %>%
      slice_head(n = 10) %>%
      mutate(
        tooltip = paste0("Giao dịch: ", tx, "<br>Loại BĐS: ", category_name, "<br>Số tin: ", format_count_vi(n))
      )
    fill_values <- setNames(chart_colors(nrow(plot_df)), plot_df$category_name)
    p <- plot_df %>%
      ggplot(aes(x = reorder(category_name, n), y = n, fill = category_name, text = tooltip)) +
      geom_col(width = 0.72) +
      scale_fill_manual(values = fill_values) +
      coord_flip() +
      guides(fill = "none") +
      labs(x = NULL, y = "Số tin") +
      chart_theme()
    interactive_chart(p, tooltip = "text")
  })

  output$metrics_table <- renderTable({
    m <- metrics()
    if (nrow(m) == 0) return(data.frame(Ghi_chu = paste0("Chưa có ", PATHS$metrics_csv)))
    segment_totals <- listings() %>%
      mutate(segment = if_else(transaction_type == "Cho thuê", "rent", "sale")) %>%
      count(segment, name = "total_listings")

    m %>%
      left_join(segment_totals, by = "segment") %>%
      mutate(
        total_listings = coalesce(total_listings, train_rows + test_rows),
        segment = recode(segment, sale = "Bán", rent = "Cho thuê", .default = segment),
        model = model_label_vi(model),
        split_type = recode(
          split_type,
          stratified_random_by_source = "Kiểm định 80/20 theo nguồn",
          time_based = "Kiểm định theo thời gian",
          random_fallback = "Kiểm định ngẫu nhiên",
          .default = split_type
        ),
        total_listings = format_count_vi(total_listings),
        train_rows = format_count_vi(train_rows),
        test_rows = format_count_vi(test_rows),
        rmse_vnd = format_vnd_full(rmse_vnd),
        mae_vnd = format_vnd_full(mae_vnd),
        mape = paste0(round(mape * 100, 1), "%"),
        r2 = format_metric(r2)
      ) %>%
      select(
        `Nhóm dữ liệu` = segment,
        `Tổng listings` = total_listings,
        `Mô hình` = model,
        `Cách đánh giá` = split_type,
        `Huấn luyện` = train_rows,
        `Kiểm định` = test_rows,
        RMSE = rmse_vnd,
        MAE = mae_vnd,
        MAPE = mape,
        `R²` = r2
      )
  })

  output$listing_map <- renderLeaflet({
    df <- map_displayed()
    validate(need(nrow(df) > 0, "Không có điểm dữ liệu phù hợp bộ lọc."))
    price_cuts <- map_price_cuts()

    marker_df <- df %>%
      mutate(popup_html = map_listing_popups(df)) %>%
      transmute(
        map_lon,
        map_lat,
        price,
        coord_status,
        .map_layer_id,
        popup_html
      )

    cluster_icon <- htmlwidgets::JS(
      "function(cluster) {
        var count = cluster.getChildCount();
        var children = cluster.getAllChildMarkers();
        var tones = { low: 0, mid: 0, high: 0 };
        children.forEach(function(marker) {
          var color = String((marker.options && marker.options.fillColor) || '').toLowerCase();
          if (color === '#059669') tones.low += 1;
          else if (color === '#d97706') tones.mid += 1;
          else if (color === '#ed1c24') tones.high += 1;
        });
        var maxTone = 'low';
        if (tones.mid > tones[maxTone]) maxTone = 'mid';
        if (tones.high > tones[maxTone]) maxTone = 'high';
        var dominantShare = count > 0 ? tones[maxTone] / count : 0;
        var tone = dominantShare >= 0.58 ? maxTone : 'mixed';
        var lowEnd = Math.round((tones.low / count) * 360);
        var midEnd = lowEnd + Math.round((tones.mid / count) * 360);
        var size = count < 25 ? 'small' : (count < 120 ? 'medium' : 'large');
        var label = count >= 1000 ? (Math.round(count / 100) / 10) + 'k' : count;
        var pixelSize = count < 25 ? 42 : (count < 120 ? 50 : 58);
        var style = tone === 'mixed' ? \" style='--low-end:\" + lowEnd + \"deg;--mid-end:\" + midEnd + \"deg;'\" : '';
        return L.divIcon({
          html: '<div' + style + '><span>' + label + '</span></div>',
          className: 'bds-marker-cluster bds-marker-cluster-' + size + ' bds-marker-cluster-' + tone,
          iconSize: L.point(pixelSize, pixelSize)
        });
      }"
    )
    cluster_radius <- htmlwidgets::JS(
      "function(zoom) {
        if (zoom <= 11) return 96;
        if (zoom <= 13) return 78;
        if (zoom <= 15) return 58;
        return 42;
      }"
    )

    leaflet(
      marker_df,
      options = leafletOptions(
        preferCanvas = TRUE,
        zoomSnap = 1,
        zoomDelta = 1,
        wheelPxPerZoomLevel = 160,
        wheelDebounceTime = 80,
        zoomAnimation = TRUE,
        zoomAnimationThreshold = 2,
        markerZoomAnimation = FALSE,
        fadeAnimation = FALSE
      )
    ) %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(updateWhenIdle = TRUE, keepBuffer = 1)
      ) %>%
      setView(lng = 106.70, lat = 10.78, zoom = 11) %>%
      addCircleMarkers(
        lng = ~map_lon, lat = ~map_lat,
        layerId = ~.map_layer_id,
        radius = ~ifelse(coord_status == "Tọa độ gốc từ nguồn", 5, 4),
        stroke = TRUE, weight = 1, color = "#ffffff",
        fillColor = ~price_color(price, price_cuts[[1]], price_cuts[[2]]),
        fillOpacity = ~ifelse(coord_status == "Tọa độ gốc từ nguồn", 0.82, 0.55),
        popup = ~popup_html,
        popupOptions = popupOptions(maxWidth = 340, closeButton = TRUE),
        clusterOptions = markerClusterOptions(
          showCoverageOnHover = FALSE,
          zoomToBoundsOnClick = TRUE,
          spiderfyOnMaxZoom = TRUE,
          spiderfyDistanceMultiplier = 1.6,
          removeOutsideVisibleBounds = TRUE,
          spiderLegPolylineOptions = list(weight = 1.2, color = "#64748b", opacity = 0.42),
          maxClusterRadius = cluster_radius,
          disableClusteringAtZoom = 18,
          animate = FALSE,
          animateAddingMarkers = FALSE,
          chunkedLoading = TRUE,
          chunkInterval = 80,
          chunkDelay = 25,
          iconCreateFunction = cluster_icon
        )
      )
  })

  output$area_price_plot <- renderPlotly({
    tx <- chart_transaction("area_price_tx")
    price_info <- price_display_info(tx)
    df <- known_rows_or_all(analysis_chart_data("area_price_tx"), "category_name") %>%
      mutate(display_price = .data[[price_info$value_col]]) %>%
      filter(!is.na(area), area > 0, !is.na(display_price), display_price > 0)
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", tx, "phù hợp bộ lọc.")))
    price_cutoff <- quantile(df$display_price, 0.98, na.rm = TRUE)
    plot_df <- df %>%
      filter(display_price <= price_cutoff) %>%
      plot_sample(max_n = 1600) %>%
      mutate(
        tooltip = paste0(
          "Giao dịch: ", tx,
          "<br>Loại BĐS: ", category_name,
          "<br>Khu vực cũ: ", district_name,
          "<br>Diện tích: ", format_number_vi(area, 1), " m²",
          "<br>Giá: ", format_number_vi(display_price, price_info$digits), " ", price_info$unit
        )
      )
    color_values <- setNames(chart_colors(n_distinct(plot_df$category_name)), sort(unique(plot_df$category_name)))
    p <- plot_df %>%
      ggplot(aes(x = area, y = display_price, color = category_name, text = tooltip)) +
      geom_point(alpha = 0.55, size = 1.7) +
      scale_color_manual(values = color_values) +
      guides(color = "none") +
      labs(x = "Diện tích (m²)", y = price_info$axis) +
      chart_theme()
    interactive_chart(p, tooltip = "text") %>%
      layout(showlegend = FALSE, margin = list(l = 92, r = 28, t = 16, b = 78))
  })

  output$price_m2_plot <- renderPlotly({
    tx <- chart_transaction("price_m2_tx")
    m2_info <- price_m2_display_info(tx)
    df <- known_rows_or_all(analysis_chart_data("price_m2_tx"), "district_name")
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", tx, "phù hợp bộ lọc.")))
    p <- df %>%
      filter(!is.na(price_per_m2), price_per_m2 > 0) %>%
      group_by(district_name) %>%
      summarise(median_price_m2 = median(price_per_m2, na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n >= 3) %>%
      slice_max(median_price_m2, n = 10) %>%
      mutate(display_price_m2 = median_price_m2 / m2_info$scale) %>%
      mutate(
        tooltip = paste0(
          "Giao dịch: ", tx,
          "<br>Khu vực cũ: ", district_name,
          "<br>Giá trung vị/m²: ", format_number_vi(display_price_m2, m2_info$digits), " ", m2_info$unit,
          "<br>Số tin: ", format_count_vi(n)
        )
      ) %>%
      ggplot(aes(x = reorder(district_name, display_price_m2), y = display_price_m2, text = tooltip)) +
      geom_col(fill = "#059669", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = m2_info$axis) +
      chart_theme()
    interactive_chart(p, tooltip = "text")
  })

  output$price_category_plot <- renderPlotly({
    selected_transaction <- chart_transaction("price_category_tx")
    price_info <- price_display_info(selected_transaction)
    df <- known_rows_or_all(analysis_chart_data("price_category_tx"), "category_name") %>%
      mutate(display_price = .data[[price_info$value_col]]) %>%
      filter(!is.na(display_price), display_price > 0)
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", selected_transaction, "phù hợp bộ lọc.")))
    price_cutoff <- quantile(df$display_price, 0.98, na.rm = TRUE)

    summary_df <- df %>%
      filter(display_price <= price_cutoff) %>%
      filter(!is.na(category_name), category_name != "") %>%
      group_by(category_name) %>%
      summarise(
        q1_price = quantile(display_price, 0.25, na.rm = TRUE),
        median_price = median(display_price, na.rm = TRUE),
        q3_price = quantile(display_price, 0.75, na.rm = TRUE),
        listing_count = n(),
        .groups = "drop"
      ) %>%
      filter(listing_count >= 3) %>%
      slice_max(listing_count, n = 10) %>%
      arrange(median_price) %>%
      mutate(
        category_label = factor(category_name, levels = category_name),
        tooltip = paste0(
          "Giao dịch: ", selected_transaction,
          "<br>Loại BĐS: ", category_name,
          "<br>Giá trung vị: ", format_number_vi(median_price, price_info$digits), " ", price_info$unit,
          "<br>Vùng phổ biến: ", format_number_vi(q1_price, price_info$digits), " - ", format_number_vi(q3_price, price_info$digits), " ", price_info$unit,
          "<br>Số tin: ", format_count_vi(listing_count)
        )
      )
    validate(need(nrow(summary_df) > 0, "Không có dữ liệu phù hợp để vẽ biểu đồ."))

    plot_ly(
      summary_df,
      x = ~median_price,
      y = ~category_label,
      type = "scatter",
      mode = "markers",
      text = ~tooltip,
      hovertemplate = "%{text}<extra></extra>",
      marker = list(size = 10, color = "#0072bc", line = list(color = "#ffffff", width = 1.5)),
      error_x = list(
        type = "data",
        symmetric = FALSE,
        array = ~q3_price - median_price,
        arrayminus = ~median_price - q1_price,
        color = "#0072bc",
        thickness = 2,
        width = 4
      )
    ) %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 150, r = 20, t = 36, b = 60),
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#d7e6f5", font = list(color = "#1f2937")),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        annotations = list(list(
          x = 0,
          y = 1.12,
          xref = "paper",
          yref = "paper",
          text = paste0("Đang hiển thị giao dịch: ", selected_transaction),
          showarrow = FALSE,
          xanchor = "left",
          font = list(size = 12, color = "#64748b")
        )),
        xaxis = list(title = price_info$axis, automargin = TRUE, gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb"),
        yaxis = list(title = "", automargin = TRUE, categoryorder = "array", categoryarray = levels(summary_df$category_label))
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$log_price_plot <- renderPlotly({
    tx <- chart_transaction("log_price_tx")
    plot_df <- analysis_chart_data("log_price_tx") %>%
      filter(!is.na(price), price > 0) %>%
      mutate(price_index = log1p(price))
    validate(need(nrow(plot_df) > 0, paste("Không có dữ liệu", tx, "phù hợp bộ lọc.")))

    plot_ly(
      plot_df,
      x = ~price_index,
      type = "histogram",
      nbinsx = 28,
      marker = list(color = "#d97706", line = list(color = "#ffffff", width = 0.5)),
      hovertemplate = paste0("Giao dịch: ", tx, "<br>Mức giá chuẩn hóa: %{x:.2f}<br>Số tin: %{y}<extra></extra>")
    ) %>%
      layout(
        margin = list(l = 62, r = 18, t = 16, b = 54),
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#d7e6f5", font = list(color = "#1f2937")),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        bargap = 0.04,
        xaxis = list(title = "Mức giá chuẩn hóa", automargin = TRUE, gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb"),
        yaxis = list(title = "Số tin", automargin = TRUE, gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$district_category_heatmap <- renderPlotly({
    tx <- chart_transaction("district_category_heatmap_tx")
    m2_info <- price_m2_display_info(tx)
    df <- analysis_chart_data("district_category_heatmap_tx") %>%
      filter(finite_positive(price_per_m2))
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", tx, "phù hợp bộ lọc.")))

    top_districts <- known_rows_or_all(df, "district_name") %>%
      count(district_name, sort = TRUE) %>%
      slice_head(n = 10) %>%
      pull(district_name)
    top_categories <- known_rows_or_all(df, "category_name") %>%
      count(category_name, sort = TRUE) %>%
      slice_head(n = 8) %>%
      pull(category_name)

    summary_df <- df %>%
      filter(district_name %in% top_districts, category_name %in% top_categories) %>%
      group_by(district_name, category_name) %>%
      summarise(display_m2 = median(price_per_m2, na.rm = TRUE) / m2_info$scale, n = n(), .groups = "drop")

    z <- matrix(NA_real_, nrow = length(top_categories), ncol = length(top_districts), dimnames = list(top_categories, top_districts))
    for (i in seq_len(nrow(summary_df))) {
      z[summary_df$category_name[[i]], summary_df$district_name[[i]]] <- summary_df$display_m2[[i]]
    }

    plot_ly(
      x = top_districts,
      y = top_categories,
      z = z,
      type = "heatmap",
      colorscale = "Viridis",
      hovertemplate = paste0(
        "Khu vực: %{x}<br>Loại BĐS: %{y}<br>Giá/m² trung vị: %{z:.1f} ",
        m2_info$unit,
        "<extra></extra>"
      )
    ) %>%
      layout(
        margin = list(l = 158, r = 24, t = 12, b = 120),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        xaxis = list(title = "", automargin = TRUE, tickangle = -35, tickfont = list(size = 10)),
        yaxis = list(title = "", automargin = TRUE)
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$source_sunburst_plot <- renderPlotly({
    tx <- chart_transaction("source_radar_tx")
    df <- analysis_chart_data("source_radar_tx") %>%
      filter(!is_missing_label(source))
    validate(need(nrow(df) > 0, paste("Không có dữ liệu", tx, "phù hợp bộ lọc để vẽ radar nguồn.")))

    top_sources <- df %>%
      count(source, sort = TRUE) %>%
      slice_head(n = 6) %>%
      pull(source)

    has_other <- any(!df$source %in% top_sources)
    source_levels <- c(top_sources, if (has_other) "other_source")
    source_labels <- ifelse(source_levels == "other_source", "Khác", source_label_vi(source_levels))

    df <- df %>%
      mutate(source_group = if_else(source %in% top_sources, as.character(source), "other_source"))

    radar_grid <- expand.grid(
      source_group = source_levels,
      stringsAsFactors = FALSE
    )

    source_counts <- df %>%
      count(source_group, name = "listings")
    total_listings <- sum(source_counts$listings, na.rm = TRUE)

    plot_df <- radar_grid %>%
      left_join(source_counts, by = "source_group") %>%
      mutate(
        listings = coalesce(listings, 0L),
        share = if (total_listings > 0) listings / total_listings * 100 else 0,
        source_label = source_labels[match(source_group, source_levels)],
        tooltip = paste0(
          "Giao dịch: ", tx,
          "<br>Nguồn: ", source_label,
          "<br>Tỷ trọng: ", format_number_vi(share, 1), "%",
          "<br>Số tin: ", format_count_vi(listings)
        )
      ) %>%
      arrange(match(source_group, source_levels))
    plot_df <- bind_rows(plot_df, plot_df[1, , drop = FALSE])

    axis_max <- max(5, ceiling(max(plot_df$share, na.rm = TRUE) / 5) * 5)
    color_values <- c("Bán" = "#0072bc", "Cho thuê" = "#10b981")
    fill_values <- c("Bán" = "rgba(0, 114, 188, 0.18)", "Cho thuê" = "rgba(16, 185, 129, 0.18)")

    polar_base <- list(
      bgcolor = "rgba(0,0,0,0)",
      domain = list(x = c(0.08, 0.92), y = c(0, 1)),
      radialaxis = list(
        visible = TRUE,
        range = c(0, axis_max),
        ticksuffix = "%",
        gridcolor = "#e5e7eb",
        linecolor = "#cbd5e1",
        tickfont = list(size = 10, color = "#64748b")
      ),
      angularaxis = list(
        gridcolor = "#e5e7eb",
        linecolor = "#cbd5e1",
        tickfont = list(size = 10, color = "#334155")
      )
    )

    plot_ly() %>%
      add_trace(
        data = plot_df,
        type = "scatterpolar",
        mode = "lines+markers",
        r = ~share,
        theta = ~source_label,
        text = ~tooltip,
        hovertemplate = "%{text}<extra></extra>",
        fill = "toself",
        opacity = 0.68,
        line = list(color = color_values[[tx]], width = 3),
        marker = list(color = color_values[[tx]], size = 7),
        fillcolor = fill_values[[tx]],
        showlegend = FALSE
      ) %>%
      layout(
        margin = list(l = 72, r = 72, t = 8, b = 20),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#d7e6f5", font = list(color = "#1f2937")),
        showlegend = FALSE,
        polar = polar_base
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$time_trend_plot <- renderPlotly({
    tx <- chart_transaction("time_trend_tx")
    m2_info <- price_m2_display_info(tx)
    df <- analysis_chart_data("time_trend_tx") %>%
      mutate(posted_date = as.Date(posted_at)) %>%
      filter(!is.na(posted_date), posted_date <= Sys.Date(), finite_positive(price_per_m2))
    validate(need(nrow(df) > 0, paste("Không có dữ liệu ngày hợp lệ cho", tx)))

    trend_df <- df %>%
      mutate(posted_month = lubridate::floor_date(posted_date, unit = "month")) %>%
      group_by(posted_month) %>%
      summarise(
        n = n(),
        median_m2 = median(price_per_m2, na.rm = TRUE) / m2_info$scale,
        .groups = "drop"
      ) %>%
      arrange(posted_month)

    plot_ly(trend_df, x = ~posted_month) %>%
      add_bars(
        y = ~n,
        name = "Số tin",
        marker = list(color = "rgba(0,114,188,0.25)"),
        hovertemplate = "Tháng: %{x|%m/%Y}<br>Số tin: %{y}<extra></extra>"
      ) %>%
      add_lines(
        y = ~median_m2,
        name = paste0("Giá/m² trung vị (", m2_info$unit, ")"),
        yaxis = "y2",
        line = list(color = "#ef4444", width = 3),
        hovertemplate = paste0("Tháng: %{x|%m/%Y}<br>Giá/m²: %{y:.1f} ", m2_info$unit, "<extra></extra>")
      ) %>%
      layout(
        barmode = "overlay",
        margin = list(l = 82, r = 82, t = 12, b = 96),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        legend = list(orientation = "h", x = 0, y = -0.28, font = list(size = 11), itemwidth = 30),
        xaxis = list(title = "", automargin = TRUE, gridcolor = "#e5e7eb"),
        yaxis = list(title = "Số tin", gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb"),
        yaxis2 = list(title = m2_info$unit, overlaying = "y", side = "right", showgrid = FALSE)
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$correlation_plot <- renderPlotly({
    tx <- chart_transaction("correlation_tx")
    df <- analysis_chart_data("correlation_tx") %>%
      mutate(
        log_price = log1p(price),
        log_area = log1p(area),
        log_price_m2 = log1p(price_per_m2)
      )
    numeric_cols <- intersect(
      c("log_price", "log_price_m2", "log_area", "rooms", "distance_to_center", "listing_age_days", "title_token_count"),
      names(df)
    )
    validate(need(length(numeric_cols) >= 2, "Chưa đủ biến số để tính tương quan."))
    cor_df <- df[, numeric_cols, drop = FALSE]
    cor_df[] <- lapply(cor_df, function(x) suppressWarnings(as.numeric(x)))
    validate(need(sum(stats::complete.cases(cor_df)) >= 20, paste("Không đủ dữ liệu số cho", tx)))

    cor_mat <- stats::cor(cor_df, use = "pairwise.complete.obs")
    labels <- feature_label_vi(colnames(cor_mat))
    plot_ly(
      x = labels,
      y = labels,
      z = cor_mat,
      type = "heatmap",
      zmin = -1,
      zmax = 1,
      colorscale = list(c(0, "#b91c1c"), c(0.5, "#ffffff"), c(1, "#0072bc")),
      hovertemplate = "Biến X: %{x}<br>Biến Y: %{y}<br>Tương quan: %{z:.2f}<extra></extra>"
    ) %>%
      layout(
        margin = list(l = 150, r = 24, t = 12, b = 120),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        xaxis = list(title = "", automargin = TRUE, tickangle = -35, tickfont = list(size = 10)),
        yaxis = list(title = "", automargin = TRUE)
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$price_ecdf_plot <- renderPlotly({
  tx <- chart_transaction("ecdf_tx")
  m2_info <- price_m2_display_info(tx)

  df_base <- analysis_chart_data("ecdf_tx") %>%
    filter(finite_positive(price_per_m2)) %>%
    mutate(display_m2 = price_per_m2 / m2_info$scale)

  validate(need(nrow(df_base) > 0, paste("Không có dữ liệu", tx, "để vẽ phân phối tích lũy.")))

  top_districts <- df_base %>%
    count(district_name, sort = TRUE) %>%
    head(4) %>%
    pull(district_name)

  target_districts <- unique(c(top_districts, "Quận 1", "Quận Bình Tân"))

  cutoff_price <- quantile(df_base$display_m2, 0.95, na.rm = TRUE)

  df_plot <- df_base %>%
    filter(district_name %in% target_districts) %>%
    filter(display_m2 <= cutoff_price)

  df_plot <- df_plot %>%
    arrange(district_name, display_m2) %>%
    group_by(district_name) %>%
    mutate(ecdf_prob = row_number() / n()) %>%
    ungroup()

  p <- ggplot(df_plot, aes(x = display_m2, y = ecdf_prob, color = district_name)) +
    geom_step(linewidth = 1) +
    labs(x = m2_info$axis, y = "Xác suất tích lũy", color = "Khu vực") +
    chart_theme()

  interactive_chart(p) %>%
    layout(
      legend = list(orientation = "h", x = 0, y = 1.1),
      margin = list(l = 60, r = 20, t = 40, b = 60)
    )
})

  output$stat_kpi_cards <- renderUI({
    df <- stat_base_data()
    tx <- input$stat_transaction %||% "Bán"
    total_valid <- listings() %>%
      filter(finite_positive(price_per_m2), finite_positive(price)) %>%
      nrow()
    m2_info <- price_m2_display_info(tx)
    values <- df$price_per_m2[finite_positive(df$price_per_m2)]
    q75 <- safe_quantile(values, 0.75)
    se_log <- if (length(values) >= 2) stats::sd(log1p(values), na.rm = TRUE) / sqrt(length(values)) else NA_real_
    median_label <- if (length(values) > 0) {
      paste0(format_number_vi(median(values / m2_info$scale, na.rm = TRUE), m2_info$digits), " ", m2_info$unit)
    } else {
      "Chưa có dữ liệu"
    }
    high_prob_label <- if (length(values) > 0 && is.finite(q75[[1]])) {
      paste0(round(mean(values >= q75[[1]], na.rm = TRUE) * 100, 1), "%")
    } else {
      "Chưa có dữ liệu"
    }
    div(
      class = "kpi-grid",
      kpi_card(
        paste0("Cỡ mẫu ", tx),
        format_count_vi(nrow(df)),
        paste0("trong tổng ", format_count_vi(total_valid), " dòng hợp lệ"),
        "database",
        "default"
      ),
      kpi_card("Trung vị giá/m²", median_label, "thống kê mẫu", "chart-line", "warning"),
      kpi_card("Sai số chuẩn", format_number_vi(se_log, 4), "trên log(giá/m²)", "ruler", "success"),
      kpi_card("P(giá cao)", high_prob_label, "ngưỡng Q3 của nhóm lọc", "percent", "danger")
    )
  })

  output$probability_heatmap <- renderPlotly({
    df <- stat_base_data() %>%
      filter(!is_missing_label(district_name), !is_missing_label(category_name))
    validate(need(nrow(df) > 0, "Không có dữ liệu để tính xác suất có điều kiện."))

    top_districts <- df %>% count(district_name, sort = TRUE) %>% slice_head(n = 10) %>% pull(district_name)
    top_categories <- df %>% count(category_name, sort = TRUE) %>% slice_head(n = 8) %>% pull(category_name)
    prob_df <- df %>%
      filter(district_name %in% top_districts, category_name %in% top_categories) %>%
      count(district_name, category_name) %>%
      group_by(district_name) %>%
      mutate(prob = n / sum(n)) %>%
      ungroup()

    z <- matrix(0, nrow = length(top_categories), ncol = length(top_districts), dimnames = list(top_categories, top_districts))
    for (i in seq_len(nrow(prob_df))) {
      z[prob_df$category_name[[i]], prob_df$district_name[[i]]] <- prob_df$prob[[i]]
    }

    plot_ly(
      x = top_districts,
      y = top_categories,
      z = z,
      type = "heatmap",
      colorscale = "Blues",
      hovertemplate = "Khu vực: %{x}<br>Loại BĐS: %{y}<br>P(loại | khu vực): %{z:.1%}<extra></extra>"
    ) %>%
      layout(
        margin = list(l = 158, r = 24, t = 12, b = 120),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        xaxis = list(title = "", automargin = TRUE, tickangle = -35, tickfont = list(size = 10)),
        yaxis = list(title = "", automargin = TRUE)
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$stat_distribution_plot <- renderPlotly({
    tx <- input$stat_transaction %||% "Bán"
    m2_info <- price_m2_display_info(tx)
    districts <- unique(c(stat_selected_district_a(), stat_selected_district_b()))
    df <- stat_base_data() %>%
      filter(district_name %in% districts, finite_positive(price_per_m2)) %>%
      filter_price_m2_chart_outliers() %>%
      mutate(display_m2 = price_per_m2 / m2_info$scale)
    validate(need(nrow(df) > 0, "Chưa có dữ liệu cho hai khu vực đã chọn."))

    color_values <- setNames(chart_colors(n_distinct(df$district_name)), sort(unique(df$district_name)))
    p <- df %>%
      ggplot(aes(x = display_m2, color = district_name)) +
      stat_ecdf(linewidth = 1) +
      scale_color_manual(values = color_values) +
      labs(x = paste0("Giá/m² (", m2_info$unit, ")"), y = "Xác suất tích lũy", color = "Khu vực") +
      chart_theme()
    interactive_chart(p)
  })

  output$clt_plot <- renderPlotly({
    tx <- input$stat_transaction %||% "Bán"
    m2_info <- price_m2_display_info(tx)
    values <- price_m2_chart_values(stat_base_data()$price_per_m2)
    sample_size <- max(10, min(300, as.integer(input$stat_sample_size %||% 50)))
    reps <- max(200, min(1500, as.integer(input$stat_reps %||% 600)))
    simulation_seed <- 2026 + sample_size * 31 + reps * 17
    means <- bootstrap_mean_distribution(values / m2_info$scale, sample_size, reps, seed = simulation_seed)
    validate(need(length(means) > 0, "Cần ít nhất vài dòng giá/m² hợp lệ để mô phỏng trung bình mẫu."))
    observed_mean <- mean(values / m2_info$scale, na.rm = TRUE)

    plot_ly(x = means, type = "histogram", nbinsx = 34, marker = list(color = "#0072bc", line = list(color = "#ffffff", width = 0.5))) %>%
      layout(
        title = list(
          text = paste0("Cỡ mẫu = ", sample_size, " | Số lần lặp = ", reps),
          x = 0,
          xanchor = "left",
          font = list(size = 13)
        ),
        shapes = list(list(type = "line", x0 = observed_mean, x1 = observed_mean, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#ef4444", width = 3))),
        annotations = list(list(x = observed_mean, y = 1, yref = "paper", text = "Trung bình mẫu gốc", showarrow = FALSE, xanchor = "left", font = list(color = "#ef4444"))),
        margin = list(l = 62, r = 20, t = 46, b = 62),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        bargap = 0.04,
        xaxis = list(title = paste0("Trung bình mẫu giá/m² (", m2_info$unit, ")"), gridcolor = "#e5e7eb"),
        yaxis = list(title = "Số lần lặp", gridcolor = "#e5e7eb"),
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$bootstrap_plot <- renderPlotly({
    tx <- input$stat_transaction %||% "Bán"
    m2_info <- price_m2_display_info(tx)
    district <- stat_selected_district_a()
    values <- stat_base_data() %>%
      filter(district_name == district, finite_positive(price_per_m2)) %>%
      pull(price_per_m2)
    values <- price_m2_chart_values(values) / m2_info$scale
    boot <- bootstrap_median_ci(values, input$stat_reps, confidence_level_value(input$stat_confidence))
    validate(need(length(boot$distribution) > 0, "Cần tối thiểu 5 dòng cho bootstrap."))

    plot_ly(x = boot$distribution, type = "histogram", nbinsx = 34, marker = list(color = "#10b981", line = list(color = "#ffffff", width = 0.5))) %>%
      layout(
        shapes = list(
          list(type = "line", x0 = boot$lower, x1 = boot$lower, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#f59e0b", width = 2, dash = "dash")),
          list(type = "line", x0 = boot$upper, x1 = boot$upper, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#f59e0b", width = 2, dash = "dash")),
          list(type = "line", x0 = boot$observed, x1 = boot$observed, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#ef4444", width = 3))
        ),
        annotations = list(
          list(
            x = boot$lower, y = 0.95, yref = "paper",
            text = format_number_vi(boot$lower, m2_info$digits),
            showarrow = FALSE, xanchor = "left", font = list(color = "#d97706")
          ),
          list(
            x = boot$upper, y = 0.95, yref = "paper",
            text = format_number_vi(boot$upper, m2_info$digits),
            showarrow = FALSE, xanchor = "right", font = list(color = "#d97706")
          ),
          list(
            x = boot$observed, y = 1, yref = "paper",
            text = format_number_vi(boot$observed, m2_info$digits),
            showarrow = FALSE, xanchor = "left", font = list(color = "#ef4444")
          )
        ),
        margin = list(l = 62, r = 20, t = 12, b = 62),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        bargap = 0.04,
        xaxis = list(title = paste0("Trung vị bootstrap giá/m² (", m2_info$unit, ")"), gridcolor = "#e5e7eb"),
        yaxis = list(title = "Số lần lặp", gridcolor = "#e5e7eb"),
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$hypothesis_table <- renderTable({
    tx <- input$stat_transaction %||% "Bán"
    m2_info <- price_m2_display_info(tx)
    district_a <- stat_selected_district_a()
    district_b <- stat_selected_district_b()
    alpha <- 0.05
    test_df <- stat_base_data() %>%
      filter(district_name %in% c(district_a, district_b), finite_positive(price_per_m2)) %>%
      mutate(group = district_name, log_m2 = log1p(price_per_m2))

    if (length(unique(test_df$group)) < 2 || nrow(test_df) < 10) {
      return(data.frame(Ket_qua = "Chưa đủ dữ liệu để kiểm định hai nhóm."))
    }

    group_a <- test_df %>% filter(group == district_a)
    group_b <- test_df %>% filter(group == district_b)
    t_result <- tryCatch(stats::t.test(log_m2 ~ group, data = test_df), error = function(e) NULL)
    w_result <- tryCatch(stats::wilcox.test(log_m2 ~ group, data = test_df), error = function(e) NULL)
    diff_median <- median(group_a$price_per_m2, na.rm = TRUE) - median(group_b$price_per_m2, na.rm = TRUE)

    tibble::tibble(
      `Mục` = c("H0", "Nhóm A", "Nhóm B", "Chênh lệch trung vị A-B", "p-value kiểm định t", "p-value Wilcoxon", "Kết luận α=0,05"),
      `Giá trị` = c(
        "Trung bình log(giá/m²) hai khu vực bằng nhau",
        paste0(district_a, " · n=", format_count_vi(nrow(group_a))),
        paste0(district_b, " · n=", format_count_vi(nrow(group_b))),
        paste0(format_number_vi(diff_median / m2_info$scale, m2_info$digits), " ", m2_info$unit),
        if (is.null(t_result)) "NA" else p_value_label(t_result$p.value),
        if (is.null(w_result)) "NA" else p_value_label(w_result$p.value),
        if (is.null(t_result)) "Không đủ dữ liệu" else hypothesis_decision(t_result$p.value, alpha)
      )
    )
  })

  output$empirical_probability_table <- renderTable({
    df <- stat_base_data()
    district <- stat_selected_district_a()
    category <- input$stat_category
    top_category <- df %>%
      filter(!is_missing_label(category_name)) %>%
      count(category_name, sort = TRUE) %>%
      slice(1) %>%
      pull(category_name)
    if (length(top_category) == 0) top_category <- "Không rõ"
    focus_category <- if (is_selected_filter(category)) category[[1]] else top_category
    q75 <- safe_quantile(df$price_per_m2, 0.75)

    tibble::tibble(
      `Xác suất thực nghiệm` = c(
        paste0("P(khu vực = ", district, ")"),
        paste0("P(loại BĐS = ", focus_category, ")"),
        paste0("P(giá/m² >= Q3)"),
        paste0("P(", focus_category, " | ", district, ")"),
        paste0("P(tọa độ gốc từ nguồn)")
      ),
      `Giá trị` = paste0(round(c(
        mean(df$district_name == district, na.rm = TRUE),
        mean(df$category_name == focus_category, na.rm = TRUE),
        mean(df$price_per_m2 >= q75[[1]], na.rm = TRUE),
        {
          district_df <- df %>% filter(district_name == district)
          if (nrow(district_df) == 0) NA_real_ else mean(district_df$category_name == focus_category, na.rm = TRUE)
        },
        mean(df$coord_status == "Tọa độ gốc từ nguồn", na.rm = TRUE)
      ) * 100, 1), "%")
    )
  })

  prediction <- eventReactive(input$predict_btn, {
    req(input$pred_district, input$pred_category, input$predict_transaction, input$predict_area, input$predict_rooms)
    area_val <- as.numeric(input$predict_area)
    if (is.na(area_val) || area_val <= 0) return(NA_real_)
    df <- listings()
    if (nrow(df) == 0) return(NA_real_)

    input_row <- build_prediction_row(
      df = df,
      district = input$pred_district,
      category = input$pred_category,
      ward = input$predict_ward,
      area = input$predict_area,
      rooms = input$predict_rooms,
      transaction_type = input$predict_transaction
    )

    predict_price(input_row, input_row$is_rent[[1]])
  }, ignoreInit = TRUE)

  output$prediction_text <- renderText({
    pred <- prediction()
    if (is.null(pred) || is.na(pred)) "Chưa dự đoán được" else format_vnd_full(pred)
  })

  output$prediction_note <- renderText({
    pred <- prediction()
    if (is.null(pred) || is.na(pred)) {
      "Hãy chọn khu vực cũ và loại bất động sản có trong dữ liệu huấn luyện."
    } else {
      paste("Giao dịch:", input$predict_transaction, "· Loại:", input$pred_category, "· Khu vực:", input$pred_district, "· Diện tích:", input$predict_area, "m²")
    }
  })

  output$prediction_model_note <- renderText({
    is_rent_pred <- identical(input$predict_transaction, "Cho thuê")
    paste0("Mô hình: ", prediction_model_label(is_rent_pred), " · giá trị mang tính tham khảo")
  })

  output$prediction_market_band <- renderUI({
    pred <- prediction()
    if (is.null(pred) || is.na(pred)) return(NULL)
    band <- prediction_market_band(
      listings(),
      input$pred_district,
      input$pred_category,
      input$predict_transaction,
      input$predict_area
    )
    error_band <- prediction_error_band(
      listings(),
      input$pred_district,
      input$pred_category,
      input$predict_transaction,
      input$predict_area,
      pred
    )
    error_band_chips <- if (is.null(error_band)) {
      NULL
    } else if (is.finite(error_band$lower) && is.finite(error_band$upper)) {
      list(
        div(class = "filter-chip",
            div(class = "filter-chip-label", "Khoảng dự đoán"),
            div(class = "filter-chip-value", paste(format_vnd(error_band$lower), "-", format_vnd(error_band$upper)))),
        div(class = "filter-chip",
            div(class = "filter-chip-label", "Mẫu sai số mô hình"),
            div(class = "filter-chip-value", paste0(format_count_vi(error_band$n), " dòng · ", error_band$confidence)))
      )
    } else {
      list(
        div(class = "filter-chip",
            div(class = "filter-chip-label", "Khoảng dự đoán"),
            div(class = "filter-chip-value", "Cần thêm mẫu")),
        div(class = "filter-chip",
            div(class = "filter-chip-label", "Mẫu sai số mô hình"),
            div(class = "filter-chip-value", paste0(format_count_vi(error_band$n), " dòng · ", error_band$confidence)))
      )
    }
    div(
      class = "filter-summary full",
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Mẫu tương đồng"),
          div(class = "filter-chip-value", paste0(format_count_vi(band$n), " tin"))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Q1 thị trường"),
          div(class = "filter-chip-value", format_vnd(band$lower))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Trung vị thị trường"),
          div(class = "filter-chip-value", format_vnd(band$median))),
      div(class = "filter-chip",
          div(class = "filter-chip-label", "Q3 thị trường"),
          div(class = "filter-chip-value", format_vnd(band$upper))),
      error_band_chips
    )
  })

  output$importance_plot <- renderPlotly({
    is_rent_pred <- identical(input$predict_transaction, "Cho thuê")
    path <- if (is_rent_pred) {
      if (file.exists(RF_IMPORTANCE_RENT_PATH)) RF_IMPORTANCE_RENT_PATH else RF_IMPORTANCE_SALE_PATH
    } else {
      if (file.exists(RF_IMPORTANCE_SALE_PATH)) RF_IMPORTANCE_SALE_PATH else RF_IMPORTANCE_RENT_PATH
    }
    validate(need(file.exists(path), "Chưa có dữ liệu mức ảnh hưởng biến."))
    p <- read_csv(path, show_col_types = FALSE) %>%
      slice_head(n = 10) %>%
      mutate(
        feature_label = feature_label_vi(feature),
        tooltip = paste0("Yếu tố: ", feature_label, "<br>Mức ảnh hưởng: ", format_number_vi(IncNodePurity, 1))
      ) %>%
      ggplot(aes(x = reorder(feature_label, IncNodePurity), y = IncNodePurity, text = tooltip)) +
      geom_col(fill = "#0072bc", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "Mức ảnh hưởng") +
      chart_theme()
    interactive_chart(p, tooltip = "text")
  })

  output$model_card_ui <- renderUI({
    tx <- chart_transaction("diagnostic_tx")
    segment_key <- if (identical(tx, "Cho thuê")) "rent" else "sale"
    m <- metrics() %>% filter(segment == segment_key)
    r <- registry() %>% filter(segment == segment_key)
    diag <- diagnostic_data()
    best <- if (nrow(r) > 0) model_label_vi(r$best_model[[1]]) else best_model_name_only(m)
    best_mape <- if (nrow(r) > 0) paste0("MAPE ", round(r$mape[[1]] * 100, 1), "%") else best_model_mape_only(m)
    diag_mape <- if (nrow(diag) > 0) paste0(round(mean(diag$ape, na.rm = TRUE) * 100, 1), "%") else "NA"
    diag_residual <- if (nrow(diag) > 0) format_number_vi(stats::sd(diag$residual_log, na.rm = TRUE), 3) else "NA"

    div(
      class = "kpi-grid",
      kpi_card("Nhóm mô hình", tx, "bán và thuê được huấn luyện riêng", "tags", "default", value_class = "text-mode"),
      kpi_card("Mô hình tốt nhất", best, "chọn theo sai số kiểm định", "bullseye", "success", delta = best_mape, value_class = "text-mode"),
      kpi_card("MAPE kiểm tra", diag_mape, "mẫu dự đoán lại từ dữ liệu hiện có", "chart-simple", "warning"),
      kpi_card("Độ lệch sai số", diag_residual, "độ phân tán sai số log", "ruler", "danger")
    )
  })

  output$diagnostic_scatter_plot <- renderPlotly({
    tx <- chart_transaction("diagnostic_tx")
    price_info <- price_display_info(tx)
    df <- diagnostic_data() %>%
      mutate(
        actual_display = actual_price / ifelse(identical(tx, "Cho thuê"), 1e6, 1e9),
        predicted_display = predicted_price / ifelse(identical(tx, "Cho thuê"), 1e6, 1e9),
        tooltip = paste0(
          "Khu vực: ", district_name,
          "<br>Loại BĐS: ", category_name,
          "<br>Giá thực tế: ", format_number_vi(actual_display, price_info$digits), " ", price_info$unit,
          "<br>Giá dự đoán: ", format_number_vi(predicted_display, price_info$digits), " ", price_info$unit,
          "<br>APE: ", round(ape * 100, 1), "%"
        )
      )
    validate(need(nrow(df) > 0, "Chưa có dữ liệu kiểm tra mô hình."))

    max_axis <- safe_quantile(c(df$actual_display, df$predicted_display), 0.98)
    p <- df %>%
      filter(actual_display <= max_axis[[1]], predicted_display <= max_axis[[1]]) %>%
      ggplot(aes(x = actual_display, y = predicted_display, color = category_name, text = tooltip)) +
      geom_point(alpha = 0.55, size = 1.8) +
      geom_abline(slope = 1, intercept = 0, color = "#ef4444", linewidth = 0.9) +
      guides(color = "none") +
      labs(x = paste0("Giá thực tế (", price_info$unit, ")"), y = paste0("Giá dự đoán (", price_info$unit, ")")) +
      chart_theme()
    interactive_chart(p, tooltip = "text") %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 92, r = 28, t = 16, b = 78),
        xaxis = list(automargin = TRUE, title = list(standoff = 18), gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb"),
        yaxis = list(automargin = TRUE, title = list(standoff = 18), gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb")
      )
  })

  output$diagnostic_residual_plot <- renderPlotly({
    df <- diagnostic_data()
    validate(need(nrow(df) > 0, "Chưa có dữ liệu sai số."))
    plot_ly(
      df,
      x = ~residual_log,
      type = "histogram",
      nbinsx = 36,
      marker = list(color = "#f59e0b", line = list(color = "#ffffff", width = 0.5)),
      hovertemplate = "Sai số log: %{x:.3f}<br>Số dòng: %{y}<extra></extra>"
    ) %>%
      layout(
        shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#ef4444", width = 3))),
        margin = list(l = 82, r = 28, t = 16, b = 86),
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#d7e6f5", font = list(color = "#1f2937")),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        bargap = 0.04,
        xaxis = list(title = list(text = "Sai số log(giá thực tế) - log(giá dự đoán)", standoff = 18), automargin = TRUE, gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb"),
        yaxis = list(title = list(text = "Số dòng", standoff = 18), automargin = TRUE, gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$diagnostic_error_heatmap <- renderPlotly({
    df <- diagnostic_data()
    validate(need(nrow(df) > 0, "Chưa có dữ liệu để vẽ heatmap sai số."))

    top_districts <- df %>%
      filter(!is_missing_label(district_name)) %>%
      count(district_name, sort = TRUE) %>%
      slice_head(n = 12) %>%
      pull(district_name)
    top_categories <- df %>%
      filter(!is_missing_label(category_name)) %>%
      count(category_name, sort = TRUE) %>%
      slice_head(n = 8) %>%
      pull(category_name)

    plot_df <- df %>%
      filter(district_name %in% top_districts, category_name %in% top_categories) %>%
      group_by(district_name, category_name) %>%
      summarise(
        mape = mean(ape, na.rm = TRUE) * 100,
        median_residual = median(residual_log, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      filter(n >= 4)
    validate(need(nrow(plot_df) > 0, "Cần ít nhất 4 dòng cho mỗi nhóm khu vực - loại BĐS."))

    z <- matrix(NA_real_, nrow = length(top_categories), ncol = length(top_districts), dimnames = list(top_categories, top_districts))
    text <- matrix("", nrow = length(top_categories), ncol = length(top_districts), dimnames = list(top_categories, top_districts))
    for (i in seq_len(nrow(plot_df))) {
      category <- plot_df$category_name[[i]]
      district <- plot_df$district_name[[i]]
      z[category, district] <- plot_df$mape[[i]]
      text[category, district] <- paste0(
        "Khu vực: ", district,
        "<br>Loại BĐS: ", category,
        "<br>MAPE: ", format_number_vi(plot_df$mape[[i]], 1), "%",
        "<br>Trung vị residual log: ", format_number_vi(plot_df$median_residual[[i]], 3),
        "<br>Số dòng: ", format_count_vi(plot_df$n[[i]])
      )
    }

    plot_ly(
      x = top_districts,
      y = top_categories,
      z = z,
      text = text,
      type = "heatmap",
      colorscale = list(
        list(0, "#e0f2fe"),
        list(0.45, "#facc15"),
        list(0.75, "#f97316"),
        list(1, "#dc2626")
      ),
      colorbar = list(title = "MAPE (%)"),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        margin = list(l = 178, r = 42, t = 16, b = 130),
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#d7e6f5", font = list(color = "#1f2937")),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        xaxis = list(title = "", automargin = TRUE, tickangle = -35, tickfont = list(size = 10)),
        yaxis = list(title = "", automargin = TRUE, tickfont = list(size = 10))
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$diagnostic_error_group_plot <- renderPlotly({
    df <- diagnostic_data()
    validate(need(nrow(df) > 0, "Chưa có dữ liệu kiểm tra mô hình theo khu vực."))
    plot_df <- df %>%
      group_by(district_name) %>%
      summarise(mape = mean(ape, na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n >= 10) %>%
      slice_max(mape, n = 12) %>%
      mutate(
        tooltip = paste0("Khu vực: ", district_name, "<br>MAPE kiểm tra: ", round(mape * 100, 1), "%<br>Số dòng: ", format_count_vi(n))
      )
    validate(need(nrow(plot_df) > 0, "Cần ít nhất 10 dòng/khu vực để vẽ sai số nhóm."))
    p <- plot_df %>%
      ggplot(aes(x = reorder(district_name, mape), y = mape * 100, text = tooltip)) +
      geom_col(fill = "#ef4444", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "MAPE (%)") +
      chart_theme()
    interactive_chart(p, tooltip = "text") %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 178, r = 28, t = 16, b = 82),
        xaxis = list(automargin = TRUE, title = list(standoff = 18), gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb"),
        yaxis = list(automargin = TRUE, tickfont = list(size = 11))
      )
  })

  output$metrics_compare_plot <- renderPlotly({
    tx <- chart_transaction("diagnostic_tx")
    segment_key <- if (identical(tx, "Cho thuê")) "rent" else "sale"
    m <- metrics() %>% filter(segment == segment_key)
    validate(need(nrow(m) > 0, "Chưa có file chỉ số mô hình."))
    min_rmse <- min(m$rmse_vnd, na.rm = TRUE)
    min_mae <- min(m$mae_vnd, na.rm = TRUE)
    plot_df <- bind_rows(
      m %>% transmute(model, metric = "MAPE (%)", value = mape * 100),
      m %>% transmute(model, metric = "R² (%)", value = pmax(r2, 0) * 100),
      m %>% transmute(model, metric = "Chỉ số RMSE", value = rmse_vnd / min_rmse * 100),
      m %>% transmute(model, metric = "Chỉ số MAE", value = mae_vnd / min_mae * 100)
    ) %>%
      mutate(
        model_short = dplyr::recode(
          model,
          "Linear Regression" = "Tuyến tính",
          "Random Forest" = "Rừng NN",
          "XGBoost" = "XGB",
          "RF + XGBoost Ensemble" = "Tổ hợp",
          "Tuned RF/XGBoost Ensemble" = "Tối ưu",
          .default = model
        ),
        model_short = factor(model_short, levels = unique(model_short)),
        tooltip = paste0("Mô hình: ", model_label_vi(model), "<br>Chỉ số: ", metric, "<br>Giá trị: ", format_number_vi(value, 1))
      )

    plot_ly(
      plot_df,
      x = ~model_short,
      y = ~value,
      color = ~metric,
      type = "bar",
      text = ~tooltip,
      hovertemplate = "%{text}<extra></extra>",
      colors = chart_colors(n_distinct(plot_df$metric))
    ) %>%
      layout(
        barmode = "group",
        margin = list(l = 82, r = 28, t = 16, b = 120),
        font = list(family = "Inter, -apple-system, BlinkMacSystemFont, Segoe UI, Arial, sans-serif", color = "#1f2937"),
        hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#d7e6f5", font = list(color = "#1f2937")),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        legend = list(orientation = "h", x = 0, y = -0.30, font = list(size = 11), itemwidth = 30),
        xaxis = list(title = "", automargin = TRUE, tickangle = 0, tickfont = list(size = 11)),
        yaxis = list(title = list(text = "Giá trị / chỉ số", standoff = 18), automargin = TRUE, gridcolor = "#e5e7eb", zerolinecolor = "#e5e7eb")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$cluster_plot <- renderPlotly({
    validate(need(file.exists(CLUSTER_PATH), paste0("Chưa có ", PATHS$clusters_csv, ". Hãy chạy scripts/04_mo_hinh_hoa/huan_luyen_mo_hinh.R.")))
    tx <- chart_transaction("cluster_tx")
    m2_info <- price_m2_display_info(tx)
    cluster_df <- read_csv(CLUSTER_PATH, show_col_types = FALSE)
    if (!"transaction_type" %in% names(cluster_df)) {
      cluster_df$transaction_type <- "Bán"
    }
    cluster_df <- cluster_df %>%
      mutate(
        district_name = clean_display_label(district_name),
        category_name = clean_display_label(category_name)
      ) %>%
      filter(transaction_type == tx)
    validate(need(nrow(cluster_df) > 0, paste("Chưa có dữ liệu phân cụm cho giao dịch", tx)))

    plot_df <- known_rows_or_all(cluster_df, "district_name") %>%
      known_rows_or_all("category_name") %>%
      mutate(
        cluster = as.factor(cluster),
        display_price_m2 = median_price_per_m2 / m2_info$scale,
        tooltip = paste0(
          "Giao dịch: ", transaction_type,
          "<br>Khu vực cũ: ", district_name,
          "<br>Loại BĐS: ", category_name,
          "<br>Cụm: ", cluster,
          "<br>Diện tích trung vị: ", format_number_vi(median_area, 1), " m²",
          "<br>Giá/m² trung vị: ", format_number_vi(display_price_m2, m2_info$digits), " ", m2_info$unit,
          "<br>Số tin: ", format_count_vi(listing_count)
        )
      )
    color_values <- setNames(chart_colors(n_distinct(plot_df$cluster)), sort(unique(plot_df$cluster)))
    p <- plot_df %>%
      ggplot(aes(x = median_area, y = display_price_m2, color = cluster, size = listing_count, text = tooltip)) +
      geom_point(alpha = 0.78) +
      scale_color_manual(values = color_values) +
      labs(x = "Diện tích trung vị (m²)", y = paste0("Giá/m² trung vị (", m2_info$unit, ")"), color = "Cụm", size = "Số tin") +
      chart_theme()
    interactive_chart(p, tooltip = "text")
  })

  output$data_quality_cards <- renderUI({
    df <- data_filtered()
    quality <- build_data_quality_summary(df)
    exact_rate <- mean(df$coord_status == "Tọa độ gốc từ nguồn", na.rm = TRUE)
    future_rows <- quality %>% filter(nhom == "Ngày đăng tương lai") %>% pull(so_dong)
    missing_rows <- quality %>% filter(nhom == "Thiếu khu vực/loại BĐS") %>% pull(so_dong)
    duplicate_rows <- quality %>% filter(nhom == "Trùng lặp nghi ngờ") %>% pull(so_dong)
    div(
      class = "kpi-grid",
      kpi_card("Dòng sau lọc", format_count_vi(nrow(df)), "đang hiển thị trong bảng", "table", "default"),
      kpi_card("Tọa độ gốc", paste0(round(exact_rate * 100, 1), "%"), "phần còn lại là ước lượng", "map-location-dot", "success"),
      kpi_card("Ngày tương lai", format_count_vi(future_rows), "cần rà format/crawl", "calendar-days", ifelse(future_rows > 0, "danger", "success")),
      kpi_card("Trùng nghi ngờ", format_count_vi(duplicate_rows), paste0("thiếu nhãn: ", format_count_vi(missing_rows)), "copy", "warning")
    )
  })

  output$data_quality_table <- renderTable({
    build_data_quality_summary(data_filtered()) %>%
      transmute(
        `Nhóm kiểm tra` = nhom,
        `Số dòng` = format_count_vi(so_dong),
        `Mức độ` = muc_do,
        `Ghi chú` = ghi_chu
      )
  })

  output$data_quality_plot <- renderPlotly({
    df <- data_filtered() %>%
      mutate(coord_exact = coord_status == "Tọa độ gốc từ nguồn") %>%
      group_by(source) %>%
      summarise(
        listings = n(),
        exact_rate = mean(coord_exact, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(listings)) %>%
      mutate(
        source_label = source_label_vi(source),
        tooltip = paste0(
          "Nguồn: ", source_label,
          "<br>Số dòng: ", format_count_vi(listings),
          "<br>Tọa độ gốc: ", round(exact_rate * 100, 1), "%"
        )
      )
    validate(need(nrow(df) > 0, "Không có dữ liệu để vẽ độ phủ nguồn."))
    p <- df %>%
      ggplot(aes(x = reorder(source_label, listings), y = listings, fill = exact_rate, text = tooltip)) +
      geom_col(width = 0.72) +
      coord_flip() +
      scale_fill_gradient(low = "#f59e0b", high = "#10b981", labels = function(x) paste0(round(x * 100), "%")) +
      labs(x = NULL, y = "Số dòng", fill = "Tọa độ gốc") +
      chart_theme()
    interactive_chart(p, tooltip = "text")
  })

  output$data_table <- renderDT({
    data_filtered() %>%
      mutate(source_link = listing_url(ad_url, source)) %>%
      transmute(
        `Nguồn` = source_label_vi(source),
        `Giao dịch` = transaction_type,
        `Tiêu đề` = title,
        `Khu vực cũ` = district_name,
        `Phường/xã` = ward,
        `Loại BĐS` = category_name,
        `Giá` = format_vnd_full(price),
        `Diện tích` = paste0(round(area, 1), " m²"),
        `Giá/m²` = format_vnd_full(price_per_m2),
        `Link` = ifelse(
          !is.na(source_link),
          paste0('<a href="', htmltools::htmlEscape(source_link), '" target="_blank" rel="noopener noreferrer">Xem tin</a>'),
          ""
        )
      ) %>%
      datatable(
        rownames = FALSE,
        filter = "none",
        escape = 1:9,
        options = list(
          deferRender = TRUE,
          pageLength = 15,
          searchDelay = 350,
          scrollX = TRUE,
          language = list(search = "Tìm kiếm:", lengthMenu = "Hiển thị _MENU_ dòng")
        )
      )
  }, server = TRUE)
}
