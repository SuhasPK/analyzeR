# Module UI
readRUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("read_data_button"), "Read Data"),
    br(), br(),
    uiOutput(ns("read_content"))
  )
}

# Module Server
readRServer <- function(input, output, session, dataset) {
  ns <- session$ns

  show_data <- reactiveVal(FALSE)

  observeEvent(input$read_data_button, {
    showModal(modalDialog(
      title = "Read Data",
      "Do you want readR to read the data?",
      footer = tagList(
        modalButton("No"),
        actionButton(ns("go_to_readR"), "Yes")
      )
    ))
  })

  observeEvent(input$go_to_readR, {
    removeModal()
    show_data(TRUE)
  })

  # Render the content area: hint before first click, tables after
  output$read_content <- renderUI({
    if (!show_data()) {
      p(style = "color: #888; margin-top: 10px;",
        "Click 'Read Data' to generate a summary of the loaded dataset.")
    } else {
      tagList(
        h4("Dataset Summary"),
        DTOutput(ns("dataset_summary")),
        br(),
        h4("Statistics Summary"),
        DTOutput(ns("stats_summary")),
        br(),
        h4("Frequency Table for Categorical Data"),
        DTOutput(ns("frequency_table"))
      )
    }
  })

  output$dataset_summary <- renderDT({
    req(show_data(), dataset())
    summary_df <- data.frame(
      Variable = names(dataset()),
      Type     = sapply(dataset(), function(x) paste(class(x), collapse = "/"))
    )
    datatable(summary_df, options = list(
      pageLength = 10, lengthMenu = c(10, 20, 50),
      autoWidth = TRUE, scrollX = TRUE,
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    ), class = "display compact cell-border stripe hover")
  })

  output$stats_summary <- renderDT({
    req(show_data(), dataset())
    data <- dataset()
    summary_stats <- data.frame(
      Variable = names(data),
      Mean     = sapply(data, function(x) if (is.numeric(x)) round(mean(x, na.rm = TRUE), 3) else NA),
      Median   = sapply(data, function(x) if (is.numeric(x)) round(median(x, na.rm = TRUE), 3) else NA),
      SD       = sapply(data, function(x) if (is.numeric(x)) round(sd(x, na.rm = TRUE), 3) else NA),
      Range    = sapply(data, function(x) if (is.numeric(x)) paste(round(min(x, na.rm = TRUE), 3), "-", round(max(x, na.rm = TRUE), 3)) else NA),
      IQR      = sapply(data, function(x) if (is.numeric(x)) round(IQR(x, na.rm = TRUE), 3) else NA),
      Missing  = sapply(data, function(x) sum(is.na(x)))
    )
    datatable(summary_stats, options = list(
      pageLength = 10, lengthMenu = c(10, 20, 50),
      autoWidth = TRUE, scrollX = TRUE,
      columnDefs = list(list(className = "dt-left", targets = "_all")),
      dom = "t",
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}"
      )
    ), class = "display compact cell-border stripe hover")
  })

  output$frequency_table <- renderDT({
    req(show_data(), dataset())
    data <- dataset()
    cat_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]

    if (length(cat_cols) == 0) return(NULL)

    freq_df <- do.call(rbind, lapply(cat_cols, function(var) {
      freq <- table(data[[var]], useNA = "no")
      if (length(freq) == 0) return(NULL)
      data.frame(
        Variable  = var,
        Category  = names(freq),
        Frequency = as.integer(freq),
        Pct       = round(100 * as.integer(freq) / sum(freq), 1),
        stringsAsFactors = FALSE
      )
    }))

    if (is.null(freq_df) || nrow(freq_df) == 0) return(NULL)

    datatable(freq_df, options = list(
      pageLength = 10, lengthMenu = c(10, 20, 50),
      autoWidth = TRUE, scrollX = TRUE,
      columnDefs = list(list(className = "dt-left", targets = "_all")),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}"
      )
    ), class = "display compact cell-border stripe hover")
  })
}
