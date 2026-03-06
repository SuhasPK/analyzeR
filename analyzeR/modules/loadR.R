# Module UI
loadRUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("file_input_ui")),
    uiOutput(ns("file_info")),
    uiOutput(ns("data_display"))
  )
}

# Module Server
# reset_trigger: a reactive() that increments when the dataset is cleared
loadRServer <- function(id, reset_trigger, report_rv = NULL, monitor_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    load_t0 <- reactiveVal(NULL)

    # Write file metadata to report_rv when a file is selected; start timing
    observeEvent(input$file, {
      load_t0(proc.time()["elapsed"])
      if (is.null(input$file) || is.null(report_rv)) return()
      report_rv$file_name <- input$file$name
      report_rv$file_ext  <- tolower(tools::file_ext(input$file$name))
    })

    # Log to MonitR once the dataset reactive resolves (observer is safe to write rv)
    observeEvent(dataset(), {
      req(dataset())
      t0 <- load_t0()
      if (!is.null(t0)) {
        .monitr_log(monitor_rv, "Load",
                    rows = nrow(dataset()),
                    secs = round(proc.time()["elapsed"] - t0, 2),
                    note = paste0(".", tolower(tools::file_ext(input$file$name))))
        load_t0(NULL)
      }
    }, ignoreNULL = TRUE)

    # Re-render the fileInput on reset so it clears the selected file
    output$file_input_ui <- renderUI({
      reset_trigger()
      fileInput(ns("file"), "Upload Dataset",
                accept = c(".csv", ".tsv", ".txt",
                           ".xlsx",
                           ".json", ".ndjson",
                           ".rds",
                           ".parquet"))
    })

    # ── Parse the uploaded file ──────────────────────────────────────────────
    dataset <- reactive({
      req(input$file)
      ext  <- tolower(tools::file_ext(input$file$name))
      path <- input$file$datapath

      tryCatch({

        if (ext == "csv") {
          sep <- detect_sep(path)
          read.csv(path, sep = sep, stringsAsFactors = FALSE)

        } else if (ext == "tsv") {
          read.delim(path, stringsAsFactors = FALSE)

        } else if (ext == "txt") {
          sep <- detect_sep(path)
          read.table(path, header = TRUE, sep = sep,
                     stringsAsFactors = FALSE, fill = TRUE, quote = '"')

        } else if (ext == "xlsx") {
          as.data.frame(readxl::read_excel(path))

        } else if (ext == "json") {
          obj <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
          if (is.data.frame(obj)) {
            obj
          } else if (is.list(obj) && length(obj) > 0) {
            tryCatch(
              as.data.frame(obj),
              error = function(e) as.data.frame(do.call(rbind, lapply(obj, as.data.frame)))
            )
          } else {
            showNotification("JSON structure could not be coerced to a data frame.",
                             type = "warning", duration = 6)
            NULL
          }

        } else if (ext == "ndjson") {
          con <- file(path, "r")
          on.exit(try(close(con), silent = TRUE))
          obj <- jsonlite::stream_in(con, verbose = FALSE)
          as.data.frame(obj)

        } else if (ext == "rds") {
          obj <- readRDS(path)
          if (is.data.frame(obj)) {
            obj
          } else {
            showNotification("RDS object is not a data frame — attempting coercion.",
                             type = "warning", duration = 5)
            tryCatch(
              as.data.frame(obj),
              error = function(e) {
                showNotification("Could not coerce RDS object to a data frame.",
                                 type = "error", duration = 6)
                NULL
              }
            )
          }

        } else if (ext == "parquet") {
          if (!.arrow_available) {
            showNotification(
              "Install the 'arrow' package to read Parquet files: install.packages('arrow')",
              type = "error", duration = 8)
            return(NULL)
          }
          as.data.frame(arrow::read_parquet(path))

        } else {
          showNotification(paste0("Unsupported file type: .", ext),
                           type = "error", duration = 5)
          NULL
        }

      }, error = function(e) {
        showNotification(paste("Failed to read file:", conditionMessage(e)),
                         type = "error", duration = 6)
        NULL
      })
    })

    # ── File info bar (shown immediately on selection) ───────────────────────
    output$file_info <- renderUI({
      req(input$file)
      file      <- input$file
      file_type <- file$type
      if (str_detect(file$name, "\\.tsv$") && nchar(file_type) == 0)
        file_type <- "text/tab-separated-values"
      if (nchar(file_type) == 0) file_type <- "unknown"

      div(class = "file-info-bar",
        div(class = "file-info-badge", "name", tags$span(file$name)),
        div(class = "file-info-badge", "size",
            tags$span(paste(format(file$size, big.mark = ","), "bytes"))),
        div(class = "file-info-badge", "type", tags$span(file_type))
      )
    })

    # ── Metadata cards + preview (shown after dataset is parsed) ────────────
    output$data_display <- renderUI({
      req(dataset())
      data <- dataset()

      n_num  <- sum(sapply(data, is.numeric))
      n_chr  <- sum(sapply(data, is.character))
      n_fac  <- sum(sapply(data, is.factor))
      n_lgl  <- sum(sapply(data, is.logical))
      n_miss <- sum(is.na(data))
      n_dups <- sum(duplicated(data))
      mem    <- format(object.size(data), units = "auto")
      total  <- as.numeric(nrow(data)) * ncol(data)
      miss_pct <- if (total > 0) round(100 * n_miss / total, 1) else 0

      cards <- list(
        div(class = "meta-card",
            tags$span(class = "meta-label", "Rows"),
            tags$span(class = "meta-value", format(nrow(data), big.mark = ","))),
        div(class = "meta-card",
            tags$span(class = "meta-label", "Columns"),
            tags$span(class = "meta-value", ncol(data))),
        div(class = "meta-card",
            tags$span(class = "meta-label", "Numeric"),
            tags$span(class = "meta-value", n_num)),
        div(class = "meta-card",
            tags$span(class = "meta-label", "Character"),
            tags$span(class = "meta-value", n_chr)),
        if (n_fac > 0) div(class = "meta-card",
            tags$span(class = "meta-label", "Factor"),
            tags$span(class = "meta-value", n_fac)),
        if (n_lgl > 0) div(class = "meta-card",
            tags$span(class = "meta-label", "Logical"),
            tags$span(class = "meta-value", n_lgl)),
        div(class = "meta-card",
            tags$span(class = "meta-label", "Missing"),
            tags$span(
              class = if (n_miss > 0) "meta-value meta-warn" else "meta-value",
              paste0(format(n_miss, big.mark = ","), " (", miss_pct, "%)")
            )),
        div(class = "meta-card",
            tags$span(class = "meta-label", "Duplicates"),
            tags$span(class = "meta-value", format(n_dups, big.mark = ","))),
        div(class = "meta-card",
            tags$span(class = "meta-label", "Memory"),
            tags$span(class = "meta-value", mem))
      )

      tagList(
        tags$p(
          style = "font-size: 14px; color: #888; font-weight: bold; margin: 16px 0 8px;",
          input$file$name
        ),
        div(class = "dataset-meta-panel", cards),
        tags$p("Preview \u2014 first 10 rows",
               style = "color: #666; font-size: 12px; margin: 16px 0 6px;"),
        div(withSpinner(DTOutput(ns("preview"))), style = "width: 100%; overflow-x: auto;")
      )
    })

    output$preview <- renderDT({
      req(dataset())
      datatable(
        head(dataset(), 10),
        options = list(
          pageLength = 10,
          dom        = "t",
          autoWidth  = TRUE,
          scrollX    = TRUE,
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        class = "display compact cell-border stripe hover"
      )
    })

    return(dataset)
  })
}
