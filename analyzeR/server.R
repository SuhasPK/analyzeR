server <- function(input, output, session) {
  rv            <- reactiveValues(data = NULL)
  reset_counter <- reactiveVal(0L)
  reset_trigger <- reactive(reset_counter())
  monitor_rv    <- reactiveValues(log = list())

  # ── Shared report state — written to by each module ──────────────────────────
  # reportRServer reads from here to build the downloadable report.
  report_rv <- reactiveValues(
    file_name       = NULL,   # loadR: original file name
    file_ext        = NULL,   # loadR: extension
    clean_log       = NULL,   # cleanR: character vector of action log entries
    hyp_results     = list(), # analyzeR: list of all hypothesis test results (accumulates)
    plot_list       = list(), # plotR: named list of {obj, type} per generated plot
    text_col        = NULL,   # textR: column that was analysed
    text_freq       = NULL,   # textR: word frequency data.frame
    text_sentiment  = NULL,   # textR: list(n_pos, n_neg, net, df)
    learn_bench     = NULL,   # learnR: benchmark data.frame
    learn_target    = NULL,   # learnR: target column name
    learn_pt        = NULL,   # learnR: "Regression" or "Classification"
    learn_best      = NULL,   # learnR: best model name
    learn_metrics   = NULL,   # learnR: full metrics list (per algo)
    learn_preds     = NULL,   # learnR: predictions per model (list)
    learn_actual    = NULL,   # learnR: actual test-set y values
    learn_feat_imp  = NULL,   # learnR: feature importance data.frame (RF)
    learn_feat_cols = NULL,   # learnR: feature column names
    learn_rows_orig = NULL,   # learnR: dataset rows before sampling
    learn_rows_used = NULL    # learnR: rows actually used for training
  )

  # Clear all report_rv fields when dataset is reset
  observeEvent(reset_trigger(), {
    report_rv$file_name       <- NULL
    report_rv$file_ext        <- NULL
    report_rv$clean_log       <- NULL
    report_rv$hyp_results     <- list()
    report_rv$plot_list       <- list()
    report_rv$text_col        <- NULL
    report_rv$text_freq       <- NULL
    report_rv$text_sentiment  <- NULL
    report_rv$learn_bench     <- NULL
    report_rv$learn_target    <- NULL
    report_rv$learn_pt        <- NULL
    report_rv$learn_best      <- NULL
    report_rv$learn_metrics   <- NULL
    report_rv$learn_preds     <- NULL
    report_rv$learn_actual    <- NULL
    report_rv$learn_feat_imp  <- NULL
    report_rv$learn_feat_cols <- NULL
    report_rv$learn_rows_orig <- NULL
    report_rv$learn_rows_used <- NULL
  }, ignoreInit = TRUE)

  raw_dataset <- loadRServer("load", reset_trigger, report_rv, monitor_rv)

  observe({
    tryCatch(
      { rv$data <- raw_dataset() },
      error = function(e) {
        if (!inherits(e, "shiny.silent.error"))
          showNotification(paste("Failed to load dataset:", conditionMessage(e)),
                           type = "error", duration = 8)
      }
    )
  })

  readRServer   ("read",    reactive(rv$data), reset_trigger)
  cleanRServer  ("clean",   rv,                reset_trigger, report_rv, monitor_rv)
  analyzeRServer("analyze", reactive(rv$data), reset_trigger, report_rv)
  plotRServer   ("plot",    reactive(rv$data), reset_trigger, report_rv)
  textRServer   ("text",    reactive(rv$data), reset_trigger, report_rv)
  learnRServer  ("learn",   reactive(rv$data), reset_trigger, report_rv, monitor_rv)
  reportRServer ("report",  reactive(rv$data), report_rv, reset_trigger)
  removeRServer ("remove",  rv, reset_counter)

  # ── MonitR outputs ────────────────────────────────────────────────────────

  # Quick stats bar — polls every 3 s for memory; does NOT re-render the panel
  output$monitr_stats <- renderUI({
    invalidateLater(3000)
    mem_mb  <- .monitr_mem_mb()
    ds_mb   <- if (!is.null(rv$data))
      round(as.numeric(object.size(rv$data)) / 1024^2, 1) else NULL
    log     <- monitor_rv$log
    last_op <- if (length(log) > 0) {
      e <- log[[length(log)]]
      paste0(e$op, if (!is.null(e$secs)) paste0(" \u2014 ", e$secs, "s") else "")
    } else "\u2014"
    tagList(
      span(class = "monitr-stat", paste0("R Heap: ", mem_mb, " MB")),
      span(class = "monitr-sep",  "\u00b7"),
      span(class = "monitr-stat",
           paste0("Dataset: ", if (!is.null(ds_mb)) paste0(ds_mb, " MB") else "\u2014")),
      span(class = "monitr-sep",  "\u00b7"),
      span(class = "monitr-stat", paste0("Last: ", last_op))
    )
  })

  # Operation log table — updates only when monitor_rv$log changes
  output$monitr_log_ui <- renderUI({
    log <- monitor_rv$log
    if (length(log) == 0)
      return(p(style = "color:#444; text-align:center; padding:6px 0; margin:0; font-size:13px;",
               "No operations logged yet."))
    rows <- lapply(rev(log), function(e) {
      tags$tr(
        tags$td(e$op),
        tags$td(if (!is.null(e$rows)) format(e$rows, big.mark = ",") else "\u2014"),
        tags$td(if (!is.null(e$secs)) paste0(e$secs, "s") else "\u2014"),
        tags$td(if (!is.null(e$note) && nchar(e$note) > 0) e$note else "\u2014"),
        tags$td(format(e$ts, "%H:%M:%S"))
      )
    })
    tags$table(class = "monitr-table",
      tags$thead(tags$tr(
        tags$th("Operation"), tags$th("Rows"), tags$th("Time"),
        tags$th("Note"),      tags$th("At")
      )),
      tags$tbody(do.call(tagList, rows))
    )
  })

  # ── Global dataset status badge (rendered in app header) ─────────────────
  output$dataset_status <- renderUI({
    if (is.null(rv$data)) {
      span(class = "ds-status ds-status-empty", "no dataset loaded")
    } else {
      span(
        class = "ds-status ds-status-loaded",
        paste0(
          format(nrow(rv$data), big.mark = ","),
          " rows \u00d7 ", ncol(rv$data), " cols"
        )
      )
    }
  })
}
