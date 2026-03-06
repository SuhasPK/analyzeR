# Module UI
readRUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    uiOutput(ns("tab_guard")),
    actionButton(ns("read_data_button"), "Generate Report", class = "btn-primary"),
    br(), br(),
    uiOutput(ns("read_content"))
  )
}

# Module Server
readRServer <- function(id, dataset, reset_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    show_data <- reactiveVal(FALSE)

    # ── No-data guard ──────────────────────────────────────────────────────
    output$tab_guard <- renderUI({
      if (!is.null(dataset())) return(NULL)
      .no_data_ui()
    })

    # Disable / enable button
    observe({
      if (is.null(dataset())) shinyjs::disable("read_data_button")
      else                    shinyjs::enable("read_data_button")
    })

    observeEvent(reset_trigger(), { show_data(FALSE) })

    # ── Single-pass computation — runs once on button click ────────────────
    # Everything is computed here so the five DT formatters below are thin.
    report <- eventReactive(input$read_data_button, {
      req(dataset())
      data <- dataset()
      n    <- nrow(data)
      p    <- ncol(data)

      is_num <- vapply(data, is.numeric,                              logical(1))
      is_cat <- vapply(data, function(x) is.character(x) || is.factor(x), logical(1))
      miss_n <- vapply(data, function(x) sum(is.na(x)),              integer(1))

      # Overview scalars
      total_miss <- sum(miss_n)
      n_dups     <- sum(duplicated(data))
      mem        <- format(object.size(data), units = "auto")
      miss_rate  <- if (n * p > 0) round(100 * total_miss / (n * p), 2) else 0

      # Variable dictionary — one pass
      var_dict <- data.frame(
        Variable      = names(data),
        `R Type`      = vapply(data, function(x) paste(class(x), collapse = "/"), character(1)),
        `Sem. Type`   = vapply(data, infer_semantic_type, character(1)),
        `Non-Missing` = n - miss_n,
        Missing       = miss_n,
        `Missing %`   = round(100 * miss_n / n, 1),
        Unique        = vapply(data, function(x) length(unique(x[!is.na(x)])), integer(1)),
        check.names   = FALSE,
        stringsAsFactors = FALSE,
        row.names     = NULL
      )

      # Numeric stats — one loop, simplified columns (Kurtosis / IQR / Zeros removed)
      num_cols  <- names(data)[is_num]
      num_stats <- if (length(num_cols) > 0) {
        do.call(rbind, lapply(num_cols, function(v) {
          x <- data[[v]]
          data.frame(
            Variable = v,
            Min      = round(min(x,                    na.rm = TRUE), 4),
            Q1       = round(quantile(x, .25,          na.rm = TRUE), 4),
            Median   = round(median(x,                 na.rm = TRUE), 4),
            Mean     = round(mean(x,                   na.rm = TRUE), 4),
            Q3       = round(quantile(x, .75,          na.rm = TRUE), 4),
            Max      = round(max(x,                    na.rm = TRUE), 4),
            SD       = round(sd(x,                     na.rm = TRUE), 4),
            Skewness = round(col_skewness(x),                         3),
            check.names = FALSE, stringsAsFactors = FALSE
          )
        }))
      } else NULL

      # Categorical stats — one loop
      cat_cols  <- names(data)[is_cat]
      cat_stats <- if (length(cat_cols) > 0) {
        do.call(rbind, lapply(cat_cols, function(v) {
          vals <- data[[v]][!is.na(data[[v]])]
          if (length(vals) == 0) {
            data.frame(Variable = v, N = 0L, Missing = miss_n[[v]], `Miss %` = 100,
                       Unique = 0L, `Top Value` = NA_character_,
                       `Top Freq` = NA_integer_, `Top %` = NA_real_,
                       check.names = FALSE, stringsAsFactors = FALSE)
          } else {
            tbl      <- sort(table(vals), decreasing = TRUE)
            top_freq <- as.integer(tbl[1L])
            data.frame(
              Variable    = v,
              N           = length(vals),
              Missing     = miss_n[[v]],
              `Miss %`    = round(100 * miss_n[[v]] / n, 1),
              Unique      = length(tbl),
              `Top Value` = names(tbl)[1L],
              `Top Freq`  = top_freq,
              `Top %`     = round(100 * top_freq / length(vals), 1),
              check.names = FALSE, stringsAsFactors = FALSE
            )
          }
        }))
      } else NULL

      # Missing / null analysis
      bar_pct <- pmin(round(100 * miss_n / n, 1), 100)
      bar_clr <- ifelse(bar_pct > 50, "#cc3300",
                        ifelse(bar_pct > 10, "#cc8800", "#00cc44"))
      bar_html <- ifelse(
        unname(miss_n) == 0L,
        "<span style='color:#2a2a2a;'>\u2014</span>",
        paste0(
          '<div style="background:#1a1a1a;border-radius:3px;width:120px;',
          'height:10px;display:inline-block;">',
          '<div style="background:', bar_clr,
          ';width:', bar_pct, '%;height:10px;border-radius:3px;"></div></div>'
        )
      )
      null_df <- data.frame(
        Variable    = names(data),
        `R Type`    = vapply(data, function(x) paste(class(x), collapse = "/"), character(1)),
        Missing     = unname(miss_n),
        `Missing %` = unname(bar_pct),
        Visual      = bar_html,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      null_df <- null_df[order(-null_df$Missing), ]

      list(
        n          = n,     p        = p,
        n_num      = sum(is_num),
        n_cat      = sum(is_cat),
        total_miss = total_miss,
        miss_rate  = miss_rate,
        n_dups     = n_dups,
        mem        = mem,
        var_dict   = var_dict,
        num_stats  = num_stats,
        cat_stats  = cat_stats,
        null_df    = null_df
      )
    })

    observeEvent(input$read_data_button, {
      req(dataset())
      show_data(TRUE)
    })

    # ── UI shell — tabbed layout ──────────────────────────────────────────
    # Rule: every DTOutput must be at exactly ONE renderUI depth.
    output$read_content <- renderUI({
      if (!show_data()) {
        if (is.null(dataset())) return(NULL)   # tab_guard already shown above
        return(p(style = "color: #888; margin-top: 4px;",
                 "Click \u2018Generate Report\u2019 to produce a full summary of the loaded dataset."))
      }
      req(report())
      r <- report()

      # Helper: overview card div
      mc <- function(label, value, warn = FALSE) {
        div(class = "meta-card",
          tags$span(class = "meta-label", label),
          tags$span(class = if (warn) "meta-value meta-warn" else "meta-value", value)
        )
      }

      tabs <- list(
        tabPanel("Overview",
          br(),
          div(class = "dataset-meta-panel",
            mc("Observations",     format(r$n, big.mark = ",")),
            mc("Variables",        r$p),
            mc("Numeric Vars",     r$n_num),
            mc("Categorical Vars", r$n_cat),
            mc("Missing Cells",
               paste0(format(r$total_miss, big.mark = ","), " (", r$miss_rate, "%)"),
               warn = r$total_miss > 0),
            mc("Duplicate Rows",   format(r$n_dups, big.mark = ","), warn = r$n_dups > 0),
            mc("Memory",           r$mem)
          )
        ),
        tabPanel("Variables",
          br(),
          p(style = "color: #888; font-size: 13px; margin-bottom: 10px;",
            "R type, inferred semantic type, and completeness for every column."),
          div(class = "dt-wrap", withSpinner(DTOutput(ns("var_dict"))))
        ),
        if (r$n_num > 0) tabPanel("Numeric Stats",
          br(),
          p(style = "color: #888; font-size: 13px; margin-bottom: 10px;",
            "Q1/Q3 = 25th/75th percentile. Skewness: |>1| = highly skewed."),
          withSpinner(DTOutput(ns("num_stats")))
        ),
        if (r$n_cat > 0) tabPanel("Categorical",
          br(),
          p(style = "color: #888; font-size: 13px; margin-bottom: 10px;",
            "Top Value = most frequent category. Top % is share of non-missing rows."),
          withSpinner(DTOutput(ns("cat_stats")))
        ),
        tabPanel("Missing",
          br(),
          p(style = "color: #888; font-size: 13px; margin-bottom: 10px;",
            "Columns sorted by missing count. Bar width is proportional to missing %."),
          div(class = "dt-wrap", withSpinner(DTOutput(ns("null_analysis"))))
        )
      )

      do.call(tabsetPanel, Filter(Negate(is.null), tabs))
    })

    # ── Thin DT formatters — no computation here ───────────────────────────
    output$var_dict <- renderDT({
      req(report())
      datatable(report()$var_dict,
        options = list(
          pageLength = 20, lengthMenu = c(10, 20, 50),
          autoWidth = TRUE, scrollX = TRUE,
          columnDefs = list(list(className = "dt-left", targets = "_all"))
        ),
        class    = "display compact cell-border stripe hover",
        rownames = FALSE
      )
    })

    output$num_stats <- renderDT({
      req(report(), !is.null(report()$num_stats))
      datatable(report()$num_stats,
        options = list(
          pageLength = 15, lengthMenu = c(10, 15, 25, 50),
          autoWidth = TRUE, scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-left",  targets = 0),
            list(className = "dt-right", targets = c(1, 2, 3, 4, 5, 6, 7, 8))
          )
        ),
        class    = "display compact cell-border stripe hover",
        rownames = FALSE
      )
    })

    output$cat_stats <- renderDT({
      req(report(), !is.null(report()$cat_stats))
      datatable(report()$cat_stats,
        options = list(
          pageLength = 15, lengthMenu = c(10, 15, 25),
          autoWidth = TRUE, scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-left",  targets = c(0, 5)),
            list(className = "dt-right", targets = c(1, 2, 3, 4, 6, 7))
          )
        ),
        class    = "display compact cell-border stripe hover",
        rownames = FALSE
      )
    })

    output$null_analysis <- renderDT({
      req(report())
      datatable(report()$null_df,
        escape  = FALSE,
        options = list(
          pageLength = 20, lengthMenu = c(10, 20, 50),
          autoWidth = TRUE, scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-left",   targets = c(0, 1)),
            list(className = "dt-right",  targets = c(2, 3)),
            list(className = "dt-center", targets = 4),
            list(orderable = FALSE,       targets = 4)
          )
        ),
        class    = "display compact cell-border stripe hover",
        rownames = FALSE
      )
    })
  })
}
