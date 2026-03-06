# Module UI
cleanRUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("tab_guard")),
    tabsetPanel(
      # ── Tab 1: Clean ────────────────────────────────────────────────────
      tabPanel("Clean",
        br(),
        actionButton(ns("remove_dups"), "Remove Duplicates", class = "btn-warning"),
        hr(),
        selectInput(ns("na_action"), "Handle Missing Values",
          choices = c("None", "Drop rows with NA", "Fill with Mean",
                      "Fill with Median", "Fill with Mode")),
        actionButton(ns("apply_na"), "Apply NA Strategy", class = "btn-primary"),
        hr(),
        uiOutput(ns("col_select_ui")),
        actionButton(ns("drop_cols"), "Drop Selected Columns", class = "btn-danger"),
        hr(),
        h5("Action Log"),
        verbatimTextOutput(ns("clean_log"))
      ),

      # ── Tab 2: Data Quality ─────────────────────────────────────────────
      tabPanel("Data Quality",
        br(),
        uiOutput(ns("dq_content"))
      ),

      # ── Tab 3: Preview ──────────────────────────────────────────────────
      tabPanel("Preview",
        br(),
        withSpinner(DTOutput(ns("clean_preview")))
      )
    )
  )
}

# Module Server
cleanRServer <- function(id, rv, reset_trigger, report_rv = NULL, monitor_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    log_entries <- reactiveVal(character(0))

    observeEvent(reset_trigger(), { log_entries(character(0)) })

    # Mirror log entries into report_rv whenever they change
    observeEvent(log_entries(), {
      if (!is.null(report_rv)) report_rv$clean_log <- log_entries()
    }, ignoreNULL = FALSE)

    add_log <- function(msg) {
      log_entries(c(log_entries(),
                    paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg)))
    }

    # ── No-data guard ────────────────────────────────────────────────────
    output$tab_guard <- renderUI({
      if (!is.null(rv$data)) return(NULL)
      .no_data_ui()
    })

    # Disable / enable action buttons based on data presence.
    # Note: apply_type_recs is rendered inside a renderUI and only appears when
    # data is loaded — shinyjs cannot target a non-existent DOM element, so it
    # is intentionally omitted here (its observeEvent already uses req(rv$data)).
    observe({
      has_data <- !is.null(rv$data)
      if (has_data) {
        shinyjs::enable("remove_dups")
        shinyjs::enable("apply_na")
        shinyjs::enable("drop_cols")
      } else {
        shinyjs::disable("remove_dups")
        shinyjs::disable("apply_na")
        shinyjs::disable("drop_cols")
      }
    })

    # ── Remove Duplicates ────────────────────────────────────────────────
    observeEvent(input$remove_dups, {
      req(rv$data)
      t0 <- proc.time()["elapsed"]
      withProgress(message = "Removing duplicates...", value = 0, {
        before  <- nrow(rv$data)
        rv$data <- rv$data[!duplicated(rv$data), ]
        removed <- before - nrow(rv$data)
        setProgress(1)
      })
      add_log(paste0("Removed duplicates: ", removed,
                     " row(s) removed. Now ", nrow(rv$data), " rows."))
      showNotification(paste0("Removed ", removed, " duplicate row(s)."),
                       type = "message", duration = 4)
      .monitr_log(monitor_rv, "Clean: Dedup", rows = nrow(rv$data),
                  secs = round(proc.time()["elapsed"] - t0, 2),
                  note = paste0(removed, " dupes removed"))
    })

    # ── NA Strategy ─────────────────────────────────────────────────────
    observeEvent(input$apply_na, {
      req(rv$data)
      action <- input$na_action
      if (action == "None") return()
      t0 <- proc.time()["elapsed"]

      withProgress(message = "Handling missing values...", value = 0,
                   detail = action, {
        data <- rv$data

        if (action == "Drop rows with NA") {
          before  <- nrow(data)
          data    <- tidyr::drop_na(data)
          removed <- before - nrow(data)
          add_log(paste0("Dropped rows with NA: ", removed, " row(s) removed."))
          showNotification(paste0("Dropped ", removed, " row(s) with NA."),
                           type = "message", duration = 4)

        } else if (action == "Fill with Mean") {
          data <- dplyr::mutate(data, dplyr::across(where(is.numeric),
            ~ tidyr::replace_na(., mean(., na.rm = TRUE))))
          add_log("Filled numeric NA values with column mean.")
          showNotification("Filled numeric NAs with mean.",
                           type = "message", duration = 4)

        } else if (action == "Fill with Median") {
          data <- dplyr::mutate(data, dplyr::across(where(is.numeric),
            ~ tidyr::replace_na(., median(., na.rm = TRUE))))
          add_log("Filled numeric NA values with column median.")
          showNotification("Filled numeric NAs with median.",
                           type = "message", duration = 4)

        } else if (action == "Fill with Mode") {
          mode_val <- function(x) {
            ux <- unique(x[!is.na(x)])
            if (length(ux) == 0) return(NA)
            ux[which.max(tabulate(match(x, ux)))]
          }
          for (nm in names(data)) {
            col <- data[[nm]]
            if (any(is.na(col))) {
              mv <- mode_val(col)
              if (!is.na(mv)) { col[is.na(col)] <- mv; data[[nm]] <- col }
            }
          }
          add_log("Filled NA values with column mode.")
          showNotification("Filled NAs with mode.", type = "message", duration = 4)
        }

        rv$data <- data
        setProgress(1)
      })
      .monitr_log(monitor_rv, "Clean: NA", rows = nrow(rv$data),
                  secs = round(proc.time()["elapsed"] - t0, 2),
                  note = action)
    })

    # ── Dynamic column selector ─────────────────────────────────────────
    output$col_select_ui <- renderUI({
      req(rv$data)
      checkboxGroupInput(ns("cols_to_drop"), "Select Columns to Drop",
        choices = names(rv$data), inline = TRUE)
    })

    # ── Drop selected columns ────────────────────────────────────────────
    observeEvent(input$drop_cols, {
      req(rv$data, input$cols_to_drop)
      t0   <- proc.time()["elapsed"]
      cols <- input$cols_to_drop
      withProgress(message = "Dropping columns...", value = 0,
                   detail = paste(cols, collapse = ", "), {
        rv$data <- rv$data[, !names(rv$data) %in% cols, drop = FALSE]
        setProgress(1)
      })
      add_log(paste0("Dropped column(s): ", paste(cols, collapse = ", ")))
      showNotification(paste0("Dropped: ", paste(cols, collapse = ", ")),
                       type = "warning", duration = 4)
      .monitr_log(monitor_rv, "Clean: Drop Cols", rows = nrow(rv$data),
                  secs = round(proc.time()["elapsed"] - t0, 2),
                  note = paste(cols, collapse = ", "))
    })

    # ── Apply Type Recommendations ───────────────────────────────────────
    observeEvent(input$apply_type_recs, {
      req(rv$data)
      data    <- rv$data
      changed <- character(0)

      for (col in names(data)) {
        x       <- data[[col]]
        n_valid <- sum(!is.na(x))
        if (n_valid == 0) next

        if (is.character(x)) {
          num_conv  <- suppressWarnings(as.numeric(x[!is.na(x)]))
          num_rate  <- mean(!is.na(num_conv))
          n_unique  <- length(unique(x[!is.na(x)]))
          sample_v  <- head(x[!is.na(x)], 30)
          date_rate <- mean(grepl(
            "^\\d{4}-\\d{2}-\\d{2}|^\\d{2}/\\d{2}/\\d{4}|^\\d{2}-[A-Za-z]{3}-\\d{4}",
            sample_v))

          if (num_rate >= 0.90) {
            data[[col]] <- suppressWarnings(as.numeric(x))
            changed     <- c(changed, paste0(col, " \u2192 numeric"))
          } else if (date_rate >= 0.70) {
            data[[col]] <- suppressWarnings(as.Date(x))
            changed     <- c(changed, paste0(col, " \u2192 Date"))
          } else if (n_unique <= 10 && n_valid >= 10) {
            data[[col]] <- as.factor(x)
            changed     <- c(changed, paste0(col, " \u2192 factor"))
          }

        } else if (is.numeric(x)) {
          vals     <- x[!is.na(x)]
          n_unique <- length(unique(vals))
          if (length(vals) > 0 && all(vals == floor(vals)) &&
              max(abs(vals)) < 2e9 && n_unique <= 2) {
            data[[col]] <- as.factor(x)
            changed     <- c(changed, paste0(col, " \u2192 factor"))
          }
        }
      }

      if (length(changed) == 0) {
        showNotification("No type conversions needed — all columns look correctly typed.",
                         type = "message", duration = 4)
        return()
      }
      t0      <- proc.time()["elapsed"]
      rv$data <- data
      add_log(paste0("Applied type recommendations: ", paste(changed, collapse = "; ")))
      showNotification(paste0("Converted: ", paste(changed, collapse = ", ")),
                       type = "message", duration = 5)
      .monitr_log(monitor_rv, "Clean: Type Recs", rows = nrow(rv$data),
                  secs = round(proc.time()["elapsed"] - t0, 2),
                  note = paste0(length(changed), " col(s) converted"))
    })

    # ── Action log ──────────────────────────────────────────────────────
    output$clean_log <- renderPrint({
      entries <- log_entries()
      if (length(entries) == 0) cat("No actions taken yet.")
      else cat(paste(entries, collapse = "\n"))
    })

    # ── Preview ─────────────────────────────────────────────────────────
    output$clean_preview <- renderDT({
      req(rv$data)
      datatable(rv$data, options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50),
        scrollX    = TRUE
      ), class = "display compact cell-border stripe hover")
    })

    # ── Data Quality tab ─────────────────────────────────────────────────
    output$dq_content <- renderUI({
      if (is.null(rv$data)) {
        return(p(style = "color: #888; margin-top: 6px;",
                 "Load a dataset first to see data quality analysis."))
      }
      tagList(
        h4("Null / Missing Analysis"),
        p(style = "color: #888; font-size: 13px; margin: -6px 0 10px;",
          "Per-column missing counts, percentages, and a visual completeness bar."),
        div(class = "dt-wrap", withSpinner(DTOutput(ns("null_table")))),
        hr(),
        h4("Data Type Recommendations"),
        p(style = "color: #888; font-size: 13px; margin: -6px 0 10px;",
          "Columns where the detected type may differ from the intended type."),
        div(class = "dt-wrap", withSpinner(DTOutput(ns("type_recs")))),
        br(),
        actionButton(ns("apply_type_recs"), "Apply Recommendations",
                     class = "btn-warning",
                     title = "Converts each flagged column to its recommended type")
      )
    })

    # ── Null analysis table ──────────────────────────────────────────────
    output$null_table <- renderDT({
      req(rv$data)
      data <- rv$data

      miss_n   <- sapply(data, function(x) sum(is.na(x)))
      miss_pct <- round(100 * miss_n / nrow(data), 1)
      complete_pct <- 100 - miss_pct

      bar_color <- ifelse(miss_pct > 50, "#cc3300",
                          ifelse(miss_pct > 10, "#cc8800", "#00aa44"))

      bar_html <- paste0(
        '<div style="background:#1a1a1a;border-radius:3px;width:140px;height:10px;',
        'display:inline-block;vertical-align:middle;">',
        '<div style="background:', bar_color,
        ';width:', pmin(complete_pct, 100),
        '%;height:10px;border-radius:3px;"></div></div>',
        ' <span style="font-size:11px;color:#888;">', complete_pct, '%</span>'
      )

      null_df <- data.frame(
        Column       = names(data),
        Type         = sapply(data, function(x) paste(class(x), collapse = "/")),
        Total        = nrow(data),
        Missing      = miss_n,
        `Missing %`  = miss_pct,
        Completeness = bar_html,
        check.names  = FALSE,
        stringsAsFactors = FALSE
      )
      null_df <- null_df[order(-null_df$Missing), ]

      datatable(null_df,
        escape   = FALSE,
        options  = list(
          pageLength = 20, lengthMenu = c(10, 20, 50),
          autoWidth = TRUE, scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-left",   targets = c(0, 1)),
            list(className = "dt-right",  targets = c(2, 3, 4)),
            list(className = "dt-center", targets = 5),
            list(orderable = FALSE,       targets = 5)
          )
        ),
        class    = "display compact cell-border stripe hover",
        rownames = FALSE
      )
    })

    # ── Type mismatch recommendations ────────────────────────────────────
    output$type_recs <- renderDT({
      req(rv$data)
      data <- rv$data

      results <- lapply(names(data), function(col) {
        x       <- data[[col]]
        n_valid <- sum(!is.na(x))
        if (n_valid == 0) return(NULL)

        issue <- NA_character_
        rec   <- NA_character_
        sev   <- NA_character_

        if (is.character(x)) {
          num_conv <- suppressWarnings(as.numeric(x[!is.na(x)]))
          num_rate <- mean(!is.na(num_conv))
          n_unique <- length(unique(x[!is.na(x)]))
          sample_vals <- head(x[!is.na(x)], 30)
          date_rate   <- mean(grepl(
            "^\\d{4}-\\d{2}-\\d{2}|^\\d{2}/\\d{2}/\\d{4}|^\\d{2}-[A-Za-z]{3}-\\d{4}",
            sample_vals))

          if (num_rate >= 0.90) {
            issue <- paste0(round(num_rate * 100), "% of values parse as numeric")
            rec   <- "Convert to numeric: as.numeric(df[[col]])"
            sev   <- "warn"
          } else if (date_rate >= 0.70) {
            issue <- "Values match common date patterns"
            rec   <- "Convert to Date: as.Date(df[[col]])"
            sev   <- "info"
          } else if (n_unique <= 10 && n_valid >= 10) {
            issue <- paste0("Only ", n_unique, " unique value(s) in a character column")
            rec   <- "Convert to factor: as.factor(df[[col]])"
            sev   <- "info"
          } else {
            return(NULL)
          }

        } else if (is.numeric(x)) {
          vals <- x[!is.na(x)]
          if (length(vals) > 0 && all(vals == floor(vals)) && max(abs(vals)) < 2e9) {
            n_unique <- length(unique(vals))
            if (n_unique <= 2) {
              issue <- "Numeric column with only 2 unique values"
              rec   <- "Consider: as.logical() or as.factor()"
              sev   <- "info"
            }
          }
          if (is.na(issue)) return(NULL)

        } else {
          return(NULL)
        }

        badge_class <- switch(sev,
          "warn" = "rec-badge rec-warn",
          "info" = "rec-badge rec-info",
          "rec-badge rec-ok"
        )

        data.frame(
          Column         = col,
          `Current Type` = paste(class(x), collapse = "/"),
          Issue          = issue,
          Recommendation = paste0('<span class="', badge_class, '">', rec, '</span>'),
          check.names    = FALSE,
          stringsAsFactors = FALSE
        )
      })

      rec_df <- do.call(rbind, Filter(Negate(is.null), results))

      if (is.null(rec_df) || nrow(rec_df) == 0) {
        return(datatable(
          data.frame(Message = "No type mismatches detected. All columns appear correctly typed."),
          options  = list(dom = "t"),
          class    = "display compact",
          rownames = FALSE
        ))
      }

      datatable(rec_df,
        escape   = FALSE,
        options  = list(
          pageLength = 20,
          autoWidth  = TRUE, scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-left", targets = "_all"),
            list(width = "220px", targets = 3)
          )
        ),
        class    = "display compact cell-border stripe hover",
        rownames = FALSE
      )
    })
  })
}
