# ─────────────────────────────────────────────────────────────────────────────
# reportR.R  —  Session Report module
#
# Two-step: Build (stores HTML in reactive, shows progress dialog) →
#           Download (writes pre-built HTML to file instantly).
# ─────────────────────────────────────────────────────────────────────────────

# ── Module UI ─────────────────────────────────────────────────────────────────
reportRUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    uiOutput(ns("tab_guard")),
    fluidRow(
      column(7,
        h4("Session Report"),
        p(style = "color:#888; font-size:13px; line-height:1.6;",
          "Select sections, click ", tags$strong(style="color:#ccc;","Build Report"),
          " to generate (progress shown while building), then download. ",
          "Open the HTML in a browser and use ",
          tags$strong(style = "color:#ccc;", "Ctrl+P \u2192 Save as PDF"),
          " to export a PDF."
        ),
        br(),
        h5("Select Sections to Include"),
        p(style = "color:#666; font-size:12px; margin:-4px 0 10px;",
          "Sections without data are skipped automatically even if checked."),
        checkboxGroupInput(ns("sections_include"), NULL,
          choices = list(
            "Dataset Overview"       = "overview",
            "Variable Summary"       = "var_summary",
            "Numeric Statistics"     = "num_stats",
            "Missing Value Analysis" = "missing",
            "Cleaning Log"           = "clean_log",
            "Normality Tests"        = "normality",
            "Outlier Detection"      = "outliers",
            "Hypothesis Test"        = "hyp_test",
            "Plots"                  = "plots",
            "Text Analysis"          = "text",
            "ML Benchmark"           = "ml_bench",
            "ML Model Details"       = "ml_details",
            "ML Feature Importance"  = "ml_feat_imp"
          ),
          selected = c("overview", "var_summary", "num_stats", "missing",
                       "clean_log", "normality", "outliers", "hyp_test",
                       "plots", "text", "ml_bench", "ml_details", "ml_feat_imp")
        ),
        uiOutput(ns("plot_selector_ui")),
        uiOutput(ns("hyp_selector_ui")),
        uiOutput(ns("text_selector_ui")),
        br(),
        # Step 1 — Build
        actionButton(ns("build_btn"), "\u25B6  Build Report",
                     class = "btn-success", style = "width:100%; margin-bottom:8px;"),
        # Step 2 — Download (appears after build)
        uiOutput(ns("download_ui")),
        br(),
        hr(),
        h5("Data Availability"),
        p(style = "color:#666; font-size:12px; margin:-4px 0 10px;",
          "Sections marked \u2713 have data ready. Others will be skipped."),
        uiOutput(ns("sections_status"))
      )
    )
  )
}


# ── Module Server ──────────────────────────────────────────────────────────────
reportRServer <- function(id, dataset, report_rv, reset_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Stores the pre-built HTML string; NULL = not yet built
    report_html <- reactiveVal(NULL)

    # ── No-data guard ──────────────────────────────────────────────────
    output$tab_guard <- renderUI({
      if (!is.null(dataset())) return(NULL)
      .no_data_ui()
    })

    # ── Reset on data clear ────────────────────────────────────────────
    observeEvent(reset_trigger(), {
      report_html(NULL)
    })

    # ── Nested plot selector ───────────────────────────────────────────
    output$plot_selector_ui <- renderUI({
      plots <- report_rv$plot_list
      if (length(plots) == 0) return(NULL)
      if (!("plots" %in% input$sections_include)) return(NULL)
      div(
        style = paste0(
          "margin-left:24px; margin-top:-4px; margin-bottom:12px; ",
          "padding:10px 14px; border-left:2px solid #2a2a2a; ",
          "background:#0a0a0a; border-radius:0 4px 4px 0;"
        ),
        p(style = "color:#888; font-size:12px; margin:0 0 6px;",
          paste0(length(plots), " plot(s) generated \u2014 choose which to include:")),
        checkboxGroupInput(ns("selected_plots"), NULL,
          choices  = names(plots),
          selected = names(plots)
        )
      )
    })

    # ── Nested text analysis selector ─────────────────────────────────
    output$text_selector_ui <- renderUI({
      if (!("text" %in% input$sections_include)) return(NULL)
      if (is.null(report_rv$text_freq) || nrow(report_rv$text_freq) == 0) return(NULL)

      all_choices <- list(
        "Word Frequency Plot"  = "text_freq_plot",
        "Word Frequency Table" = "text_freq_table",
        "N-gram Plot"          = "text_ngram_plot",
        "N-gram Table"         = "text_ngram_table",
        "Sentiment Summary"    = "text_sent_summary",
        "Sentiment Plot"       = "text_sent_plot",
        "Sentiment Table"      = "text_sent_table"
      )
      div(
        style = paste0(
          "margin-left:24px; margin-top:-4px; margin-bottom:12px; ",
          "padding:10px 14px; border-left:2px solid #2a2a2a; ",
          "background:#0a0a0a; border-radius:0 4px 4px 0;"
        ),
        p(style = "color:#888; font-size:12px; margin:0 0 6px;",
          "Text Analysis sub-items to include:"),
        checkboxGroupInput(ns("selected_text_items"), NULL,
          choices  = all_choices,
          selected = names(all_choices)
        )
      )
    })

    # ── Nested hypothesis test selector ────────────────────────────────
    output$hyp_selector_ui <- renderUI({
      tests <- report_rv$hyp_results
      if (length(tests) == 0) return(NULL)
      if (!("hyp_test" %in% input$sections_include)) return(NULL)
      div(
        style = paste0(
          "margin-left:24px; margin-top:-4px; margin-bottom:12px; ",
          "padding:10px 14px; border-left:2px solid #2a2a2a; ",
          "background:#0a0a0a; border-radius:0 4px 4px 0;"
        ),
        p(style = "color:#888; font-size:12px; margin:0 0 6px;",
          paste0(length(tests), " hypothesis test(s) run \u2014 choose which to include:")),
        checkboxGroupInput(ns("selected_tests"), NULL,
          choices  = names(tests),
          selected = names(tests)
        )
      )
    })

    # ── Sections status panel ──────────────────────────────────────────
    output$sections_status <- renderUI({
      has_data    <- !is.null(dataset())
      has_clean   <- !is.null(report_rv$clean_log)   && length(report_rv$clean_log) > 0
      has_hyp     <- !is.null(report_rv$hyp_results) && length(report_rv$hyp_results) > 0
      has_plots   <- !is.null(report_rv$plot_list)   && length(report_rv$plot_list) > 0
      has_text    <- !is.null(report_rv$text_freq)   && nrow(report_rv$text_freq) > 0
      has_learn   <- !is.null(report_rv$learn_bench)
      has_details <- !is.null(report_rv$learn_preds) && length(report_rv$learn_preds) > 0
      has_fi      <- !is.null(report_rv$learn_feat_imp)

      .sr <- function(label, active, note = "") {
        icon <- if (active) "\u2713" else "\u25cb"
        clr  <- if (active) "#00cc44" else "#444"
        div(style = "margin:4px 0;",
          span(style = paste0("color:", clr, "; width:18px; display:inline-block;"), icon),
          span(style = paste0("color:", clr, "; font-size:13px;"), label),
          if (nchar(note) > 0)
            span(style = "color:#555; font-size:12px; margin-left:8px;", note)
        )
      }

      tagList(
        .sr("Dataset Overview + Variable Summary", has_data,
            if (has_data) paste0(format(nrow(dataset()), big.mark=","),
                                 " rows \u00d7 ", ncol(dataset()), " cols") else ""),
        .sr("Numeric Statistics", has_data,
            if (has_data) paste0(sum(sapply(dataset(), is.numeric)), " numeric col(s)") else ""),
        .sr("Missing Value Analysis", has_data),
        .sr("Cleaning Log", has_clean,
            if (has_clean) paste0(length(report_rv$clean_log), " action(s) logged")
            else "perform cleaning in CleanR"),
        .sr("Normality + Outlier Tests", has_data),
        .sr("Hypothesis Tests", has_hyp,
            if (has_hyp) paste0(length(report_rv$hyp_results), " test(s) run")
            else "run tests in AnalyzeR"),
        .sr("Plots", has_plots,
            if (has_plots) paste0(length(report_rv$plot_list), " plot(s) generated")
            else "generate plots in PlotR"),
        .sr("Text Analysis", has_text,
            if (has_text) paste0("column: ", report_rv$text_col)
            else "analyse text in TextR"),
        .sr("ML Benchmark", has_learn,
            if (has_learn) paste0(report_rv$learn_pt, " \u2014 ",
                                  nrow(report_rv$learn_bench), " model(s)")
            else "train models in LearnR"),
        .sr("ML Model Details", has_details,
            if (has_details) paste0(length(report_rv$learn_preds), " model(s) ready")
            else "train models in LearnR"),
        .sr("ML Feature Importance", has_fi,
            if (has_fi) "Random Forest importance ready"
            else "train Random Forest in LearnR")
      )
    })

    # ── Build button: generate HTML with progress dialog ───────────────
    observeEvent(input$build_btn, {
      req(dataset())
      report_html(NULL)    # invalidate previous build

      data     <- dataset()
      rv       <- report_rv
      sec_inc  <- input$sections_include
      sel_plt  <- input$selected_plots
      sel_tst  <- input$selected_tests
      sel_txt  <- input$selected_text_items

      n_rows   <- nrow(data); n_cols <- ncol(data)
      n_num    <- sum(sapply(data, is.numeric))
      n_plots  <- length(rv$plot_list  %||% list())
      n_models <- length(rv$learn_preds %||% list())

      html <- withProgress(
        message = "analyzeR \u2014 Building Report",
        detail  = "Starting...",
        value   = 0,
        {
          tryCatch(
            .build_html_report(
              data               = data,
              rv                 = rv,
              sections_include   = sec_inc,
              selected_plots     = sel_plt,
              selected_tests     = sel_tst,
              selected_text_items = sel_txt,
              n_rows             = n_rows,
              n_cols             = n_cols,
              n_num              = n_num,
              n_plots            = n_plots,
              n_models           = n_models,
              progress_cb        = function(v, detail) setProgress(value = v, detail = detail)
            ),
            error = function(e) {
              msg <- conditionMessage(e)
              paste0(
                '<!DOCTYPE html><html><head><meta charset="UTF-8">',
                '<style>body{font-family:sans-serif;background:#111;color:#e44;padding:32px;}',
                'pre{background:#1a1a1a;padding:16px;color:#ccc;border-radius:4px;}</style>',
                '</head><body>',
                '<h2>Report generation failed</h2>',
                '<p>An error occurred while building the report:</p>',
                '<pre>', gsub("<","&lt;", gsub("&","&amp;", msg)), '</pre>',
                '<p style="color:#888;">',
                'Try deselecting sections with missing data, or check the R console for details.',
                '</p></body></html>'
              )
            }
          )
        }
      )

      report_html(html)
      showNotification(
        "Report built \u2014 click Download to save the HTML file.",
        type = "message", duration = 6
      )
    })

    # ── Download UI: shown only after a successful build ───────────────
    output$download_ui <- renderUI({
      if (is.null(report_html())) return(
        div(style = "color:#444; font-size:12px; margin-top:4px;",
            "Click Build Report above to generate the report first.")
      )
      tagList(
        div(style = "color:#00cc44; font-size:12px; margin-bottom:6px;",
            "\u2713 Report ready"),
        downloadButton(ns("dl_report"), "Download Report (.html)",
                       class  = "btn-primary",
                       style  = "width:100%;")
      )
    })

    # ── Download handler: just writes pre-built HTML ───────────────────
    output$dl_report <- downloadHandler(
      filename = function() {
        paste0("analyzeR_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
      },
      content = function(file) {
        html <- report_html()
        if (is.null(html)) {
          html <- "<html><body><p>No report generated. Click Build Report first.</p></body></html>"
        }
        # Write as UTF-8 explicitly — avoids locale encoding failures on all platforms
        con <- file(file, encoding = "UTF-8", open = "w")
        tryCatch(
          writeLines(html, con = con, useBytes = FALSE),
          finally = close(con)
        )
      }
    )
  })
}


# ── HTML report builder ────────────────────────────────────────────────────────
# Pure function — no reactive dependencies.
# progress_cb(value 0-1, detail_string) is called at each section to update the
# Shiny progress bar. Pass NULL when calling outside of a withProgress context.
.build_html_report <- function(data, rv,
                               sections_include    = NULL,
                               selected_plots      = NULL,
                               selected_tests      = NULL,
                               selected_text_items = NULL,
                               n_rows = 0L, n_cols = 0L,
                               n_num = 0L, n_plots = 0L, n_models = 0L,
                               progress_cb = NULL) {

  .step <- function(v, msg) if (!is.null(progress_cb)) progress_cb(v, msg)

  # Is a section selected?
  inc <- function(key) is.null(sections_include) || key %in% sections_include

  # HTML-escape
  .esc <- function(x) {
    x <- as.character(x)
    x <- gsub("&",  "&amp;",  x, fixed = TRUE)
    x <- gsub("<",  "&lt;",   x, fixed = TRUE)
    x <- gsub(">",  "&gt;",   x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x
  }

  # data.frame → HTML table — handles any column types gracefully
  .tbl <- function(df, max_rows = 500, row_names = FALSE) {
    if (is.null(df) || nrow(df) == 0)
      return("<p style='color:#888;font-size:13px;'>No data available.</p>")
    df <- head(df, max_rows)
    # Safely coerce all columns to character
    df_ch <- as.data.frame(
      lapply(df, function(x) {
        tryCatch(as.character(x), error = function(e) rep("?", length(x)))
      }),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    if (row_names) {
      hdr  <- paste0("<th></th><th>",
                     paste(.esc(names(df)), collapse = "</th><th>"), "</th>")
      rows <- vapply(seq_len(nrow(df_ch)), function(i)
        paste0("<tr><th>", .esc(rownames(df)[i]), "</th><td>",
               paste(.esc(df_ch[i, ]), collapse = "</td><td>"),
               "</td></tr>"), character(1))
    } else {
      hdr  <- paste0("<th>", paste(.esc(names(df)), collapse = "</th><th>"), "</th>")
      rows <- vapply(seq_len(nrow(df_ch)), function(i)
        paste0("<tr><td>",
               paste(.esc(df_ch[i, ]), collapse = "</td><td>"),
               "</td></tr>"), character(1))
    }
    paste0('<div class="tbl-wrap"><table class="rt"><thead><tr>', hdr,
           '</tr></thead><tbody>', paste(rows, collapse = ""),
           '</tbody></table></div>')
  }

  # Render a ggplot to a base64-embedded <img> tag
  .plot_to_img <- function(p, alt = "", width = 9, height = 5) {
    tryCatch({
      if (!isTRUE(.base64_available)) stop("base64enc not installed")
      tmp <- tempfile(fileext = ".png")
      on.exit(unlink(tmp), add = TRUE)
      ggplot2::ggsave(tmp, plot = p, width = width, height = height,
                      dpi = 130, bg = "#0d0d0d")
      b64 <- base64enc::base64encode(tmp)
      paste0('<img src="data:image/png;base64,', b64,
             '" style="max-width:100%;border-radius:4px;" alt="', .esc(alt), '"/>')
    }, error = function(e) {
      paste0('<p style="color:#888;font-size:13px;">Chart could not be embedded: ',
             .esc(conditionMessage(e)),
             '. Install base64enc: <code>install.packages("base64enc")</code></p>')
    })
  }

  # Wrap a section — catches errors so one bad section never kills the whole report
  sec_num <- 0L
  .sec <- function(title, body_expr) {
    sec_num <<- sec_num + 1L
    body <- tryCatch(
      force(body_expr),
      error = function(e)
        paste0('<p style="color:#f88;font-size:13px;">Section could not be rendered: ',
               .esc(conditionMessage(e)), '</p>')
    )
    paste0('<div class="sec"><h2>', sec_num, '. ', .esc(title), '</h2>', body, '</div>')
  }

  sections <- character(0)

  # ── 1. Dataset Overview ──────────────────────────────────────────────────
  .step(0.05, paste0("Dataset overview (", format(n_rows, big.mark=","),
                     " rows \u00d7 ", n_cols, " cols)..."))
  if (!is.null(data) && inc("overview")) {
    sections <- c(sections, .sec("Dataset Overview", {
      n     <- nrow(data); p <- ncol(data)
      total <- as.numeric(n) * p
      mn    <- sum(is.na(data))
      nd    <- sum(duplicated(data))
      mem   <- format(object.size(data), units = "auto")
      n_num_c <- sum(sapply(data, is.numeric))
      n_cat_c <- sum(sapply(data, function(x) is.character(x) || is.factor(x)))
      n_dat_c <- sum(sapply(data, function(x) inherits(x, c("Date","POSIXct","POSIXlt"))))
      n_lgl_c <- sum(sapply(data, is.logical))
      mr      <- if (total > 0) paste0(round(100 * mn / total, 2), "%") else "0%"
      file_ln <- if (!is.null(rv$file_name))
        paste0('<p><strong>File:</strong> ', .esc(rv$file_name), '</p>') else ""
      ov_df <- data.frame(
        Metric = c("Rows", "Columns", "Numeric Cols", "Categorical Cols",
                   "Date/Time Cols", "Logical Cols",
                   "Total Cells", "Missing Cells", "Missing Rate",
                   "Duplicate Rows", "Memory"),
        Value  = c(format(n, big.mark=","), p, n_num_c, n_cat_c,
                   n_dat_c, n_lgl_c,
                   format(total, big.mark=","),
                   format(mn, big.mark=","), mr,
                   format(nd, big.mark=","), mem),
        stringsAsFactors = FALSE
      )
      paste0(file_ln, .tbl(ov_df))
    }))
  }

  # ── 2. Variable Summary ──────────────────────────────────────────────────
  .step(0.12, paste0("Variable summary for ", n_cols, " columns..."))
  if (!is.null(data) && inc("var_summary")) {
    sections <- c(sections, .sec("Variable Summary", {
      mn_v <- sapply(data, function(x) sum(is.na(x)))
      vd <- data.frame(
        Variable    = names(data),
        `R Type`    = sapply(data, function(x) paste(class(x), collapse="/")),
        `Sem. Type` = sapply(data, infer_semantic_type),
        Missing     = unname(mn_v),
        `Missing %` = paste0(round(100 * mn_v / max(nrow(data), 1), 1), "%"),
        Unique      = sapply(data, function(x) length(unique(x[!is.na(x)]))),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      .tbl(vd)
    }))
  }

  # ── 3. Numeric Statistics ────────────────────────────────────────────────
  .step(0.20, paste0("Numeric statistics for ", n_num, " numeric column(s)..."))
  if (!is.null(data) && inc("num_stats")) {
    nc <- names(data)[sapply(data, is.numeric)]
    if (length(nc) == 0) {
      sections <- c(sections, .sec("Numeric Statistics",
        "<p style='color:#888;font-size:13px;'>No numeric columns in this dataset.</p>"))
    } else {
      sections <- c(sections, .sec("Numeric Statistics", {
        ns_df <- do.call(rbind, lapply(nc, function(v) {
          x <- data[[v]]
          x_clean <- x[!is.na(x) & is.finite(x)]
          if (length(x_clean) == 0)
            return(data.frame(Variable=v, Min=NA, Q1=NA, Median=NA, Mean=NA,
                              Q3=NA, Max=NA, SD=NA, Skewness=NA,
                              check.names=FALSE, stringsAsFactors=FALSE))
          data.frame(
            Variable = v,
            Min      = round(min(x_clean), 4),
            Q1       = round(quantile(x_clean, .25), 4),
            Median   = round(median(x_clean), 4),
            Mean     = round(mean(x_clean), 4),
            Q3       = round(quantile(x_clean, .75), 4),
            Max      = round(max(x_clean), 4),
            SD       = round(sd(x_clean), 4),
            Skewness = round(col_skewness(x_clean), 3),
            check.names = FALSE, stringsAsFactors = FALSE
          )
        }))
        .tbl(ns_df)
      }))
    }
  }

  # ── 4. Missing Value Analysis ────────────────────────────────────────────
  .step(0.28, "Missing value analysis...")
  if (!is.null(data) && inc("missing")) {
    sections <- c(sections, .sec("Missing Value Analysis", {
      mn_v <- sapply(data, function(x) sum(is.na(x)))
      mp_v <- round(100 * mn_v / max(nrow(data), 1), 1)
      nd_df <- data.frame(
        Variable    = names(data),
        Missing     = unname(mn_v),
        `Missing %` = paste0(unname(mp_v), "%"),
        Complete    = paste0(pmax(0, 100 - unname(mp_v)), "%"),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      nd_df <- nd_df[order(-nd_df$Missing), ]
      .tbl(nd_df)
    }))
  }

  # ── 5. Cleaning Log ──────────────────────────────────────────────────────
  .step(0.34, "Cleaning log...")
  if (!is.null(rv$clean_log) && length(rv$clean_log) > 0 && inc("clean_log")) {
    sections <- c(sections, .sec("Data Cleaning Log", {
      paste0('<pre>', paste(.esc(rv$clean_log), collapse="\n"), '</pre>')
    }))
  }

  # ── 6. Normality Tests ───────────────────────────────────────────────────
  .step(0.40, paste0("Shapiro-Wilk normality tests for ", n_num, " numeric column(s)..."))
  if (!is.null(data) && inc("normality")) {
    nc <- names(data)[sapply(data, is.numeric)]
    if (length(nc) == 0) {
      sections <- c(sections, .sec("Normality Tests (Shapiro-Wilk)",
        "<p style='color:#888;font-size:13px;'>No numeric columns in this dataset.</p>"))
    } else {
      sections <- c(sections, .sec("Normality Tests (Shapiro-Wilk)", {
        norm_rows <- lapply(nc, function(v) {
          x <- na.omit(data[[v]])
          x <- x[is.finite(x)]
          if (length(x) < 3 || length(unique(x)) < 2) return(
            data.frame(Variable=v, W=NA, p_value=NA,
                       Result="Insufficient data", stringsAsFactors=FALSE))
          if (length(x) > 5000) x <- sample(x, 5000)
          tryCatch({
            t <- shapiro.test(x)
            data.frame(Variable=v,
                       W       = round(t$statistic, 4),
                       p_value = round(t$p.value,  4),
                       Result  = ifelse(t$p.value >= 0.05, "Normal", "Non-normal"),
                       stringsAsFactors=FALSE)
          }, error = function(e)
            data.frame(Variable=v, W=NA, p_value=NA,
                       Result=paste0("Error: ", conditionMessage(e)),
                       stringsAsFactors=FALSE))
        })
        ndf <- do.call(rbind, norm_rows)
        .tbl(ndf)
      }))
    }
  }

  # ── 7. Outlier Detection ─────────────────────────────────────────────────
  .step(0.47, "IQR outlier detection...")
  if (!is.null(data) && inc("outliers")) {
    nc <- names(data)[sapply(data, is.numeric)]
    if (length(nc) == 0) {
      sections <- c(sections, .sec("Outlier Detection (IQR method)",
        "<p style='color:#888;font-size:13px;'>No numeric columns in this dataset.</p>"))
    } else {
      sections <- c(sections, .sec("Outlier Detection (IQR method)", {
        out_df <- do.call(rbind, lapply(nc, function(v) {
          x <- na.omit(data[[v]]); x <- x[is.finite(x)]
          if (length(x) < 4)
            return(data.frame(Variable=v, Outliers=NA, `Outlier %`=NA,
                              check.names=FALSE, stringsAsFactors=FALSE))
          q1 <- quantile(x, .25); q3 <- quantile(x, .75); iqr <- q3 - q1
          nout <- if (iqr == 0) 0L else sum(x < q1 - 1.5*iqr | x > q3 + 1.5*iqr)
          data.frame(Variable=v,
                     Outliers   = nout,
                     `Outlier %`= round(100 * nout / length(x), 2),
                     check.names=FALSE, stringsAsFactors=FALSE)
        }))
        .tbl(out_df)
      }))
    }
  }

  # ── 8. Hypothesis Tests ───────────────────────────────────────────────────
  .step(0.53, paste0("Hypothesis tests (",
                     length(rv$hyp_results %||% list()), " test(s))..."))
  if (!is.null(rv$hyp_results) && length(rv$hyp_results) > 0 && inc("hyp_test")) {
    tests_to_show <- if (!is.null(selected_tests) && length(selected_tests) > 0)
      rv$hyp_results[names(rv$hyp_results) %in% selected_tests]
    else
      rv$hyp_results

    n_tests <- length(tests_to_show)
    for (i_test in seq_along(tests_to_show)) {
      test_label <- names(tests_to_show)[i_test]
      .step(0.53 + 0.16 * (i_test / max(n_tests, 1L)),
            paste0("Hypothesis test ", i_test, "/", n_tests, ": ", test_label, "..."))
      res    <- tests_to_show[[test_label]]
      pval   <- res$p_value; alpha <- res$alpha
      reject <- !is.null(pval) && !is.na(pval) && pval < alpha
      dec_cl <- if (reject) "color:#00cc44;font-weight:bold;" else "color:#aaa;"
      dec_lb <- if (reject)
        paste0("Reject H\u2080 (p = ", round(pval, 4), " < \u03b1 = ", alpha, ")")
      else
        paste0("Fail to reject H\u2080 (p = ", round(pval, 4), " \u2265 \u03b1 = ", alpha, ")")

      sections <- c(sections, .sec(paste0("Hypothesis Test \u2014 ", test_label), {
        params_html <- ""
        if (!is.null(res$params) && length(res$params) > 0) {
          params_df <- data.frame(Parameter=names(res$params),
                                  Value=unlist(res$params),
                                  stringsAsFactors=FALSE, row.names=NULL)
          params_html <- paste0('<h3>Test Parameters</h3>', .tbl(params_df))
        }
        plot_html <- if (!is.null(res$plot_obj))
          paste0('<h3>Visualization</h3>',
                 .plot_to_img(res$plot_obj, alt=test_label, width=8, height=4.5))
        else ""

        paste0(
          params_html,
          '<p>', .esc(res$interp %||% ""), '</p>',
          '<p><strong>Decision:</strong> <span style="', dec_cl, '">', .esc(dec_lb), '</span></p>',
          if (!is.null(res$stat_df))  .tbl(res$stat_df)  else "",
          if (!is.null(res$tukey_df))
            paste0('<h3>Tukey HSD Pairwise</h3>', .tbl(res$tukey_df, row_names=TRUE)) else "",
          if (!is.null(res$cont_tbl))
            paste0('<h3>Contingency Table</h3>',  .tbl(res$cont_tbl, row_names=TRUE)) else "",
          plot_html
        )
      }))
    }
  }

  # ── 9. Plots ─────────────────────────────────────────────────────────────
  if (!is.null(rv$plot_list) && length(rv$plot_list) > 0 && inc("plots")) {
    plots_to_show <- if (!is.null(selected_plots) && length(selected_plots) > 0)
      rv$plot_list[names(rv$plot_list) %in% selected_plots]
    else
      rv$plot_list

    for (i in seq_along(plots_to_show)) {
      key   <- names(plots_to_show)[i]
      pinfo <- plots_to_show[[key]]
      .step(0.58 + 0.12 * (i / max(length(plots_to_show), 1)),
            paste0("Embedding plot ", i, "/", length(plots_to_show), ": ", key, "..."))
      sections <- c(sections, .sec(paste0("Plot \u2014 ", key),
        .plot_to_img(pinfo$obj, alt = key)))
    }
  } else {
    .step(0.70, "No plots to embed — skipping...")
  }

  # ── 10. Text Analysis ────────────────────────────────────────────────────
  .step(0.72, "Text analysis...")
  if (!is.null(rv$text_freq) && nrow(rv$text_freq) > 0 && inc("text")) {
    # Helper: is a sub-item selected?
    tinc <- function(key) is.null(selected_text_items) || key %in% selected_text_items

    sections <- c(sections, .sec("Text Analysis", {
      parts <- paste0('<p><strong>Column analysed:</strong> ', .esc(rv$text_col %||% ""), '</p>')

      # Word Frequency
      if (tinc("text_freq_plot") && !is.null(rv$text_plots$freq)) {
        parts <- paste0(parts, '<h3>Word Frequency (Top 30)</h3>',
                        .plot_to_img(rv$text_plots$freq, alt="Word Frequency", width=9, height=5))
      }
      if (tinc("text_freq_table") && !is.null(rv$text_freq)) {
        freq_top        <- head(rv$text_freq, 30)
        names(freq_top) <- c("Word", "Count", "Pct%")
        parts <- paste0(parts, '<h3>Word Frequency Table</h3>', .tbl(freq_top))
      }

      # N-grams
      if (tinc("text_ngram_plot") && !is.null(rv$text_plots$ngrams)) {
        parts <- paste0(parts, '<h3>Bigrams (Top 20)</h3>',
                        .plot_to_img(rv$text_plots$ngrams, alt="N-grams", width=9, height=5))
      }
      if (tinc("text_ngram_table") && !is.null(rv$text_ngrams)) {
        ngram_top        <- head(rv$text_ngrams, 20)
        names(ngram_top) <- c("Bigram", "Count", "Pct%")
        parts <- paste0(parts, '<h3>N-gram Table</h3>', .tbl(ngram_top))
      }

      # Sentiment summary
      if (tinc("text_sent_summary") && !is.null(rv$text_sentiment)) {
        ts  <- rv$text_sentiment
        net <- ts$n_pos - ts$n_neg
        ovr <- if (net > 0) "Positive" else if (net < 0) "Negative" else "Neutral"
        parts <- paste0(parts,
          '<h3>Sentiment Summary</h3>',
          '<p>Positive words: <strong>', ts$n_pos, '</strong> &nbsp;|&nbsp; ',
          'Negative words: <strong>', ts$n_neg, '</strong> &nbsp;|&nbsp; ',
          'Net score: <strong>', net, '</strong> &nbsp;|&nbsp; ',
          'Overall: <strong>', ovr, '</strong></p>')
      }

      # Sentiment plot
      if (tinc("text_sent_plot") && !is.null(rv$text_plots$sentiment)) {
        parts <- paste0(parts, '<h3>Sentiment \u2014 Top Positive &amp; Negative Words</h3>',
                        .plot_to_img(rv$text_plots$sentiment, alt="Sentiment", width=9, height=5))
      }

      # Sentiment table
      if (tinc("text_sent_table") && !is.null(rv$text_sentiment$df) &&
          nrow(rv$text_sentiment$df) > 0) {
        wc_sent <- dplyr::count(rv$text_sentiment$df, word, sentiment, sort = TRUE)
        names(wc_sent) <- c("Word", "Sentiment", "Count")
        parts <- paste0(parts, '<h3>Sentiment Word Table</h3>', .tbl(wc_sent))
      }

      parts
    }))
  }

  # ── 11. ML Benchmark ─────────────────────────────────────────────────────
  .step(0.78, "ML benchmark table...")
  if (!is.null(rv$learn_bench) && inc("ml_bench")) {
    sections <- c(sections, .sec("ML Benchmark", {
      best_ln <- if (!is.null(rv$learn_best) && !is.na(rv$learn_best))
        paste0('<p><strong>Best model:</strong> \u2605 ', .esc(rv$learn_best), '</p>') else ""
      sample_html <- if (!is.null(rv$learn_rows_orig) && !is.null(rv$learn_rows_used)) {
        if (rv$learn_rows_used < rv$learn_rows_orig)
          paste0('<p><strong>Training rows:</strong> ',
                 format(rv$learn_rows_used, big.mark=","), ' sampled from ',
                 format(rv$learn_rows_orig, big.mark=","), ' (seed\u00a0=\u00a042)</p>')
        else
          paste0('<p><strong>Training rows:</strong> ',
                 format(rv$learn_rows_orig, big.mark=","), ' (full dataset)</p>')
      } else ""
      paste0('<p><strong>Problem type:</strong> ', .esc(rv$learn_pt %||% ""),
             ' &nbsp;|&nbsp; <strong>Target:</strong> ', .esc(rv$learn_target %||% ""), '</p>',
             sample_html, best_ln, .tbl(rv$learn_bench))
    }))
  }

  # ── 12. ML Model Details ──────────────────────────────────────────────────
  .step(0.84, paste0("ML model detail plots (", n_models, " model(s))..."))
  if (!is.null(rv$learn_preds) && length(rv$learn_preds) > 0 && inc("ml_details")) {
    pt <- rv$learn_pt; actual <- rv$learn_actual
    algo_names <- names(rv$learn_preds)
    detail_parts <- lapply(seq_along(algo_names), function(i_algo) {
      algo <- algo_names[[i_algo]]
      .step(0.84 + 0.07 * (i_algo / max(length(algo_names), 1L)),
            paste0("Model details: ", algo, " (", i_algo, "/", length(algo_names), ")..."))
      preds <- rv$learn_preds[[algo]]
      if (is.null(preds))
        return(paste0('<h3>', .esc(algo), '</h3>',
                      '<p style="color:#888;">No predictions stored.</p>'))
      p <- tryCatch({
        if (identical(pt, "Classification")) {
          lv    <- union(levels(as.factor(actual)), levels(as.factor(preds)))
          cm_df <- as.data.frame(
            table(Predicted=factor(preds, levels=lv), Actual=factor(actual, levels=lv)))
          ggplot2::ggplot(cm_df, ggplot2::aes(x=Actual, y=Predicted, fill=Freq)) +
            ggplot2::geom_tile(color="#111111") +
            ggplot2::geom_text(ggplot2::aes(label=Freq),
                               color="white", size=5, fontface="bold") +
            ggplot2::scale_fill_gradient(low="#0a1a0a", high="#00cc44") +
            .dark_theme() +
            ggplot2::labs(title=paste("Confusion Matrix \u2014", algo)) +
            ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45, hjust=1))
        } else {
          a <- as.numeric(actual); pv <- as.numeric(preds)
          ggplot2::ggplot(data.frame(Actual=a, Predicted=pv),
                          ggplot2::aes(x=Actual, y=Predicted)) +
            ggplot2::geom_point(alpha=0.65, color="#00ff00") +
            ggplot2::geom_abline(slope=1, intercept=0,
                                 color="white", linetype="dashed", linewidth=0.7) +
            .dark_theme() +
            ggplot2::labs(title=paste("Actual vs Predicted \u2014", algo))
        }
      }, error=function(e) NULL)
      img_html <- if (!is.null(p)) .plot_to_img(p, alt=algo, width=8, height=5)
                  else paste0('<p style="color:#888;">Chart unavailable for ',
                              .esc(algo), '.</p>')
      paste0('<h3 style="color:#ccc;border-bottom:1px solid #1e1e1e;',
             'padding-bottom:6px;margin:20px 0 10px;">', .esc(algo), '</h3>', img_html)
    })
    lbl  <- if (identical(pt, "Classification")) "Confusion Matrices" else "Actual vs Predicted"
    sections <- c(sections, .sec(paste0("ML Model Details \u2014 ", lbl),
                                 paste(unlist(detail_parts), collapse="\n")))
  }

  # ── 13. ML Feature Importance ─────────────────────────────────────────────
  .step(0.92, "Feature importance plot...")
  if (!is.null(rv$learn_feat_imp) && inc("ml_feat_imp")) {
    sections <- c(sections, .sec("ML Feature Importance (Random Forest)", {
      fi  <- rv$learn_feat_imp
      df  <- fi[order(fi$Importance), ]
      df$Feature <- factor(df$Feature, levels=df$Feature)
      p <- ggplot2::ggplot(df, ggplot2::aes(x=Feature, y=Importance)) +
        ggplot2::geom_bar(stat="identity", fill="#00ff00", alpha=0.85, color="black") +
        ggplot2::coord_flip() +
        .dark_theme() +
        ggplot2::labs(title="Feature Importance (Random Forest)",
                      x=NULL, y="Impurity Importance")
      .plot_to_img(p, alt="Feature Importance", width=8, height=5)
    }))
  }

  .step(0.98, "Assembling final HTML...")

  # ── CSS ───────────────────────────────────────────────────────────────────
  css <- paste0(
    "body{font-family:'Segoe UI',Arial,sans-serif;background:#0d0d0d;color:#e0e0e0;margin:0;padding:0;}",
    ".wrap{max-width:1100px;margin:0 auto;padding:32px 24px 60px;}",
    ".hdr{border-bottom:2px solid #1e1e1e;padding-bottom:18px;margin-bottom:32px;}",
    ".hdr h1{color:#00ff00;font-size:28px;letter-spacing:4px;margin:0 0 4px;}",
    ".hdr p{color:#888;font-size:13px;margin:4px 0 0;}",
    ".sec{margin-bottom:28px;border:1px solid #1e1e1e;border-radius:8px;",
         "padding:20px 24px;background:#111;}",
    ".sec h2{color:#00ff00;font-size:16px;margin:0 0 14px;",
             "border-bottom:1px solid #1e1e1e;padding-bottom:8px;}",
    ".sec h3{color:#ccc;font-size:13px;margin:16px 0 8px;}",
    ".sec p{color:#ccc;font-size:13px;margin:6px 0;}",
    "pre{background:#0a0a0a;border:1px solid #2a2a2a;border-radius:4px;",
        "padding:12px;color:#e0e0e0;font-size:12px;white-space:pre-wrap;",
        "word-break:break-word;}",
    ".tbl-wrap{overflow-x:auto;}",
    ".rt{border-collapse:collapse;width:100%;font-size:12px;}",
    ".rt th{background:#161616;color:#fff;border:1px solid #2a2a2a;",
            "padding:6px 10px;text-align:left;white-space:nowrap;}",
    ".rt td{border:1px solid #1e1e1e;padding:5px 10px;color:#ddd;}",
    ".rt tbody tr:nth-child(even){background:#0f0f0f;}",
    "@media print{",
    "  body{background:#fff;color:#000;}",
    "  .sec{border-color:#ccc;background:#fff;}",
    "  .sec h2{color:#006600;}",
    "  .sec p,.sec h3{color:#333;}",
    "  .rt th{background:#eee;color:#000;border-color:#ccc;}",
    "  .rt td{color:#000;border-color:#ccc;}",
    "  .rt tbody tr:nth-child(even){background:#f9f9f9;}",
    "  pre{background:#f5f5f5;color:#000;border-color:#ccc;}",
    "}"
  )

  # ── Assemble ──────────────────────────────────────────────────────────────
  dt      <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  file_lb <- if (!is.null(rv$file_name))
    paste0(" &nbsp;&bull;&nbsp; ", .esc(rv$file_name)) else ""
  n_sec   <- length(sections)

  .step(1.0, paste0("Done \u2014 ", n_sec, " section(s) built."))

  paste0(
    '<!DOCTYPE html><html lang="en"><head>',
    '<meta charset="UTF-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1.0">',
    '<title>analyzeR Report</title>',
    '<style>', css, '</style>',
    '</head><body><div class="wrap">',
    '<div class="hdr">',
    '<h1>analyzeR</h1>',
    '<p>Report generated: ', dt, file_lb,
    ' &nbsp;&bull;&nbsp; ', n_sec, ' section(s)</p>',
    '</div>',
    paste(sections, collapse = "\n"),
    '</div></body></html>'
  )
}
