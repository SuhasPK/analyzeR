# ─────────────────────────────────────────────────────────────────────────────
# reportR.R  —  Session Report module
#
# Captures outputs from all other tabs and generates a downloadable HTML report.
# Open the HTML in any browser and use Ctrl+P → Save as PDF to export a PDF.
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
          "Select the sections to include, then download. ",
          "Downloads as HTML. Open in browser, then ",
          tags$strong(style = "color:#ccc;", "Ctrl+P \u2192 Save as PDF"),
          " to export a PDF."
        ),
        br(),
        h5("Select Sections to Include"),
        p(style = "color:#666; font-size:12px; margin:-4px 0 10px;",
          "Sections without data will be skipped automatically even if checked."),
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
        # Nested plot selector — visible when plots exist and "Plots" is checked
        uiOutput(ns("plot_selector_ui")),
        # Nested hypothesis test selector — visible when tests exist and "Hypothesis Test" is checked
        uiOutput(ns("hyp_selector_ui")),
        br(),
        downloadButton(ns("dl_report"), "Download Report (.html)", class = "btn-primary"),
        br(), br(),
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

    # ── No-data guard ──────────────────────────────────────────────────
    output$tab_guard <- renderUI({
      if (!is.null(dataset())) return(NULL)
      .no_data_ui()
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

    # ── Nested hypothesis test selector ────────────────────────────────────
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

    # ── Sections status panel ──────────────────────────────────────────────
    output$sections_status <- renderUI({
      has_data    <- !is.null(dataset())
      has_clean   <- !is.null(report_rv$clean_log) && length(report_rv$clean_log) > 0
      has_hyp     <- !is.null(report_rv$hyp_results) && length(report_rv$hyp_results) > 0
      has_plots   <- !is.null(report_rv$plot_list) && length(report_rv$plot_list) > 0
      has_text    <- !is.null(report_rv$text_freq) && nrow(report_rv$text_freq) > 0
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
            if (has_data) paste0(format(nrow(dataset()), big.mark = ","),
                                 " rows \u00d7 ", ncol(dataset()), " cols") else ""),
        .sr("Numeric Statistics",    has_data,
            if (has_data) paste0(sum(sapply(dataset(), is.numeric)), " numeric col(s)") else ""),
        .sr("Missing Value Analysis", has_data),
        .sr("Cleaning Log",           has_clean,
            if (has_clean) paste0(length(report_rv$clean_log), " action(s) logged")
            else "perform cleaning in CleanR"),
        .sr("Normality + Outlier Tests", has_data),
        .sr("Hypothesis Tests",        has_hyp,
            if (has_hyp) paste0(length(report_rv$hyp_results), " test(s) run")
            else "run tests in AnalyzeR"),
        .sr("Plots",                  has_plots,
            if (has_plots) paste0(length(report_rv$plot_list), " plot(s) generated")
            else "generate plots in PlotR"),
        .sr("Text Analysis",          has_text,
            if (has_text) paste0("column: ", report_rv$text_col) else "analyze text in TextR"),
        .sr("ML Benchmark",           has_learn,
            if (has_learn) paste0(report_rv$learn_pt, " \u2014 ",
                                  nrow(report_rv$learn_bench), " model(s)")
            else "train models in LearnR"),
        .sr("ML Model Details",       has_details,
            if (has_details) paste0(length(report_rv$learn_preds), " model(s) ready")
            else "train models in LearnR"),
        .sr("ML Feature Importance",  has_fi,
            if (has_fi) "Random Forest importance ready"
            else "train Random Forest in LearnR")
      )
    })

    # ── Download handler ───────────────────────────────────────────────
    output$dl_report <- downloadHandler(
      filename = function() {
        paste0("analyzeR_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
      },
      content = function(file) {
        html <- .build_html_report(dataset(), report_rv,
                                   sections_include = input$sections_include,
                                   selected_plots   = input$selected_plots,
                                   selected_tests   = input$selected_tests)
        writeLines(html, file)
      }
    )
  })
}


# ── HTML report builder ────────────────────────────────────────────────────────
# Pure function — no reactive dependencies. Called from downloadHandler.
.build_html_report <- function(data, rv, sections_include = NULL,
                               selected_plots = NULL, selected_tests = NULL) {
  # Helper: is a section key selected (NULL = include all)
  inc <- function(key) is.null(sections_include) || key %in% sections_include

  # HTML-escape helper
  .esc <- function(x) {
    x <- as.character(x)
    x <- gsub("&",  "&amp;",  x, fixed = TRUE)
    x <- gsub("<",  "&lt;",   x, fixed = TRUE)
    x <- gsub(">",  "&gt;",   x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x
  }

  # data.frame → HTML table
  .tbl <- function(df, max_rows = 200, row_names = FALSE) {
    if (is.null(df) || nrow(df) == 0)
      return("<p style='color:#888;font-size:13px;'>No data.</p>")
    df <- head(df, max_rows)
    if (row_names) {
      hdr  <- paste0("<th></th><th>",
                     paste(.esc(names(df)), collapse = "</th><th>"), "</th>")
      rows <- vapply(seq_len(nrow(df)), function(i)
        paste0("<tr><th>", .esc(rownames(df)[i]), "</th><td>",
               paste(.esc(as.character(df[i, ])), collapse = "</td><td>"),
               "</td></tr>"), character(1))
    } else {
      hdr  <- paste0("<th>", paste(.esc(names(df)), collapse = "</th><th>"), "</th>")
      rows <- vapply(seq_len(nrow(df)), function(i)
        paste0("<tr><td>",
               paste(.esc(as.character(df[i, ])), collapse = "</td><td>"),
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
      ggplot2::ggsave(tmp, plot = p, width = width, height = height,
                      dpi = 130, bg = "#0d0d0d")
      b64 <- base64enc::base64encode(tmp)
      unlink(tmp)
      paste0('<img src="data:image/png;base64,', b64,
             '" style="max-width:100%;border-radius:4px;" alt="', .esc(alt), '"/>')
    }, error = function(e) {
      paste0('<p style="color:#888;font-size:13px;">',
             'Chart could not be embedded (', .esc(conditionMessage(e)), '). ',
             'Install base64enc: <code>install.packages("base64enc")</code></p>')
    })
  }

  # Section wrapper
  sec_num  <- 0L
  .sec <- function(title, body) {
    sec_num <<- sec_num + 1L
    paste0('<div class="sec"><h2>', sec_num, '. ', .esc(title), '</h2>', body, '</div>')
  }

  sections <- character(0)

  # ── 1. Dataset Overview ──────────────────────────────────────────────────
  if (!is.null(data) && inc("overview")) {
    n     <- nrow(data); p <- ncol(data)
    total <- as.numeric(n) * p
    mn    <- sum(is.na(data))
    nd    <- sum(duplicated(data))
    mem   <- format(object.size(data), units = "auto")
    n_num <- sum(sapply(data, is.numeric))
    n_cat <- sum(sapply(data, function(x) is.character(x) || is.factor(x)))
    mr    <- if (total > 0) paste0(round(100 * mn / total, 2), "%") else "0%"

    file_ln <- if (!is.null(rv$file_name))
      paste0('<p><strong>File:</strong> ', .esc(rv$file_name), '</p>') else ""

    ov_df <- data.frame(
      Metric = c("Rows", "Columns", "Numeric Cols", "Categorical Cols",
                 "Total Cells", "Missing Cells", "Duplicate Rows", "Memory"),
      Value  = c(format(n, big.mark = ","), p, n_num, n_cat,
                 format(total, big.mark = ","),
                 paste0(format(mn, big.mark = ","), " (", mr, ")"),
                 format(nd, big.mark = ","), mem),
      stringsAsFactors = FALSE
    )
    sections <- c(sections, .sec("Dataset Overview",
      paste0(file_ln, .tbl(ov_df))))
  }

  # ── 2. Variable Summary ──────────────────────────────────────────────────
  if (!is.null(data) && inc("var_summary")) {
    mn_v <- sapply(data, function(x) sum(is.na(x)))
    vd <- data.frame(
      Variable      = names(data),
      `R Type`      = sapply(data, function(x) paste(class(x), collapse = "/")),
      `Sem. Type`   = sapply(data, infer_semantic_type),
      Missing       = unname(mn_v),
      `Missing %`   = paste0(round(100 * mn_v / nrow(data), 1), "%"),
      Unique        = sapply(data, function(x) length(unique(x[!is.na(x)]))),
      check.names   = FALSE, stringsAsFactors = FALSE
    )
    sections <- c(sections, .sec("Variable Summary", .tbl(vd)))
  }

  # ── 3. Numeric Statistics ────────────────────────────────────────────────
  if (!is.null(data) && inc("num_stats")) {
    nc <- names(data)[sapply(data, is.numeric)]
    if (length(nc) > 0) {
      ns_df <- do.call(rbind, lapply(nc, function(v) {
        x <- data[[v]]
        data.frame(
          Variable = v,
          Min      = round(min(x, na.rm = TRUE), 4),
          Q1       = round(quantile(x, .25, na.rm = TRUE), 4),
          Median   = round(median(x, na.rm = TRUE), 4),
          Mean     = round(mean(x, na.rm = TRUE), 4),
          Q3       = round(quantile(x, .75, na.rm = TRUE), 4),
          Max      = round(max(x, na.rm = TRUE), 4),
          SD       = round(sd(x, na.rm = TRUE), 4),
          Skewness = round(col_skewness(x), 3),
          check.names = FALSE, stringsAsFactors = FALSE
        )
      }))
      sections <- c(sections, .sec("Numeric Statistics", .tbl(ns_df)))
    }
  }

  # ── 4. Missing Value Analysis ────────────────────────────────────────────
  if (!is.null(data) && inc("missing")) {
    mn_v <- sapply(data, function(x) sum(is.na(x)))
    mp_v <- round(100 * mn_v / nrow(data), 1)
    nd_df <- data.frame(
      Variable    = names(data),
      Missing     = unname(mn_v),
      `Missing %` = paste0(unname(mp_v), "%"),
      Complete    = paste0(100 - unname(mp_v), "%"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    nd_df <- nd_df[order(-nd_df$Missing), ]
    sections <- c(sections, .sec("Missing Value Analysis", .tbl(nd_df)))
  }

  # ── 5. Cleaning Log ──────────────────────────────────────────────────────
  if (!is.null(rv$clean_log) && length(rv$clean_log) > 0 && inc("clean_log")) {
    log_html <- paste0(
      '<pre>',
      paste(.esc(rv$clean_log), collapse = "\n"),
      '</pre>'
    )
    sections <- c(sections, .sec("Data Cleaning Log", log_html))
  }

  # ── 6. Normality Tests ───────────────────────────────────────────────────
  if (!is.null(data) && inc("normality")) {
    nc <- names(data)[sapply(data, is.numeric)]
    if (length(nc) > 0) {
      norm_rows <- lapply(nc, function(v) {
        x <- na.omit(data[[v]])
        if (length(x) > 5000) x <- sample(x, 5000)
        if (length(x) < 3) return(NULL)
        tryCatch({
          t <- shapiro.test(x)
          data.frame(Variable = v,
                     W        = round(t$statistic, 4),
                     p_value  = round(t$p.value, 4),
                     Result   = ifelse(t$p.value >= 0.05, "Normal", "Non-normal"),
                     stringsAsFactors = FALSE)
        }, error = function(e) NULL)
      })
      ndf <- do.call(rbind, Filter(Negate(is.null), norm_rows))
      if (!is.null(ndf) && nrow(ndf) > 0)
        sections <- c(sections, .sec("Normality Tests (Shapiro-Wilk)", .tbl(ndf)))
    }
  }

  # ── 7. Outlier Detection ─────────────────────────────────────────────────
  if (!is.null(data) && inc("outliers")) {
    nc <- names(data)[sapply(data, is.numeric)]
    if (length(nc) > 0) {
      out_df <- do.call(rbind, lapply(nc, function(v) {
        x    <- na.omit(data[[v]])
        q1   <- quantile(x, .25); q3 <- quantile(x, .75)
        nout <- sum(x < q1 - 1.5*(q3-q1) | x > q3 + 1.5*(q3-q1))
        data.frame(Variable    = v,
                   Outliers    = nout,
                   `Outlier %` = round(100 * nout / length(x), 2),
                   check.names = FALSE, stringsAsFactors = FALSE)
      }))
      sections <- c(sections, .sec("Outlier Detection (IQR method)", .tbl(out_df)))
    }
  }

  # ── 8. Hypothesis Tests (one section per test run) ───────────────────────
  if (!is.null(rv$hyp_results) && length(rv$hyp_results) > 0 && inc("hyp_test")) {

    # Filter to user-selected tests; fall back to all if none specified
    tests_to_show <- if (!is.null(selected_tests) && length(selected_tests) > 0)
      rv$hyp_results[names(rv$hyp_results) %in% selected_tests]
    else
      rv$hyp_results

    for (test_label in names(tests_to_show)) {
      res    <- tests_to_show[[test_label]]
      pval   <- res$p_value
      alpha  <- res$alpha
      reject <- !is.null(pval) && !is.na(pval) && pval < alpha
      dec_cl <- if (reject) "color:#00cc44;font-weight:bold;" else "color:#aaa;"
      dec_lb <- if (reject)
        paste0("Reject H\u2080 (p = ", round(pval, 4), " < \u03b1 = ", alpha, ")")
      else
        paste0("Fail to reject H\u2080 (p = ", round(pval, 4), " \u2265 \u03b1 = ", alpha, ")")

      params_html <- ""
      if (!is.null(res$params) && length(res$params) > 0) {
        params_df <- data.frame(
          Parameter = names(res$params),
          Value     = unlist(res$params),
          stringsAsFactors = FALSE, row.names = NULL)
        params_html <- paste0('<h3>Test Parameters</h3>', .tbl(params_df))
      }

      plot_html <- ""
      if (!is.null(res$plot_obj)) {
        plot_html <- paste0(
          '<h3>Visualization</h3>',
          .plot_to_img(res$plot_obj, alt = test_label, width = 8, height = 4.5))
      }

      body <- paste0(
        params_html,
        '<p>', .esc(res$interp), '</p>',
        '<p><strong>Decision:</strong> <span style="', dec_cl, '">', .esc(dec_lb), '</span></p>',
        if (!is.null(res$stat_df)) .tbl(res$stat_df) else "",
        if (!is.null(res$tukey_df))
          paste0('<h3>Tukey HSD Pairwise</h3>', .tbl(res$tukey_df, row_names = TRUE)) else "",
        if (!is.null(res$cont_tbl))
          paste0('<h3>Contingency Table</h3>', .tbl(res$cont_tbl, row_names = TRUE)) else "",
        plot_html
      )
      sections <- c(sections, .sec(paste0("Hypothesis Test \u2014 ", test_label), body))
    }
  }

  # ── 9. Plots ─────────────────────────────────────────────────────────────
  if (!is.null(rv$plot_list) && length(rv$plot_list) > 0 && inc("plots")) {
    # Filter to user-selected plots; fall back to all if none specified
    plots_to_show <- if (!is.null(selected_plots) && length(selected_plots) > 0)
      rv$plot_list[names(rv$plot_list) %in% selected_plots]
    else
      rv$plot_list

    for (key in names(plots_to_show)) {
      pinfo    <- plots_to_show[[key]]
      plot_html <- .plot_to_img(pinfo$obj, alt = key)
      sections <- c(sections, .sec(paste0("Plot \u2014 ", key), plot_html))
    }
  }

  # ── 10. Text Analysis ────────────────────────────────────────────────────
  if (!is.null(rv$text_freq) && nrow(rv$text_freq) > 0 && inc("text")) {
    freq_top        <- head(rv$text_freq, 20)
    names(freq_top) <- c("Word", "Count", "Pct%")

    sent_html <- ""
    if (!is.null(rv$text_sentiment)) {
      ts  <- rv$text_sentiment
      net <- ts$n_pos - ts$n_neg
      ovr <- if (net > 0) "Positive" else if (net < 0) "Negative" else "Neutral"
      sent_html <- paste0(
        '<p><strong>Sentiment:</strong> Positive words: ', ts$n_pos,
        ', Negative words: ', ts$n_neg,
        ', Net score: ', net,
        ', Overall: <strong>', ovr, '</strong></p>'
      )
    }

    body <- paste0(
      '<p><strong>Column analysed:</strong> ', .esc(rv$text_col), '</p>',
      '<h3>Top 20 Words (stopwords removed)</h3>',
      .tbl(freq_top),
      sent_html
    )
    sections <- c(sections, .sec("Text Analysis", body))
  }

  # ── 11. ML Benchmark ─────────────────────────────────────────────────────
  if (!is.null(rv$learn_bench) && inc("ml_bench")) {
    best_ln <- if (!is.null(rv$learn_best) && !is.na(rv$learn_best))
      paste0('<p><strong>Best model:</strong> \u2605 ', .esc(rv$learn_best), '</p>') else ""

    sample_html <- ""
    if (!is.null(rv$learn_rows_orig) && !is.null(rv$learn_rows_used)) {
      if (rv$learn_rows_used < rv$learn_rows_orig) {
        sample_html <- paste0(
          '<p><strong>Training rows:</strong> ',
          format(rv$learn_rows_used, big.mark = ","),
          ' rows sampled from ',
          format(rv$learn_rows_orig, big.mark = ","),
          ' (random sample, seed\u00a0=\u00a042)</p>')
      } else {
        sample_html <- paste0(
          '<p><strong>Training rows:</strong> ',
          format(rv$learn_rows_orig, big.mark = ","),
          ' (full dataset \u2014 no sampling applied)</p>')
      }
    }

    body <- paste0(
      '<p><strong>Problem type:</strong> ', .esc(rv$learn_pt),
      ' &nbsp;|&nbsp; <strong>Target:</strong> ', .esc(rv$learn_target), '</p>',
      sample_html,
      best_ln,
      .tbl(rv$learn_bench)
    )
    sections <- c(sections, .sec("ML Benchmark", body))
  }

  # ── 12. ML Model Details ──────────────────────────────────────────────────
  if (!is.null(rv$learn_preds) && length(rv$learn_preds) > 0 && inc("ml_details")) {
    pt     <- rv$learn_pt
    actual <- rv$learn_actual

    detail_parts <- lapply(names(rv$learn_preds), function(algo) {
      preds <- rv$learn_preds[[algo]]
      if (is.null(preds))
        return(paste0('<h3>', .esc(algo), '</h3>',
                      '<p style="color:#888;">No predictions stored.</p>'))

      p <- tryCatch({
        if (identical(pt, "Classification")) {
          lv    <- union(levels(as.factor(actual)), levels(as.factor(preds)))
          cm_df <- as.data.frame(
            table(Predicted = factor(preds,  levels = lv),
                  Actual    = factor(actual, levels = lv)))
          ggplot2::ggplot(cm_df, ggplot2::aes(x = Actual, y = Predicted, fill = Freq)) +
            ggplot2::geom_tile(color = "#111111") +
            ggplot2::geom_text(ggplot2::aes(label = Freq),
                               color = "white", size = 5, fontface = "bold") +
            ggplot2::scale_fill_gradient(low = "#0a1a0a", high = "#00cc44") +
            .dark_theme() +
            ggplot2::labs(title = paste("Confusion Matrix \u2014", algo)) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
        } else {
          a <- as.numeric(actual)
          pred_v <- as.numeric(preds)
          ggplot2::ggplot(data.frame(Actual = a, Predicted = pred_v),
                          ggplot2::aes(x = Actual, y = Predicted)) +
            ggplot2::geom_point(alpha = 0.65, color = "#00ff00") +
            ggplot2::geom_abline(slope = 1, intercept = 0,
                                 color = "white", linetype = "dashed", linewidth = 0.7) +
            .dark_theme() +
            ggplot2::labs(title = paste("Actual vs Predicted \u2014", algo))
        }
      }, error = function(e) NULL)

      img_html <- if (!is.null(p)) .plot_to_img(p, alt = algo, width = 8, height = 5)
                  else paste0('<p style="color:#888;">Chart unavailable for ', .esc(algo), '.</p>')

      paste0('<h3 style="color:#ccc;border-bottom:1px solid #1e1e1e;',
             'padding-bottom:6px;margin:20px 0 10px;">', .esc(algo), '</h3>', img_html)
    })

    detail_label <- if (identical(pt, "Classification")) "Confusion Matrices"
                    else "Actual vs Predicted"
    body <- paste(unlist(detail_parts), collapse = "\n")
    sections <- c(sections, .sec(paste0("ML Model Details \u2014 ", detail_label), body))
  }

  # ── 13. ML Feature Importance ─────────────────────────────────────────────
  if (!is.null(rv$learn_feat_imp) && inc("ml_feat_imp")) {
    fi  <- rv$learn_feat_imp
    # feat_imp is data.frame(Feature, Importance) from ranger
    df  <- fi[order(fi$Importance), ]
    df$Feature <- factor(df$Feature, levels = df$Feature)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = Feature, y = Importance)) +
      ggplot2::geom_bar(stat = "identity", fill = "#00ff00", alpha = 0.85,
                        color = "black") +
      ggplot2::coord_flip() +
      .dark_theme() +
      ggplot2::labs(title = "Feature Importance (Random Forest \u2014 ranger impurity)",
                    x = NULL, y = "Impurity Importance")

    fi_html <- .plot_to_img(p, alt = "Feature Importance", width = 8, height = 5)
    sections <- c(sections, .sec("ML Feature Importance (Random Forest)", fi_html))
  }

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
  dt       <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  file_lb  <- if (!is.null(rv$file_name))
    paste0(" &nbsp;&bull;&nbsp; ", .esc(rv$file_name)) else ""

  paste0(
    '<!DOCTYPE html><html lang="en"><head>',
    '<meta charset="UTF-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1.0">',
    '<title>analyzeR Report</title>',
    '<style>', css, '</style>',
    '</head><body><div class="wrap">',
    '<div class="hdr">',
    '<h1>analyzeR</h1>',
    '<p>Report generated: ', dt, file_lb, '</p>',
    '</div>',
    paste(sections, collapse = "\n"),
    '</div></body></html>'
  )
}
