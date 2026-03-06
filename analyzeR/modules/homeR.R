# ─────────────────────────────────────────────────────────────────────────────
# homeR.R  —  Landing / Home page (static, no server logic)
# ─────────────────────────────────────────────────────────────────────────────

# ── Small helpers ──────────────────────────────────────────────────────────────

.home_section <- function(...) {
  div(style = paste0(
        "border-left: 3px solid #00ff00; padding-left: 18px; ",
        "margin-bottom: 40px;"),
    ...)
}

.module_card <- function(name, icon_char, description) {
  div(style = paste0(
        "background: #111; border: 1px solid #222; border-radius: 6px; ",
        "padding: 14px 16px; margin-bottom: 10px;"),
    tags$span(style = "color:#00ff00; font-weight:bold; font-size:15px;",
              icon_char, " ", name),
    tags$p(style = "margin: 6px 0 0; color:#e0e0e0; font-size:14px;",
           description)
  )
}

.limit_item <- function(title, detail) {
  div(style = "margin-bottom: 12px;",
    tags$span(style = "color:#ffaa00; font-size:14px;", "\u26a0 "),
    tags$span(style = "color:#f0f0f0; font-size:15px; font-weight:bold;", title),
    tags$p(style = "margin: 4px 0 0 22px; color:#c8c8c8; font-size:14px;", detail)
  )
}

# ── GitHub SVG mark ────────────────────────────────────────────────────────────
.github_svg <- HTML(paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" width="20" height="20" ',
  'viewBox="0 0 24 24" fill="#e8e8e8" style="vertical-align:middle;',
  'margin-right:8px;">',
  '<path d="M12 0C5.374 0 0 5.373 0 12c0 5.302 3.438 9.8 8.207 ',
  '11.387.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416',
  '-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083',
  '-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 ',
  '2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305',
  '-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124',
  '-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23A11.509 ',
  '11.509 0 0112 5.803c1.02.005 2.047.138 3.006.404 2.291-1.552 ',
  '3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 ',
  '1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372',
  '.823 1.102.823 2.222v3.293c0 .319.192.694.801.576C20.566 21.797',
  ' 24 17.3 24 12c0-6.627-5.373-12-12-12z"/>',
  '</svg>'))

# ── Module UI ──────────────────────────────────────────────────────────────────
homeRUI <- function(id) {
  ns <- NS(id)
  div(style = "max-width: 860px; margin: 10px auto 40px; padding: 0 8px;",

    # ── Description ─────────────────────────────────────────────────────────
    .home_section(
      tags$h3(style = "color:#00ff00; font-size:18px; margin:0 0 14px;
                        letter-spacing:1px;",
              "About analyzeR"),
      tags$p(style = "font-size:15px; color:#e8e8e8; line-height:1.7;",
        "analyzeR is an interactive, no-code data analysis web application built with ",
        tags$strong("R Shiny"),
        ". Upload a tabular dataset and work through a full analysis pipeline \u2014 ",
        "descriptive statistics, data cleaning, visualisation, statistical hypothesis testing, ",
        "NLP on text columns, machine learning, and session reporting \u2014 without writing ",
        "a single line of code."
      ),
      tags$p(style = "font-size:15px; color:#e8e8e8; line-height:1.7; margin-top:10px;",
        "Built on a terminal-inspired dark theme. Supports ",
        tags$strong(".csv, .tsv, .txt, .xlsx, .json, .ndjson, .rds"),
        " and ", tags$strong(".parquet"),
        " file formats up to 500\u202fMB."
      ),
      div(style = paste0(
            "display:inline-block; background:#001400; border:1px solid #003a14; ",
            "border-radius:4px; padding:4px 14px; margin-top:8px; font-size:12px; ",
            "color:#00cc44; letter-spacing:0.5px;"),
          "v", app_version)
    ),

    # ── Author ───────────────────────────────────────────────────────────────
    .home_section(
      tags$h3(style = "color:#00ff00; font-size:18px; margin:0 0 14px;
                        letter-spacing:1px;",
              "Author"),
      tags$p(style = "font-size:15px; color:#e8e8e8; margin-bottom:12px;",
             tags$strong("Suhas P K")),
      tags$a(
        href   = "https://github.com/SuhasPK/analyzeR/issues",
        target = "_blank",
        style  = paste0(
          "display:inline-flex; align-items:center; ",
          "background:#161616; border:1px solid #333; border-radius:5px; ",
          "padding:8px 16px; color:#e8e8e8; font-size:14px; ",
          "text-decoration:none;"),
        .github_svg,
        "Report a bug or contribute on GitHub"
      )
    ),

    # ── Instructions ─────────────────────────────────────────────────────────
    .home_section(
      tags$h3(style = "color:#00ff00; font-size:18px; margin:0 0 16px;
                        letter-spacing:1px;",
              "How to Use"),
      tags$p(style = "font-size:14px; color:#aaa; margin-bottom:14px;",
             "Work through the tabs from left to right. Each tab is independent \u2014 ",
             "you can jump to any tab after loading data."),

      .module_card("LoadR", "\u2191",
        paste0("Start here. Upload a dataset in any supported format: ",
               ".csv, .tsv, .txt, .xlsx, .json, .ndjson, .rds, or .parquet. ",
               "Delimiter is auto-detected for flat files. A metadata panel and ",
               "10-row preview appear immediately after the file is parsed.")),

      .module_card("ReadR", "\u2248",
        paste0("Full dataset report generated on demand. A single-pass computation ",
               "produces four tabs: Overview (row/col counts, missing rate, duplicates, memory), ",
               "Variables (R type, semantic type, completeness per column), ",
               "Numeric Stats (min, Q1, median, mean, Q3, max, SD, skewness), ",
               "and Missing (per-column missing counts sorted by severity).")),

      .module_card("CleanR", "\u2714",
        paste0("Clean your dataset interactively. Remove duplicate rows, handle missing ",
               "values (drop rows, fill with mean / median / mode), and drop unwanted columns. ",
               "The Data Quality tab shows a null-analysis table and type-mismatch recommendations. ",
               "Every action is timestamped in the action log.")),

      .module_card("AnalyzeR", "\u223f",
        paste0("Four analysis tabs: Correlation heatmap (Pearson, pairwise complete), ",
               "Shapiro-Wilk normality tests, IQR-based outlier detection, and Hypothesis Tests. ",
               "Supported tests: one-sample / two-sample / paired t-test, one-way ANOVA + Tukey HSD, ",
               "Chi-square independence, Mann-Whitney U, and Kruskal-Wallis. ",
               "Grouping columns are auto-detected from any column with 2\u201320 unique values, ",
               "including numeric-coded groups.")),

      .module_card("PlotR", "\u25a0",
        paste0("Create scatter plots, histograms, bar charts, and box plots. ",
               "Supports facet wrapping with fixed, free-x, free-y, or free scales. ",
               "Datasets with more than 1\u202f000 rows show a sampling slider to keep ",
               "rendering fast. All plots use the dark ggplot theme.")),

      .module_card("TextR", "\u00b6",
        paste0("Natural language analysis on any text or factor column. ",
               "Tabs: Overview (character / word / sentence counts, lexical diversity), ",
               "Word Frequency (top-30 bar chart, stopwords removed), ",
               "N-grams (top-20 bigrams), and Sentiment (Bing lexicon \u2014 positive / negative ",
               "word counts, net score, top-10 chart). Requires the tidytext and textdata packages. ",
               "Large datasets are automatically capped at 50\u202f000 rows before tokenisation ",
               "to prevent the session from hanging.")),

      .module_card("LearnR", "\u2605",
        paste0("Train and benchmark multiple ML models with built-in safeguards. ",
               "Regression: Linear Regression, Decision Tree, Random Forest, SVM. ",
               "Classification: Logistic Regression, Decision Tree, Random Forest, ",
               "KNN (k=5), SVM, Naive Bayes. ",
               "A Sample Cap slider (10\u202f000 \u2013 200\u202f000 rows, default 50\u202f000) ",
               "limits training data on large datasets \u2014 sampling is reproducible (seed\u00a042). ",
               "Models train one at a time via a reactive queue \u2014 a live spinner shows the ",
               "current model and a status log (", "\u2713", " / \u23F1 / \u2717) ",
               "tracks every result with timing. ",
               "A Stop Training button cancels remaining models and shows results for what completed. ",
               "Each model has a per-algorithm timeout (SVM: 90s, Random Forest: 120s, others: 30s). ",
               "On Linux / hosted deployments timeouts are enforced at the C level; on Windows they ",
               "fire only after the model finishes (the elapsed time is always shown accurately). ",
               "All model calls are wrapped in a failsafe so a single failure never aborts the run. ",
               "Results: benchmark table with Status column, confusion matrix or actual-vs-predicted, ",
               "Random Forest feature importance, and a prediction form for new observations.")),

      .module_card("ReportR", "\u2913",
        paste0("Generate a complete session report before resetting. ",
               "Captures: dataset overview, variable summary, numeric statistics, ",
               "missing value analysis, cleaning log, normality and outlier tables, ",
               "all hypothesis tests run in the session (each with its own visualisation \u2014 ",
               "density, box plot, or bar chart depending on test type), ",
               "plots from PlotR (embedded images), text analysis summary, ",
               "and full ML benchmark with model-detail charts. ",
               "Use the nested selectors to choose exactly which plots and hypothesis tests to include. ",
               "Downloads as a self-contained HTML file. ",
               "Open in any browser and use Ctrl+P \u2192 Save as PDF to export a PDF.")),

      .module_card("RemoveR", "\u2715",
        paste0("Reset the session. Shows a confirmation dialog before acting. ",
               "On confirmation, the Shiny session is fully reloaded \u2014 all tabs, ",
               "outputs, and in-memory data are wiped completely. ",
               "Download your report from ReportR before resetting.")),

      div(style = paste0(
            "background: #0a0a0a; border: 1px solid #1e4d1e; border-radius: 6px; ",
            "padding: 14px 16px; margin-bottom: 10px;"),
        tags$span(style = "color:#00ff00; font-weight:bold; font-size:15px;",
                  "\u26a1 MonitR (persistent widget)"),
        tags$p(style = "margin: 6px 0 0; color:#e0e0e0; font-size:14px;",
          paste0("A fixed bottom bar visible on every tab. Shows live R heap memory, ",
                 "dataset size in memory, and the last operation with its wall-clock duration. ",
                 "Click the arrow button to expand a scrollable log of the last 15 operations ",
                 "(Load, Clean, Train, etc.) with row count, elapsed time, and timestamp. ",
                 "Memory is polled every 3 seconds automatically."))
      )
    ),

    # ── Known Limitations ────────────────────────────────────────────────────
    .home_section(
      tags$h3(style = "color:#00ff00; font-size:18px; margin:0 0 16px;
                        letter-spacing:1px;",
              "Known Limitations \u2014 Needs Improvement"),

      .limit_item(
        "No cleaned-data export",
        paste0("The cleaned or transformed dataset cannot be downloaded directly as CSV or Excel. ",
               "Use ReportR to download a session report; direct data export is planned for a future version.")),

      .limit_item(
        "English-only NLP",
        paste0("TextR uses the Bing sentiment lexicon, which covers English only. ",
               "Multilingual sentiment analysis is not yet supported.")),

      .limit_item(
        "No time-series or deep learning",
        paste0("LearnR offers classical ML algorithms only. ",
               "Time-series models (ARIMA, Prophet) and neural networks are not available. ",
               "Hyperparameter tuning (grid search, cross-validation) is also not yet supported.")),

      .limit_item(
        "No multiple-comparison correction",
        paste0("AnalyzeR does not apply Bonferroni or FDR corrections when running ",
               "multiple hypothesis tests in the same session.")),

      .limit_item(
        "Static charts only",
        paste0("PlotR produces static ggplot images. Interactive charts with zoom, ",
               "tooltips, and brushing (via plotly or echarts4r) are not yet implemented.")),

      .limit_item(
        "Single dataset per session",
        paste0("Only one file can be loaded at a time. ",
               "Joining or merging multiple datasets is not supported.")),

      .limit_item(
        "Stop Training acts between models, not mid-model",
        paste0("LearnR's Stop Training button takes effect after the current model finishes, ",
               "not during it. R is single-threaded, so a model running in R cannot be ",
               "interrupted by a UI event. Per-model timeouts (via R.utils::withTimeout) are the ",
               "safeguard against a model hanging indefinitely. ",
               "On Linux / hosted deployments (Posit Connect, shinyapps.io), ",
               "timeouts are enforced at the C level and will cut off a model mid-run. ",
               "On Windows, R cannot interrupt C-level code via signals, so the model runs to ",
               "completion even if the configured timeout is exceeded \u2014 the status log shows ",
               "both the configured limit and the actual elapsed time so you always know what happened. ",
               "Install R.utils for timeout support; without it timeouts are skipped but all ",
               "other features still work.")),

      .limit_item(
        "Performance on very large data",
        paste0("Datasets with more than 500\u202f000 rows may cause slow rendering or high memory use, ",
               "particularly in CleanR. PlotR mitigates this with row sampling. ",
               "LearnR now has per-model timeouts to prevent indefinite hangs, ",
               "but very large datasets will still make SVM and Random Forest slow.")),

      .limit_item(
        "Basic cleaning only",
        paste0("CleanR covers common NA strategies but does not support ",
               "regex-based cleaning, type coercion, column renaming, or value replacement."))
    )
  )
}
