app_version <- "1.1.0"

# Raise the file upload limit to 500 MB (default is 5 MB)
options(shiny.maxRequestSize = 500 * 1024^2)

# ── shinycssloaders global defaults ──────────────────────────────────────────
options(spinner.color = "#00ff00", spinner.type = 8, spinner.size = 0.7)

# ── Core packages ─────────────────────────────────────────────────────────────
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(DT)
library(readxl)
library(tools)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)

# ggdark is unavailable on R 4.5+; replicate dark_theme_minimal() with base ggplot2
.dark_theme <- function() {
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.background   = ggplot2::element_rect(fill = "#1a1a1a", color = NA),
    panel.background  = ggplot2::element_rect(fill = "#1a1a1a", color = NA),
    panel.grid.major  = ggplot2::element_line(color = "#2e2e2e"),
    panel.grid.minor  = ggplot2::element_line(color = "#252525"),
    text              = ggplot2::element_text(color = "#dddddd"),
    axis.text         = ggplot2::element_text(color = "#aaaaaa"),
    axis.title        = ggplot2::element_text(color = "#cccccc"),
    plot.title        = ggplot2::element_text(color = "#ffffff", face = "bold"),
    legend.background = ggplot2::element_rect(fill = "#1a1a1a", color = NA),
    legend.text       = ggplot2::element_text(color = "#dddddd"),
    legend.title      = ggplot2::element_text(color = "#dddddd"),
    strip.background  = ggplot2::element_rect(fill = "#252525", color = NA),
    strip.text        = ggplot2::element_text(color = "#cccccc")
  )
}

# JSON/NDJSON support
library(jsonlite)
# jsonlite exports its own validate() which masks shiny::validate() —
# restore the Shiny version immediately so modules can use validate()/need() normally.
validate <- shiny::validate

# Parquet support (install if missing):
#   install.packages("arrow")
.arrow_available  <- requireNamespace("arrow",    quietly = TRUE)
if (.arrow_available) library(arrow)

# base64enc — used by reportR to embed plots in the HTML report.
# Usually already installed (it is a dependency of knitr).
#   install.packages("base64enc")
.base64_available <- requireNamespace("base64enc", quietly = TRUE)
if (.base64_available) library(base64enc)

# R.utils — optional, used by learnR for per-model training timeouts (sync path only).
#   install.packages("R.utils")
.rutils_available <- requireNamespace("R.utils", quietly = TRUE)

# callr — optional, used by learnR for true async background training with real Stop.
#   install.packages("callr")
.callr_available <- requireNamespace("callr", quietly = TRUE)

# ML packages (install if missing):
#   install.packages(c("rpart", "e1071", "class"))
#   ranger (fast RF) — preferred but requires C++ build tools on Linux.
#   Falls back to randomForest automatically when ranger is unavailable.
#   To install ranger on Linux: sudo apt-get install build-essential g++ gfortran
#   then: install.packages("ranger")
library(rpart)
library(e1071)
library(class)
# nnet — multinomial logistic regression for multi-class classification in learnR
library(nnet)

# ranger (fast RF) vs randomForest (fallback) — chosen at startup
.ranger_available <- requireNamespace("ranger",       quietly = TRUE)
.rf_available     <- requireNamespace("randomForest", quietly = TRUE)
if (.ranger_available) {
  library(ranger)
} else if (.rf_available) {
  library(randomForest)
  message("[analyzeR] ranger not found — using randomForest (slower). ",
          "See setup-linux.sh to install build tools, then: install.packages('ranger')")
} else {
  message("[analyzeR] Neither ranger nor randomForest found. ",
          "Random Forest will be unavailable. Run: install.packages('randomForest')")
}

# NLP packages (install if missing):
#   install.packages(c("tidytext", "textdata"))
library(tidytext)
library(textdata)


# ── Stat helpers ──────────────────────────────────────────────────────────────

# Sample skewness (Fisher's moment coefficient)
col_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  s <- sd(x)
  if (s == 0) return(NA_real_)
  sum(((x - mean(x)) / s)^3) / n
}

# Sample excess kurtosis
col_kurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4) return(NA_real_)
  s <- sd(x)
  if (s == 0) return(NA_real_)
  sum(((x - mean(x)) / s)^4) / n - 3
}

# Infer a human-readable "semantic type" for a column
infer_semantic_type <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return("Datetime")
  if (inherits(x, "Date"))    return("Date")
  if (is.logical(x))          return("Binary (logical)")

  n_non_na <- sum(!is.na(x))
  n_unique <- length(unique(x[!is.na(x)]))

  if (is.factor(x)) {
    if (n_unique == 2) return("Binary (factor)")
    return("Categorical (factor)")
  }

  if (is.numeric(x)) {
    if (n_unique == 2)                                   return("Binary")
    if (n_non_na > 20 && n_unique / n_non_na > 0.95)    return("ID / Key")
    if (all(x[!is.na(x)] == floor(x[!is.na(x)])))       return("Integer")
    return("Continuous")
  }

  if (is.character(x)) {
    if (n_non_na > 20 && n_unique / n_non_na > 0.95)    return("ID / Key")
    if (n_unique <= 10)                                   return("Categorical")
    num_conv <- suppressWarnings(as.numeric(x[!is.na(x)]))
    if (mean(!is.na(num_conv)) > 0.9)                    return("Numeric (as text)")
    return("Free Text")
  }

  class(x)[1]
}

# Detect best delimiter for a delimited file
detect_sep <- function(path) {
  lines <- tryCatch(readLines(path, n = 5, warn = FALSE), error = function(e) character(0))
  if (length(lines) == 0) return(",")
  text <- paste(lines, collapse = "\n")
  counts <- c(
    ","  = nchar(gsub("[^,]",  "", text)),
    ";"  = nchar(gsub("[^;]",  "", text)),
    "\t" = nchar(gsub("[^\t]", "", text))
  )
  sep <- names(which.max(counts))
  if (counts[sep] == 0) return(" ")
  sep
}

# Custom summary function (legacy — kept for compatibility)
custom_summary <- function(df) {
  data.frame(
    Min       = sapply(df, function(x) if (is.numeric(x)) min(x, na.rm = TRUE) else NA),
    `1st Qu.` = sapply(df, function(x) if (is.numeric(x)) quantile(x, 0.25, na.rm = TRUE) else NA),
    Median    = sapply(df, function(x) if (is.numeric(x)) median(x, na.rm = TRUE) else NA),
    Mean      = sapply(df, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else NA),
    `3rd Qu.` = sapply(df, function(x) if (is.numeric(x)) quantile(x, 0.75, na.rm = TRUE) else NA),
    Max       = sapply(df, function(x) if (is.numeric(x)) max(x, na.rm = TRUE) else NA),
    `Null Count` = sapply(df, function(x) sum(is.na(x))),
    check.names = FALSE,
    row.names = names(df)
  )
}


# ── Null-coalescing operator (used across multiple modules) ───────────────────
`%||%` <- function(x, y) if (is.null(x)) y else x


# ── MonitR helpers ────────────────────────────────────────────────────────────

# Estimate current R heap usage in MB
.monitr_mem_mb <- function() {
  x <- gc(verbose = FALSE, reset = FALSE)
  round((x["Ncells", "used"] * 56 + x["Vcells", "used"] * 8) / 1024^2, 1)
}

# Append an entry to the shared monitor_rv log (max 15 entries)
.monitr_log <- function(monitor_rv, op, rows = NULL, secs = NULL, note = "") {
  if (is.null(monitor_rv)) return(invisible(NULL))
  entry   <- list(op = op, rows = rows, secs = secs, note = note, ts = Sys.time())
  new_log <- c(monitor_rv$log, list(entry))
  if (length(new_log) > 15) new_log <- tail(new_log, 15)
  monitor_rv$log <- new_log
}


# ── Shared UI helpers ─────────────────────────────────────────────────────────

# Standard "no dataset loaded" prompt — shown in every analysis tab until data
# is uploaded in LoadR.
.no_data_ui <- function() {
  div(
    class = "no-data-prompt",
    div(class = "no-data-icon", "\u2191"),
    tags$h4("No dataset loaded"),
    tags$p(
      "Go to the ",
      tags$strong("LoadR"),
      " tab and upload a file to get started."
    )
  )
}


# Source module files
source("modules/homeR.R")
source("modules/loadR.R")
source("modules/readR.R")
source("modules/cleanR.R")
source("modules/analyzeR.R")
source("modules/plotR.R")
source("modules/textR.R")
source("modules/learnR.R")
source("modules/reportR.R")
source("modules/removeR.R")
