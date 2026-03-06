# setup.R — Install all analyzeR R package dependencies
#
# Usage (from project root):
#   Rscript setup.R
#
# On Linux, run ./setup-linux.sh first to install system build tools.

message("=== analyzeR v1.1.0: Installing R packages ===")

# Use a fast CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# ── Core (required) ───────────────────────────────────────────────────────────
core <- c(
  "shiny",
  "shinythemes",
  "shinyjs",
  "shinycssloaders",
  "DT",
  "readxl",
  "tools",
  "dplyr",
  "tidyr",
  "stringr",
  "tibble",
  "ggplot2",
  "jsonlite",
  "rpart",
  "e1071",
  "class",
  "tidytext",
  "textdata"
)

to_install <- core[!sapply(core, requireNamespace, quietly = TRUE)]
if (length(to_install) > 0) {
  message("Installing core packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install)
} else {
  message("All core packages already installed.")
}

# ── Random Forest: ranger preferred, randomForest fallback ───────────────────
if (!requireNamespace("ranger", quietly = TRUE)) {
  message("\nAttempting to install ranger (requires C++ build tools)...")
  tryCatch({
    install.packages("ranger")
    message("ranger installed successfully.")
  }, error = function(e) {
    message("ranger installation failed: ", conditionMessage(e))
    message("Falling back to randomForest...")
    if (!requireNamespace("randomForest", quietly = TRUE)) {
      install.packages("randomForest")
      message("randomForest installed.")
    }
    message(
      "\nTo get ranger working on Linux:\n",
      "  1. Run: sudo apt-get install build-essential g++ gfortran\n",
      "  2. Then: install.packages('ranger')\n",
      "  The app works with randomForest in the meantime."
    )
  })
} else {
  message("ranger already installed.")
}

# ── Optional packages ─────────────────────────────────────────────────────────
optional <- list(
  callr    = "async background training (real Stop button)",
  arrow    = "Parquet file support",
  base64enc = "HTML report plot embedding",
  R.utils  = "training timeouts (sync fallback path)"
)

for (pkg in names(optional)) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("\nInstalling optional package: ", pkg, " (", optional[[pkg]], ")")
    tryCatch(
      install.packages(pkg),
      error = function(e) message("  Skipping ", pkg, ": ", conditionMessage(e))
    )
  }
}

# ── Summary ───────────────────────────────────────────────────────────────────
message("\n=== Setup complete ===")
message("ranger available:       ", requireNamespace("ranger",       quietly = TRUE))
message("randomForest available: ", requireNamespace("randomForest", quietly = TRUE))
message("callr available:        ", requireNamespace("callr",        quietly = TRUE))
message("arrow available:        ", requireNamespace("arrow",        quietly = TRUE))
message("\nTo run the app:")
message('  R -e "shiny::runApp(\'analyzeR/\')"')
