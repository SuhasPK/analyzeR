# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

analyzeR is a Shiny web application for interactive data analysis. Users upload datasets and perform data exploration through a tabbed interface: LoadR (upload), ReadR (summary stats), CleanR (cleaning), AnalyzeR (statistical analysis), PlotR (visualization), LearnR (ML training & prediction), and RemoveR (reset).

## Running the App

Open the project in RStudio and use the **Run App** button, or run from the R console:

```r
# From the analyzeR/ directory
shiny::runApp()
```

## Architecture

The app uses **Shiny modules** — each tab is a self-contained module with paired `*UI()` and `*Server()` functions.

**Shared state:** `rv <- reactiveValues(data = NULL)` in `server.R` holds the dataset. A `reset_counter <- reactiveVal(0L)` signals LoadR to clear its file input widget.

**Data flow:**
```
loadRServer("load", reset_trigger) → returns dataset reactive
  └─► observe({ rv$data <- raw_dataset() })   # sync to shared state

rv$data ──► readRServer    (read-only: reactive(rv$data))
        ──► analyzeRServer (read-only: reactive(rv$data))
        ──► plotRServer    (read-only: reactive(rv$data))
        ──► learnRServer   (read-only: reactive(rv$data))
        ──► cleanRServer   (read-write: rv)
        ──► removeRServer  (read-write: rv + reset_counter)
```

**Module files** (`analyzeR/modules/`):

| File | Status | Description |
|------|--------|-------------|
| `loadR.R` | Working | File upload (.csv, .xlsx, .tsv, .txt), auto-detects .txt separator, resets on RemoveR trigger |
| `readR.R` | Working | Variable types, numeric stats, categorical frequency table (shown after button click) |
| `cleanR.R` | Working | Remove duplicates, NA strategies (drop/mean/median/mode), drop columns, action log, preview |
| `analyzeR.R` | Working | Correlation heatmap, Shapiro-Wilk normality, IQR outlier detection |
| `plotR.R` | Working | Scatter/Histogram/Bar/Box plots, facet_wrap support, dataset sampling for large data |
| `learnR.R` | Working | ML training, benchmarking, feature importance, predict new observations |
| `removeR.R` | Working | Clears rv$data and increments reset_counter to wipe LoadR UI |

## LearnR Module Details

**Algorithms available by problem type:**

| Problem | Algorithms |
|---------|-----------|
| Regression (numeric target) | Linear Regression, Decision Tree, Random Forest, SVM |
| Classification (categorical target) | Logistic Regression, Decision Tree, Random Forest, KNN (k=5), SVM, Naive Bayes |

**Problem type auto-detection:** based on target variable dtype (numeric → Regression, else → Classification).

**Internal convention:** target column is renamed to `.lr_y` inside training data frames to avoid formula naming conflicts.

**Results tabs:**
- **Benchmark**: comparison table with all metrics, best model highlighted in lime green
- **Model Details**: confusion matrix heatmap (classification) or actual-vs-predicted + residuals (regression)
- **Feature Importance**: bar chart from Random Forest `importance()` output
- **Predict New Row**: dynamic form inputs per feature, "Fill with Test Sample" button, predictions from all trained models side by side

**Metrics:**
- Regression: RMSE, MAE, R²
- Classification: Accuracy, Precision (macro), Recall (macro), F1 (macro)

## Theming

- **Base theme:** `shinytheme("cyborg")` (dark theme)
- **Font:** Ubuntu Mono (loaded from Google Fonts)
- **Accent color:** lime green (`#00ff00`) for active tabs, title, highlights
- **Custom CSS:** `analyzeR/www/custom.css`
- LearnR config panel: `#0e0e0e` background with `#2a2a2a` border

## Test Data

Sample files in `analyzeR/data/` for manual testing:
- `iris_test_csv.csv` — CSV format (use Species as target for classification)
- `iris_test_excel.xlsx` — Excel format
- `tsv_dataset.tsv` — TSV format (large, ~2MB)
- `example_txt.txt` — TXT format

## Dependencies

Install required packages in R:
```r
install.packages(c("shiny", "shinythemes", "DT", "readxl", "tools",
                   "dplyr", "tidyr", "stringr", "tibble", "ggplot2", "ggdark",
                   "rpart", "randomForest", "e1071", "class"))
```
