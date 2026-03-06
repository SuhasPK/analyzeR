# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

analyzeR is a Shiny web application for interactive data analysis. Users upload datasets and perform data exploration through a tabbed interface: LoadR (upload), ReadR (summary stats), CleanR (cleaning), AnalyzeR (statistical analysis + hypothesis tests), PlotR (visualization), TextR (NLP on text columns), LearnR (ML training & prediction), and RemoveR (reset).

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
        ──► textRServer    (read-only: reactive(rv$data))
        ──► learnRServer   (read-only: reactive(rv$data))
        ──► cleanRServer   (read-write: rv)
        ──► removeRServer  (read-write: rv + reset_counter)
```

**Module files** (`analyzeR/modules/`):

| File | Status | Description |
|------|--------|-------------|
| `loadR.R` | Working | File upload (.csv, .tsv, .txt, .xlsx, .json, .ndjson, .rds, .parquet); auto-detects delimiter (comma/semicolon/tab/space); metadata panel shows rows, cols, types, missing, duplicates, memory |
| `readR.R` | Working | Report-format output: overview cards, variable dictionary with semantic types, extended numeric stats (Q1/Q3/skewness/kurtosis/zeros/unique), categorical stats, null analysis with visual bars |
| `cleanR.R` | Working | Remove duplicates, NA strategies (drop/mean/median/mode), drop columns, action log, preview; **Data Quality tab**: null analysis table + type mismatch recommendations |
| `analyzeR.R` | Working | Correlation heatmap, Shapiro-Wilk normality, IQR outlier detection, Hypothesis Tests sub-tab |
| `plotR.R` | Working | Scatter/Histogram/Bar/Box plots, facet_wrap support, dataset sampling for large data |
| `textR.R` | Working | NLP on text columns: word frequency, bigrams, Bing sentiment analysis |
| `learnR.R` | Working | ML training, benchmarking, feature importance, predict new observations |
| `removeR.R` | Working | Clears rv$data and increments reset_counter to wipe LoadR UI |

## LearnR Module Details

**Algorithms available by problem type:**

| Problem | Algorithms |
|---------|-----------|
| Regression (numeric target) | Linear Regression, Decision Tree, Random Forest, SVM |
| Classification (categorical target) | Logistic Regression, Decision Tree, Random Forest, KNN (k=5), SVM, Naive Bayes |

**Random Forest** uses `ranger` package (not `randomForest`): multithreaded C++, 10-20x faster. Feature importance via `m$variable.importance` (named numeric vector), stored as `data.frame(Feature, Importance)` in `store$feat_imp`.

**Per-algorithm row caps** (`.LR_ALGO_CAPS` in learnR.R): SVM=6K, KNN=20K, RF/DT=200K, linear models=500K. Applied on top of the global cap slider, per-algo at training time.

**Async training** via `callr` (optional package). When installed: three observers — Dispatcher (launches bg process), Collector (polls every 400ms, handles result/stop), Finalizer (wraps up). Stop button kills the background process (`proc$kill()`). When callr not installed: falls back to synchronous queue-based training (old behavior).

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

## TextR Module Details

**Requires:** `tidytext`, `textdata` packages.

**Tabs:**
- **Overview**: character/word/sentence count, unique words, lexical diversity, avg word length, most common word (stopwords removed)
- **Word Frequency**: tokenize → anti-join stopwords → top 30 bar chart + full DT
- **N-grams**: bigrams → filter stopwords → top 20 bar chart + full DT
- **Sentiment**: Bing lexicon via `get_sentiments("bing")` → positive/negative word counts, net score, top-10 chart, matched-words DT

**Reset behaviour:** `observeEvent(reset_trigger(), { show_text(FALSE) })` returns module to initial state.

## AnalyzeR Hypothesis Tests Sub-tab

Fourth tab inside AnalyzeR. Gated by `ran_test <- reactiveVal(FALSE)` (separate from `ran_analysis`).

| Category | Sub-type | R function |
|----------|----------|------------|
| t-test | One-sample | `t.test(x, mu=mu)` |
| t-test | Two-sample (independent) | `t.test(y ~ grp)` |
| t-test | Paired | `t.test(x, y, paired=TRUE)` |
| ANOVA | One-way + Tukey HSD | `aov()` + `TukeyHSD()` |
| Chi-square | Independence | `chisq.test(table(a,b))` |
| Non-parametric | Mann-Whitney U | `wilcox.test(y ~ grp)` |
| Non-parametric | Kruskal-Wallis | `kruskal.test(y ~ grp)` |

Results show: statistic DT, decision badge (green Reject H₀ / grey Fail to reject), interpretation text, plus Tukey pairwise table (ANOVA) or contingency table (Chi-square).

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
                   "jsonlite",
                   "rpart", "randomForest", "e1071", "class",
                   "tidytext", "textdata"))

# Optional — for Parquet file support:
install.packages("arrow")
```

Note: `textdata` is required by `tidytext` to download the Bing sentiment lexicon (`get_sentiments("bing")`). On first use it will prompt to download the dataset.

Note: `jsonlite` is a base dependency of many packages and is usually already installed. `arrow` is optional — if absent, a notification is shown when a `.parquet` file is uploaded.
