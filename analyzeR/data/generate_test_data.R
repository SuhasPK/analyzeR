# generate_test_data.R
# Generates a ~300k-row synthetic HR dataset with ~15% dirty data
# for smoke-testing all analyzeR features.
#
# Columns (10):
#   employee_id       — integer ID key        → ReadR: ID/Key detection
#   age               — numeric, outliers+NAs  → CleanR, AnalyzeR outliers
#   annual_salary     — numeric, regression tgt → LearnR regression
#   department        — 6-level categorical     → PlotR, AnalyzeR ANOVA
#   region            — 16-level categorical    → PlotR >15 filter (selectInput path)
#   tenure_years      — integer, right-skewed   → AnalyzeR normality
#   performance_score — numeric 0–100           → AnalyzeR, PlotR scatter
#   satisfaction      — 4-level categorical     → LearnR multi-class classification
#   work_notes        — free prose text         → TextR NLP/sentiment
#   attrition         — binary Yes/No           → LearnR binary classification
#
# Dirty data (~15% of cells):
#   ~9%  random NAs per column
#   ~3%  duplicate rows
#   ~2%  outlier / impossible numeric values
#   ~1%  garbled strings in categorical columns
#
# Usage (from project root):
#   source("analyzeR/data/generate_test_data.R")
# Or run in RStudio with working directory set to project root.

message("=== analyzeR test data generator ===")

set.seed(42)
N <- 291000L   # base rows; ~9k duplicates added later → ~300k total

# ── Output path (auto-detect) ─────────────────────────────────────────────────
out_dir <- if (dir.exists("analyzeR/data")) "analyzeR/data" else
           if (dir.exists("data"))          "data"          else "."
out_path <- file.path(out_dir, "test_hr_300k.csv")

# ── Categorical pools ─────────────────────────────────────────────────────────
departments <- c("Sales", "Engineering", "HR", "Finance", "Marketing", "Operations")

# 16 unique values → triggers the selectInput(selectize=FALSE) path in plotR
regions <- c(
  "Northeast", "Southeast", "Midwest", "Southwest", "West", "Northwest",
  "Central", "Mountain", "Pacific", "Atlantic", "Great Plains", "Mid-Atlantic",
  "New England", "Deep South", "Gulf Coast", "Upper Midwest"
)

# ── Text phrase pools for work_notes (TextR sentiment coverage) ───────────────
pos_phrases <- c(
  "excellent team collaboration and strong support from management",
  "very happy with the work environment and outstanding growth opportunities",
  "superb benefits package and flexible working arrangements for everyone",
  "love the innovative projects and creative freedom given to employees",
  "inspiring leadership with clear career progression paths available",
  "fantastic work life balance and supportive supervisors throughout",
  "great colleagues and a genuinely positive inclusive workplace culture",
  "wonderful training programs and professional development resources provided"
)

neg_phrases <- c(
  "poor management decisions are causing serious frustration among staff",
  "stressful deadlines with inadequate resources and insufficient support",
  "toxic workplace culture and terrible lack of transparency from leadership",
  "deeply disappointed with low compensation and no recognition for hard work",
  "excessive workload and burnout are serious ongoing concerns for the team",
  "suffocating micromanagement is destroying creativity and morale",
  "awful communication breakdown between departments hurts productivity badly",
  "unfair performance reviews and biased promotion decisions undermine trust"
)

neu_phrases <- c(
  "standard office procedures and regular weekly team meetings as scheduled",
  "typical workload with routine tasks and quarterly performance reviews",
  "average benefits and normal working hours consistent with industry norms",
  "usual onboarding process completed without any notable issues encountered",
  "standard project workflow followed according to established company policy",
  "regular training sessions attended as per the department annual schedule",
  "normal performance cycle with scheduled monthly check-in meetings held",
  "ordinary day to day responsibilities and standard administrative duties"
)

all_phrases <- c(pos_phrases, neg_phrases, neu_phrases)

# ── Base columns ──────────────────────────────────────────────────────────────
message("Generating ", format(N, big.mark = ","), " base rows...")

dept_vec    <- sample(departments, N, replace = TRUE)
region_vec  <- sample(regions,     N, replace = TRUE)
tenure_raw  <- pmax(0L, round(rexp(N, rate = 0.12)))   # right-skewed, 0–~40 yrs

# Salary: correlated with department + tenure
base_sal <- c(
  Sales = 60000, Engineering = 90000, HR = 52000,
  Finance = 80000, Marketing = 65000, Operations = 55000
)[dept_vec]
annual_salary <- round(base_sal + tenure_raw * 1600 + rnorm(N, 0, 13000))

# Age: correlated with tenure
age_vec <- round(pmax(18, pmin(72, 24 + tenure_raw + rnorm(N, 6, 4))))

# Performance score: department-biased, clipped to 0–100
perf_base <- c(
  Sales = 68, Engineering = 73, HR = 64,
  Finance = 70, Marketing = 67, Operations = 65
)[dept_vec]
performance_score <- round(pmin(100, pmax(0, perf_base + rnorm(N, 0, 15))), 1)

# Satisfaction: salary ratio drives level
sal_ratio    <- annual_salary / base_sal
satisfaction <- ifelse(
  sal_ratio > 1.15,
  sample(c("High", "Very High"), N, replace = TRUE, prob = c(0.45, 0.55)),
  ifelse(
    sal_ratio > 1.0,
    sample(c("Medium", "High"),  N, replace = TRUE, prob = c(0.50, 0.50)),
    sample(c("Low",    "Medium"), N, replace = TRUE, prob = c(0.55, 0.45))
  )
)

# Attrition: driven by satisfaction + low tenure
p_leave <- ifelse(satisfaction == "Low",      0.42,
           ifelse(satisfaction == "Medium",   0.18,
           ifelse(satisfaction == "High",     0.07, 0.03)))
p_leave   <- p_leave + ifelse(tenure_raw < 2, 0.10, 0.0)
attrition <- ifelse(runif(N) < p_leave, "Yes", "No")

# Work notes: mix of single and double phrases for richer NLP signal
note_a     <- sample(all_phrases, N, replace = TRUE)
note_b     <- sample(all_phrases, N, replace = TRUE)
work_notes <- ifelse(runif(N) > 0.45, note_a, paste0(note_a, ". ", note_b))

df <- data.frame(
  employee_id       = seq_len(N),
  age               = age_vec,
  annual_salary     = annual_salary,
  department        = dept_vec,
  region            = region_vec,
  tenure_years      = tenure_raw,
  performance_score = performance_score,
  satisfaction      = satisfaction,
  work_notes        = work_notes,
  attrition         = attrition,
  stringsAsFactors  = FALSE
)

# ── Introduce dirty data ──────────────────────────────────────────────────────
message("Injecting dirty data...")

dirty_idx <- function(frac) sample(seq_len(N), floor(N * frac))

# Random NAs (~9% of cells total)
df$age[dirty_idx(0.07)]               <- NA
df$annual_salary[dirty_idx(0.07)]     <- NA
df$department[dirty_idx(0.05)]        <- NA
df$region[dirty_idx(0.05)]            <- NA
df$tenure_years[dirty_idx(0.07)]      <- NA
df$performance_score[dirty_idx(0.08)] <- NA
df$satisfaction[dirty_idx(0.05)]      <- NA
df$work_notes[dirty_idx(0.04)]        <- NA
df$attrition[dirty_idx(0.04)]         <- NA

# Outlier / impossible numeric values (~2.5% of cells)
df$age[dirty_idx(0.01)]                <- sample(c(-5, 0, 135, 200, 999),
                                                  floor(N * 0.01), replace = TRUE)
df$annual_salary[dirty_idx(0.01)]      <- sample(c(-10000, 0, 3500000, 9999999),
                                                  floor(N * 0.01), replace = TRUE)
df$performance_score[dirty_idx(0.005)] <- sample(c(-30, 105, 999, -999),
                                                  floor(N * 0.005), replace = TRUE)

# Garbled strings in categorical columns (~1%)
df$department[dirty_idx(0.01)] <- sample(c("N/A", "#REF!", "unknown", "---", ""),
                                          floor(N * 0.01), replace = TRUE)
df$region[dirty_idx(0.01)]     <- sample(c("NULL", "n/a", "?", "MISSING"),
                                          floor(N * 0.01), replace = TRUE)

# ── Append duplicate rows (~3%) ───────────────────────────────────────────────
n_dups   <- floor(N * 0.03)
dup_rows <- df[sample(seq_len(N), n_dups, replace = FALSE), ]
df       <- rbind(df, dup_rows)
df       <- df[sample(nrow(df)), ]
rownames(df) <- NULL

# ── Summary ───────────────────────────────────────────────────────────────────
total_cells <- nrow(df) * ncol(df)
na_cells    <- sum(is.na(df))
message(sprintf("Final: %s rows x %d cols  |  NAs: %s (%.1f%%)  |  Duplicates: ~%s",
  format(nrow(df), big.mark = ","),
  ncol(df),
  format(na_cells, big.mark = ","),
  100 * na_cells / total_cells,
  format(n_dups, big.mark = ",")))

message("Columns:")
for (col in names(df)) {
  n_na  <- sum(is.na(df[[col]]))
  dtype <- class(df[[col]])[1]
  n_uniq <- length(unique(df[[col]][!is.na(df[[col]])]))
  message(sprintf("  %-20s  %-10s  %s unique  %s NAs",
    col, dtype, format(n_uniq, big.mark = ","), format(n_na, big.mark = ",")))
}

# ── Save ──────────────────────────────────────────────────────────────────────
message("\nSaving to: ", out_path)
write.csv(df, out_path, row.names = FALSE, na = "")
message("Done. File size: ", round(file.size(out_path) / 1024^2, 1), " MB")
message("\nLoad in analyzeR via LoadR tab → upload test_hr_300k.csv")
message("Suggested targets:")
message("  Classification (binary)     : attrition   (Yes / No)")
message("  Classification (multi-class): satisfaction (4 levels)")
message("  Regression                  : annual_salary or performance_score")
message("  TextR column                : work_notes")
