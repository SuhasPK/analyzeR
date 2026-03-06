# ─────────────────────────────────────────────────────────────────────────────
# learnR.R  —  ML Training, Benchmarking & Prediction module
# v2: ranger (fast RF), callr async background training, per-algorithm row caps
# ─────────────────────────────────────────────────────────────────────────────

.LR_TARGET <- ".lr_y"

# Per-algorithm hard row caps. Training data above these limits gets subsampled
# automatically *per algorithm* on top of the user's global cap slider.
#
#   SVM (RBF kernel) : O(n^2) memory, O(n^3) training — dangerous above 5K
#   KNN (k=5)        : O(n) prediction cost per test row — slow above 20K
#   ranger/RF        : multithreaded C++, fine at 200K
#   Linear models    : near-linear in n, no practical cap
.LR_ALGO_CAPS <- c(
  "Linear Regression"   = 500000L,
  "Logistic Regression" = 200000L,
  "Decision Tree"       = 200000L,
  "Random Forest"       = 200000L,
  "KNN (k=5)"           =  20000L,
  "SVM"                 =   6000L,
  "Naive Bayes"         = 500000L
)

# Row count above which a yellow "slow" warning appears in the algo selector
.LR_ALGO_WARN <- c(
  "KNN (k=5)" = 8000L,
  "SVM"       = 2000L
)

.LR_MESSAGES <- list(
  "Linear Regression"   = "Fitting OLS \u2014 solving normal equations...",
  "Logistic Regression" = "Fitting logistic model \u2014 iterative reweighted LS...",
  "Decision Tree"       = "Growing decision tree \u2014 evaluating splits...",
  "Random Forest"       = "Growing 100 trees \u2014 parallel bootstrapping (ranger)...",
  "KNN (k=5)"           = "Computing pairwise distances (k = 5)...",
  "SVM"                 = "Running QP solver \u2014 building radial kernel matrix...",
  "Naive Bayes"         = "Estimating class priors and likelihoods..."
)


# ── Helpers ───────────────────────────────────────────────────────────────────

.align_factors <- function(xdf, tdf) {
  for (col in names(xdf)) {
    if (is.factor(tdf[[col]]))
      xdf[[col]] <- factor(xdf[[col]], levels = levels(tdf[[col]]))
  }
  xdf
}

.calc_metrics <- function(preds, actual, pt, secs) {
  tryCatch({
    if (pt == "Regression") {
      a  <- as.numeric(actual); p <- as.numeric(preds)
      st <- sum((a - mean(a, na.rm = TRUE))^2, na.rm = TRUE)
      list(RMSE = round(sqrt(mean((a-p)^2, na.rm=TRUE)), 4),
           MAE  = round(mean(abs(a-p), na.rm=TRUE), 4),
           R2   = round(if (st==0) 0 else max(0, 1-sum((a-p)^2)/st), 4),
           Time_s = secs, cm = NULL, err = NULL)
    } else {
      lv <- union(levels(as.factor(actual)), levels(as.factor(preds)))
      af <- factor(actual, levels=lv); pf <- factor(preds, levels=lv)
      cm <- table(Predicted=pf, Actual=af)
      ac <- sum(diag(cm))/sum(cm)
      pr <- mean(diag(cm)/pmax(rowSums(cm),1), na.rm=TRUE)
      re <- mean(diag(cm)/pmax(colSums(cm),1), na.rm=TRUE)
      f1 <- if ((pr+re)==0) 0 else 2*pr*re/(pr+re)
      list(Accuracy=round(ac,4), Precision=round(pr,4),
           Recall=round(re,4), F1=round(f1,4),
           Time_s=secs, cm=cm, err=NULL)
    }
  }, error = function(e) list(err = conditionMessage(e)))
}


# ── Background worker ─────────────────────────────────────────────────────────
# Runs inside a callr subprocess (or directly in the sync fallback path).
# MUST be fully self-contained: no references to outer scope, loads its own pkgs.
.learnR_bg_worker <- function(algo, tdf, xdf, feat_cols, pt, lr_target = ".lr_y") {
  suppressPackageStartupMessages({
    library(rpart)
    library(ranger)
    library(e1071)
    library(class)
  })

  # Local copy — can't reference outer scope in subprocess
  align_factors_local <- function(xdf, tdf) {
    for (col in names(xdf)) {
      if (is.factor(tdf[[col]]))
        xdf[[col]] <- factor(xdf[[col]], levels = levels(tdf[[col]]))
    }
    xdf
  }

  t0 <- proc.time()["elapsed"]
  m <- NULL; preds <- NULL; feat_imp <- NULL

  if (algo == "Linear Regression") {
    m     <- lm(as.formula(paste0("`", lr_target, "` ~ .")), data = tdf)
    preds <- as.numeric(predict(m, newdata = xdf))

  } else if (algo == "Logistic Regression") {
    nl <- nlevels(tdf[[lr_target]])
    if (nl != 2L) stop(sprintf("Binary only \u2014 dataset has %d classes.", nl))
    m    <- glm(as.formula(paste0("`", lr_target, "` ~ .")),
                data = tdf, family = binomial())
    prob <- predict(m, newdata = xdf, type = "response")
    lvl  <- levels(tdf[[lr_target]])
    preds <- factor(ifelse(prob > 0.5, lvl[2], lvl[1]), levels = lvl)

  } else if (algo == "Decision Tree") {
    mth   <- if (pt == "Regression") "anova" else "class"
    m     <- rpart::rpart(
      as.formula(paste0("`", lr_target, "` ~ .")), data = tdf, method = mth,
      control = rpart::rpart.control(cp = 0.01, minsplit = 5L))
    preds <- predict(m, newdata = xdf,
                     type = if (pt == "Classification") "class" else "vector")

  } else if (algo == "Random Forest") {
    if (requireNamespace("ranger", quietly = TRUE)) {
      # ranger: multithreaded C++, typically 10-20x faster than randomForest
      library(ranger)
      m <- ranger::ranger(
        as.formula(paste0("`", lr_target, "` ~ .")),
        data        = tdf,
        num.trees   = 100L,
        num.threads = max(1L, parallel::detectCores() - 1L),
        importance  = "impurity"
      )
      raw_preds <- predict(m, data = xdf)$predictions
      preds <- if (pt == "Classification")
        factor(raw_preds, levels = levels(tdf[[lr_target]]))
      else
        as.numeric(raw_preds)
      feat_imp <- m$variable.importance   # named numeric vector
    } else {
      # fallback: base randomForest
      library(randomForest)
      m     <- randomForest::randomForest(
        as.formula(paste0("`", lr_target, "` ~ .")), data = tdf, ntree = 100L)
      preds <- predict(m, newdata = xdf)
      fi_df <- tryCatch(as.data.frame(randomForest::importance(m)), error = function(e) NULL)
      feat_imp <- if (!is.null(fi_df))
        setNames(fi_df[[ncol(fi_df)]], rownames(fi_df))   # named numeric vector, same shape
      else NULL
    }

  } else if (algo == "KNN (k=5)") {
    num_c <- feat_cols[sapply(tdf[, feat_cols, drop = FALSE], is.numeric)]
    if (length(num_c) == 0L) stop("KNN requires at least 1 numeric feature.")
    preds <- class::knn(train = tdf[, num_c, drop = FALSE],
                        test  = xdf[, num_c, drop = FALSE],
                        cl    = tdf[[lr_target]], k = 5L)

  } else if (algo == "SVM") {
    tp <- if (pt == "Regression") "eps-regression" else "C-classification"
    m  <- e1071::svm(as.formula(paste0("`", lr_target, "` ~ .")),
                     data = tdf, type = tp, kernel = "radial")
    preds <- predict(m, newdata = xdf)

  } else if (algo == "Naive Bayes") {
    m     <- e1071::naiveBayes(
      as.formula(paste0("`", lr_target, "` ~ .")), data = tdf)
    preds <- predict(m, newdata = xdf)
  }

  list(ok       = TRUE,
       model    = m,
       preds    = preds,
       feat_imp = feat_imp,
       secs     = round(proc.time()["elapsed"] - t0, 3),
       status   = "ok")
}
# Force a clean environment so callr serialization doesn't capture the module scope
environment(.learnR_bg_worker) <- baseenv()


# ── Report writer ─────────────────────────────────────────────────────────────
.learnR_write_report <- function(store, report_rv) {
  if (is.null(report_rv) || length(store$metrics) == 0) return(invisible(NULL))
  mets <- store$metrics; pt <- store$pt
  bench_rows <- lapply(names(mets), function(algo) {
    m <- mets[[algo]]
    if (!is.null(m$err)) {
      if (pt == "Regression")
        data.frame(Algorithm=algo, RMSE=NA, MAE=NA, R2=NA, Time_s=NA,
                   Status=m$status %||% "error", Note=m$err, stringsAsFactors=FALSE)
      else
        data.frame(Algorithm=algo, Accuracy=NA, Precision=NA, Recall=NA,
                   F1=NA, Time_s=NA, Status=m$status %||% "error",
                   Note=m$err, stringsAsFactors=FALSE)
    } else {
      if (pt == "Regression")
        data.frame(Algorithm=algo, RMSE=m$RMSE, MAE=m$MAE, R2=m$R2,
                   Time_s=m$Time_s, Status="ok", Note="", stringsAsFactors=FALSE)
      else
        data.frame(Algorithm=algo, Accuracy=m$Accuracy, Precision=m$Precision,
                   Recall=m$Recall, F1=m$F1, Time_s=m$Time_s,
                   Status="ok", Note="", stringsAsFactors=FALSE)
    }
  })
  bench_df    <- do.call(rbind, bench_rows)
  valid_names <- Filter(function(n) is.null(mets[[n]]$err), names(mets))
  best_name   <- if (length(valid_names) > 0) {
    if (pt == "Regression")
      valid_names[which.min(sapply(valid_names, function(n) mets[[n]]$RMSE))]
    else
      valid_names[which.max(sapply(valid_names, function(n) mets[[n]]$Accuracy))]
  } else NA_character_

  report_rv$learn_bench     <- bench_df
  report_rv$learn_target    <- store$tcol
  report_rv$learn_pt        <- pt
  report_rv$learn_best      <- best_name
  report_rv$learn_metrics   <- mets
  report_rv$learn_preds     <- store$preds_test
  report_rv$learn_actual    <- store$y_test
  report_rv$learn_feat_imp  <- store$feat_imp
  report_rv$learn_feat_cols <- store$feat_cols
  report_rv$learn_rows_orig <- store$rows_orig
  report_rv$learn_rows_used <- store$rows_used
}


# ── Module UI ──────────────────────────────────────────────────────────────────
learnRUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    uiOutput(ns("tab_guard")),
    fluidRow(

      # ── Left: Configuration ────────────────────────────────────────────
      column(3,
        div(style = paste0(
              "background:#0e0e0e; border:1px solid #2a2a2a; ",
              "border-radius:8px; padding:16px 14px;"),

          tags$p(style = "color:#00ff00; font-weight:bold; margin:0 0 6px 0;",
                 "1. Target Variable"),
          selectInput(ns("target_var"), NULL, choices = NULL),
          uiOutput(ns("problem_badge")),

          hr(style = "border-color:#333;"),

          tags$p(style = "color:#00ff00; font-weight:bold; margin:0 0 6px 0;",
                 "2. Feature Variables"),
          div(style = "max-height:180px; overflow-y:auto;",
              uiOutput(ns("feature_ui"))),

          hr(style = "border-color:#333;"),

          tags$p(style = "color:#00ff00; font-weight:bold; margin:0 0 4px 0;",
                 "3. Train / Test Split"),
          sliderInput(ns("split_ratio"), NULL,
                      min = 50, max = 90, value = 70, step = 5, post = "% train"),

          hr(style = "border-color:#333;"),

          tags$p(style = "color:#00ff00; font-weight:bold; margin:0 0 4px 0;",
                 "4. Global Sample Cap"),
          sliderInput(ns("max_rows"), NULL,
                      min = 10000, max = 200000, value = 50000,
                      step = 10000, post = " rows"),
          tags$p(style = "color:#555; font-size:11px; margin:-4px 0 0;",
                 "Applied first. Slow algorithms have additional per-algo caps."),

          hr(style = "border-color:#333;"),

          tags$p(style = "color:#00ff00; font-weight:bold; margin:0 0 6px 0;",
                 "5. Algorithms"),
          uiOutput(ns("algo_ui")),

          br(),
          uiOutput(ns("async_badge")),
          br(),
          actionButton(ns("train_btn"), "Train & Benchmark",
                       class = "btn-primary", style = "width:100%;"),
          br(), br(),
          shinyjs::hidden(
            actionButton(ns("stop_btn"), "Stop Training",
                         class = "btn-danger",
                         style = "width:100%;")
          )
        )
      ),

      # ── Right: Status log + Results ──────────────────────────────────────
      column(9,
        uiOutput(ns("training_status_ui")),
        uiOutput(ns("results_panel"))
      )
    )
  )
}


# ── Module Server ──────────────────────────────────────────────────────────────
learnRServer <- function(id, dataset, reset_trigger, report_rv = NULL, monitor_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    store <- reactiveValues(
      models        = list(),
      metrics       = list(),
      preds_test    = list(),
      feat_imp      = NULL,
      tdf           = NULL,
      xdf           = NULL,
      test_df       = NULL,
      feat_cols     = NULL,
      feat_types    = NULL,
      pt            = NULL,
      y_test        = NULL,
      tcol          = NULL,
      trained       = FALSE,
      is_training   = FALSE,
      train_queue   = character(0),
      stop_flag     = FALSE,
      algos_total   = 0L,
      algos_done    = 0L,
      completed_log = list(),
      train_t0      = NULL,
      rows_orig     = NULL,
      rows_used     = NULL
    )

    # Background process tracking (async path only)
    bg <- reactiveValues(proc = NULL, algo = NULL, t0 = NULL)

    # ── No-data guard ─────────────────────────────────────────────────────
    output$tab_guard <- renderUI({
      if (!is.null(dataset())) return(NULL)
      .no_data_ui()
    })

    observe({
      if (is.null(dataset()) || store$is_training) shinyjs::disable("train_btn")
      else                                          shinyjs::enable("train_btn")
    })

    # Show whether async mode is active
    output$async_badge <- renderUI({
      if (.callr_available)
        div(style = "font-size:10px; color:#555; margin-bottom:4px;",
            "\u26A1 Async mode (real Stop supported)")
      else
        div(style = "font-size:10px; color:#444; margin-bottom:4px;",
            "Sync mode \u2014 install callr for real Stop")
    })

    observeEvent(reset_trigger(), {
      store$models <- list(); store$metrics <- list()
      store$preds_test <- list(); store$feat_imp <- NULL
      store$tdf <- NULL; store$xdf <- NULL; store$test_df <- NULL
      store$feat_cols <- NULL; store$feat_types <- NULL
      store$pt <- NULL; store$y_test <- NULL; store$tcol <- NULL
      store$trained <- FALSE; store$is_training <- FALSE
      store$train_queue <- character(0); store$stop_flag <- FALSE
      store$algos_total <- 0L; store$algos_done <- 0L
      store$completed_log <- list()
      store$train_t0 <- NULL; store$rows_orig <- NULL; store$rows_used <- NULL
      bg$proc <- NULL; bg$algo <- NULL; bg$t0 <- NULL
      shinyjs::hide("stop_btn")
      updateSelectInput(session, "target_var", choices = character(0), selected = NULL)
    })

    observe({
      req(dataset())
      updateSelectInput(session, "target_var", choices = names(dataset()))
    })

    prob_type <- reactive({
      req(dataset(), input$target_var, input$target_var %in% names(dataset()))
      if (is.numeric(dataset()[[input$target_var]])) "Regression" else "Classification"
    })

    output$problem_badge <- renderUI({
      req(prob_type())
      pt  <- prob_type()
      clr <- if (pt == "Regression") "#3399ff" else "#00ff00"
      bg_c  <- if (pt == "Regression") "#001633" else "#001a00"
      div(style = paste0(
            "display:inline-block; background:", bg_c, "; border:1px solid ", clr,
            "; border-radius:4px; padding:2px 10px; margin:4px 0 8px; ",
            "font-size:12px; color:", clr, ";"), pt)
    })

    output$feature_ui <- renderUI({
      req(dataset(), input$target_var)
      fc <- setdiff(names(dataset()), input$target_var)
      checkboxGroupInput(ns("feature_vars"), NULL, choices = fc, selected = fc)
    })

    # Algo selector with per-algorithm row-count warnings
    output$algo_ui <- renderUI({
      req(prob_type())
      pt    <- prob_type()
      algos <- if (pt == "Regression")
        c("Linear Regression", "Decision Tree", "Random Forest", "SVM")
      else
        c("Logistic Regression", "Decision Tree", "Random Forest",
          "KNN (k=5)", "SVM", "Naive Bayes")

      # Estimate effective row count after global cap
      n_eff <- if (!is.null(dataset()))
        min(nrow(dataset()), input$max_rows %||% 50000L)
      else 0L

      # Build per-algo cap warning tags (shown below checkboxes)
      cap_hints <- lapply(algos, function(a) {
        hard_cap  <- .LR_ALGO_CAPS[a]
        warn_thr  <- .LR_ALGO_WARN[a]
        if (!is.na(hard_cap) && n_eff > hard_cap) {
          div(style = "color:#ff8800; font-size:10px; margin:1px 0 1px 18px;",
              paste0("\u26A0 ", a, ": capped to ",
                     format(hard_cap, big.mark=","), " rows"))
        } else if (!is.na(warn_thr) && n_eff > warn_thr) {
          div(style = "color:#ffcc00; font-size:10px; margin:1px 0 1px 18px;",
              paste0("\u23F1 ", a, ": may be slow at ",
                     format(n_eff, big.mark=","), " rows"))
        } else NULL
      })
      cap_hints <- Filter(Negate(is.null), cap_hints)

      tagList(
        checkboxGroupInput(ns("selected_algos"), NULL, choices = algos,
                           selected = algos[seq_len(min(3L, length(algos)))]),
        if (length(cap_hints) > 0)
          div(style = "margin-top:4px; margin-bottom:2px;",
              do.call(tagList, cap_hints))
        else NULL
      )
    })


    # ── Train button: validate + build tdf/xdf + set up queue ─────────────
    observeEvent(input$train_btn, {
      req(dataset(), input$target_var,
          length(input$feature_vars) >= 1L,
          length(input$selected_algos) >= 1L)

      data  <- dataset()
      pt    <- prob_type()
      tcol  <- input$target_var
      fcols <- input$feature_vars
      algos <- input$selected_algos

      df <- na.omit(data[, c(fcols, tcol), drop = FALSE])
      if (nrow(df) < 20L) {
        showNotification("Need at least 20 clean rows after removing NAs.",
                         type = "error", duration = 5)
        return()
      }
      # Convert date/datetime -> numeric; character -> factor
      df <- as.data.frame(lapply(df, function(x) {
        if (inherits(x, c("Date", "POSIXct", "POSIXlt", "POSIXt"))) as.numeric(x)
        else if (is.character(x)) as.factor(x)
        else x
      }))
      if (pt == "Classification") df[[tcol]] <- as.factor(df[[tcol]])

      # Drop high-cardinality factor feature columns (>50 levels)
      hi_card <- intersect(fcols,
        names(df)[sapply(df, function(x) is.factor(x) && nlevels(x) > 50)])
      if (length(hi_card) > 0) {
        df    <- df[, !names(df) %in% hi_card, drop = FALSE]
        fcols <- setdiff(fcols, hi_card)
        showNotification(
          paste0("Dropped ", length(hi_card), " high-cardinality column(s): ",
                 paste(hi_card, collapse = ", ")),
          type = "warning", duration = 8)
      }
      if (length(fcols) == 0) {
        showNotification(
          "No usable feature columns remain after removing high-cardinality columns.",
          type = "error", duration = 6)
        return()
      }

      # ── Global row sampling cap (user-controlled slider) ─────────────────
      rows_orig <- nrow(df)
      max_rows  <- input$max_rows
      if (rows_orig > max_rows) {
        set.seed(42L)
        df <- df[sample(rows_orig, max_rows), ]
        showNotification(
          paste0("Global cap: sampled to ",
                 format(max_rows, big.mark = ","), " rows (seed=42). ",
                 "Slow algorithms will be capped further per-algo."),
          type = "warning", duration = 7)
      }
      store$rows_orig <- rows_orig
      store$rows_used <- nrow(df)

      ftypes <- setNames(
        sapply(fcols, function(col) if (is.numeric(df[[col]])) "numeric" else "factor"),
        fcols)

      set.seed(42L)
      n      <- nrow(df)
      tr_idx <- sample(n, floor(n * input$split_ratio / 100))
      train  <- df[tr_idx, ]; test <- df[-tr_idx, ]
      y_test <- test[[tcol]]

      tdf <- train[, fcols, drop = FALSE]
      tdf[[.LR_TARGET]] <- if (pt == "Classification") as.factor(train[[tcol]]) else train[[tcol]]
      xdf <- .align_factors(test[, fcols, drop = FALSE], tdf)

      store$tdf <- tdf; store$xdf <- xdf
      store$test_df   <- cbind(test[, fcols, drop = FALSE], .actual = y_test)
      store$feat_cols <- fcols; store$feat_types <- ftypes
      store$pt <- pt; store$y_test <- y_test; store$tcol <- tcol

      # Reset results
      store$models <- list(); store$metrics <- list()
      store$preds_test <- list(); store$feat_imp <- NULL
      store$trained <- FALSE; store$completed_log <- list()
      bg$proc <- NULL; bg$algo <- NULL; bg$t0 <- NULL

      # Kick off the queue
      store$algos_total  <- length(algos)
      store$algos_done   <- 0L
      store$stop_flag    <- FALSE
      store$train_queue  <- algos
      store$is_training  <- TRUE
      store$train_t0     <- proc.time()["elapsed"]

      shinyjs::disable("train_btn")
      shinyjs::show("stop_btn")
    })


    # ── ASYNC PATH: Dispatcher ────────────────────────────────────────────
    # Fires when training is active, no background process running, queue non-empty.
    # Launches the next algorithm in a callr background process.
    observe({
      if (!.callr_available) return()

      is_train <- store$is_training
      has_proc <- !is.null(bg$proc)
      queue    <- store$train_queue
      stop_req <- store$stop_flag

      req(is_train, !has_proc, length(queue) > 0, !stop_req)

      isolate({
        algo <- queue[1]
        store$train_queue <- queue[-1]

        # Per-algorithm row cap: subsample tdf further if needed
        tdf_use <- store$tdf
        cap      <- .LR_ALGO_CAPS[algo]
        if (!is.na(cap) && nrow(tdf_use) > cap) {
          set.seed(42L)
          tdf_use <- tdf_use[sample(nrow(tdf_use), cap), ]
          showNotification(
            paste0(algo, ": training data capped to ",
                   format(cap, big.mark=","), " rows (per-algo limit)."),
            type = "warning", duration = 5)
        }

        bg$algo <- algo
        bg$t0   <- proc.time()["elapsed"]
        bg$proc <- callr::r_bg(
          func = .learnR_bg_worker,
          args = list(algo      = algo,
                      tdf       = tdf_use,
                      xdf       = store$xdf,
                      feat_cols = store$feat_cols,
                      pt        = store$pt,
                      lr_target = .LR_TARGET),
          supervise = TRUE
        )
      })
    })


    # ── ASYNC PATH: Collector / Poller ────────────────────────────────────
    # Polls the current background process every 400ms.
    # On completion: collects result, updates store, clears bg$proc (which
    # triggers the Dispatcher to launch the next algorithm).
    observe({
      if (!.callr_available) return()

      proc <- bg$proc
      req(!is.null(proc))
      invalidateLater(400, session)

      # Handle stop request: kill the running process
      if (isolate(store$stop_flag)) {
        isolate({
          tryCatch(proc$kill(), error = function(e) NULL)
          secs <- round(proc.time()["elapsed"] - (bg$t0 %||% proc.time()["elapsed"]), 3)
          algo <- bg$algo %||% "Unknown"
          log_entry <- list(algo   = algo,
                            secs   = secs,
                            status = "stopped",
                            err    = "Stopped by user")
          store$completed_log <- c(store$completed_log, list(log_entry))
          store$algos_done    <- store$algos_done + 1L
          store$train_queue   <- character(0)   # drain queue
          bg$proc <- NULL; bg$algo <- NULL; bg$t0 <- NULL
        })
        return()
      }

      if (proc$is_alive()) return()   # still running, come back in 400ms

      # Process finished — collect result
      algo <- isolate(bg$algo)
      t0   <- isolate(bg$t0)

      res <- tryCatch(
        proc$get_result(),
        error = function(e)
          list(ok=FALSE, model=NULL, preds=NULL, feat_imp=NULL,
               secs=round(proc.time()["elapsed"] - (t0 %||% proc.time()["elapsed"]), 3),
               status="error", err=conditionMessage(e))
      )

      isolate({
        bg$proc <- NULL; bg$algo <- NULL; bg$t0 <- NULL

        if (isTRUE(res$ok)) {
          store$models[[algo]]     <- res$model
          store$preds_test[[algo]] <- res$preds
          store$metrics[[algo]]    <- .calc_metrics(res$preds, store$y_test,
                                                    store$pt, res$secs)
          # Feature importance — ranger returns a named numeric vector
          if (algo == "Random Forest" && !is.null(res$feat_imp)) {
            vi <- res$feat_imp
            store$feat_imp <- data.frame(
              Feature    = names(vi),
              Importance = as.numeric(vi),
              row.names  = NULL, stringsAsFactors = FALSE)
          }
        } else {
          store$metrics[[algo]] <- list(err = res$err %||% "Unknown error",
                                        status = res$status %||% "error")
          showNotification(paste0(algo, ": ", res$err %||% "error"),
                           type = "warning", duration = 6)
        }

        log_entry <- list(
          algo   = algo,
          secs   = res$secs %||% 0,
          status = res$status %||% "error",
          err    = res$err %||% "")
        store$completed_log <- c(store$completed_log, list(log_entry))
        store$algos_done    <- store$algos_done + 1L
        # bg$proc is now NULL — Dispatcher fires automatically for next model
      })
    })


    # ── ASYNC PATH: Finalizer ─────────────────────────────────────────────
    # Fires when training is active, no process running, and queue is empty.
    # This covers both natural completion and user-stopped cases.
    observe({
      if (!.callr_available) return()

      is_train <- store$is_training
      has_proc <- !is.null(bg$proc)
      queue    <- store$train_queue

      req(is_train, !has_proc, length(queue) == 0)

      isolate({
        store$is_training <- FALSE
        store$stop_flag   <- FALSE
        store$trained     <- length(store$metrics) > 0

        shinyjs::hide("stop_btn")
        shinyjs::enable("train_btn")

        n_ok <- sum(sapply(store$metrics, function(m) is.null(m$err)))
        was_stopped <- any(sapply(store$completed_log,
                                  function(l) (l$status %||% "") == "stopped"))
        msg <- if (was_stopped)
          paste0("Stopped. ", n_ok, " model(s) completed.")
        else
          paste0(n_ok, "/", store$algos_total, " models trained successfully.")

        showNotification(msg,
          type = if (was_stopped) "warning" else "message", duration = 4)

        if (length(store$models) > 0)
          updateSelectInput(session, "detail_model", choices = names(store$models))

        .learnR_write_report(store, report_rv)
        .monitr_log(monitor_rv,
          paste0("Train (", n_ok, "/", store$algos_total, " models)"),
          rows = nrow(store$tdf),
          secs = round(proc.time()["elapsed"] - (store$train_t0 %||% proc.time()["elapsed"]), 2),
          note = store$pt %||% "")
      })
    })


    # ── SYNC PATH: Queue worker (callr not available) ─────────────────────
    # Processes one model per observer execution (yields between models).
    observe({
      if (.callr_available) return()

      queue    <- store$train_queue
      is_train <- store$is_training
      stop_req <- store$stop_flag

      req(is_train, length(queue) > 0)

      if (stop_req) {
        isolate({
          store$is_training <- FALSE
          store$train_queue <- character(0)
          store$stop_flag   <- FALSE
          store$trained     <- length(store$metrics) > 0
          shinyjs::hide("stop_btn")
          shinyjs::enable("train_btn")
          n_ok <- sum(sapply(store$metrics, function(m) is.null(m$err)))
          showNotification(
            paste0("Stopped. ", n_ok, "/", store$algos_total, " models completed."),
            type = "warning", duration = 5)
          if (length(store$models) > 0)
            updateSelectInput(session, "detail_model", choices = names(store$models))
          .learnR_write_report(store, report_rv)
          .monitr_log(monitor_rv,
            paste0("Train stopped (", n_ok, "/", store$algos_total, ")"),
            rows = if (!is.null(store$tdf)) nrow(store$tdf) else NULL,
            secs = round(proc.time()["elapsed"] - (store$train_t0 %||% proc.time()["elapsed"]), 2),
            note = store$pt %||% "")
        })
        return()
      }

      algo  <- queue[1]
      done  <- isolate(store$algos_done)
      total <- isolate(store$algos_total)

      withProgress(
        message = paste0("(", done + 1L, "/", total, ") Training ", algo),
        detail  = .LR_MESSAGES[[algo]] %||% "Running...",
        value   = if (total > 0) done / total else 0,
        {
          isolate({
            remaining         <- queue[-1]
            store$train_queue <- remaining

            # Per-algorithm row cap
            tdf_use <- store$tdf
            cap      <- .LR_ALGO_CAPS[algo]
            if (!is.na(cap) && nrow(tdf_use) > cap) {
              set.seed(42L)
              tdf_use <- tdf_use[sample(nrow(tdf_use), cap), ]
              showNotification(
                paste0(algo, ": training data capped to ",
                       format(cap, big.mark=","), " rows (per-algo limit)."),
                type = "warning", duration = 5)
            }

            res <- tryCatch(
              .learnR_bg_worker(algo, tdf_use, store$xdf,
                                store$feat_cols, store$pt),
              error = function(e)
                list(ok=FALSE, model=NULL, preds=NULL, feat_imp=NULL,
                     secs=0, status="error", err=conditionMessage(e))
            )

            log_entry <- list(
              algo   = algo,
              secs   = res$secs %||% 0,
              status = res$status %||% "error",
              err    = res$err %||% "")

            if (isTRUE(res$ok)) {
              store$models[[algo]]     <- res$model
              store$preds_test[[algo]] <- res$preds
              store$metrics[[algo]]    <- .calc_metrics(res$preds, store$y_test,
                                                        store$pt, res$secs)
              if (algo == "Random Forest" && !is.null(res$feat_imp)) {
                vi <- res$feat_imp
                store$feat_imp <- data.frame(
                  Feature    = names(vi),
                  Importance = as.numeric(vi),
                  row.names  = NULL, stringsAsFactors = FALSE)
              }
            } else {
              store$metrics[[algo]] <- list(err = res$err, status = res$status)
              showNotification(paste0(algo, ": ", res$err),
                               type = "warning", duration = 6)
            }

            store$completed_log <- c(store$completed_log, list(log_entry))
            store$algos_done    <- store$algos_done + 1L

            if (length(remaining) == 0L) {
              store$is_training <- FALSE
              store$trained     <- length(store$metrics) > 0
              shinyjs::hide("stop_btn")
              shinyjs::enable("train_btn")
              n_ok <- sum(sapply(store$metrics, function(m) is.null(m$err)))
              showNotification(
                paste0(n_ok, "/", total, " models trained successfully."),
                type = "message", duration = 4)
              if (length(store$models) > 0)
                updateSelectInput(session, "detail_model", choices = names(store$models))
              .learnR_write_report(store, report_rv)
              .monitr_log(monitor_rv,
                paste0("Train (", n_ok, "/", total, " models)"),
                rows = nrow(store$tdf),
                secs = round(proc.time()["elapsed"] - (store$train_t0 %||% proc.time()["elapsed"]), 2),
                note = store$pt %||% "")
            }
          })
        }
      )
    })


    # ── Stop button ───────────────────────────────────────────────────────
    observeEvent(input$stop_btn, {
      store$stop_flag <- TRUE
    })


    # ── Training status log ────────────────────────────────────────────────
    output$training_status_ui <- renderUI({
      log <- store$completed_log
      if (length(log) == 0 && !store$is_training) return(NULL)

      done       <- store$algos_done
      total      <- store$algos_total
      is_running <- store$is_training
      pct        <- if (total > 0) round(done / total * 100) else 100

      log_items <- lapply(log, function(item) {
        s     <- item$status %||% "error"
        icon  <- switch(s,
          ok      = "\u2713",
          stopped = "\u25A0",
          "\u2717")
        color <- switch(s,
          ok      = "#00ff00",
          stopped = "#ffaa00",
          "#ff4444")
        suffix <- if (s == "ok") {
          paste0("  \u2014  ", item$secs, "s")
        } else if (s == "stopped") {
          "  \u2014  stopped by user"
        } else {
          paste0("  \u2014  ", substr(item$err %||% "error", 1, 70))
        }
        div(style = paste0("color:", color,
                           "; font-size:12px; font-family:monospace; margin:3px 0;"),
            paste0(icon, "  ", item$algo, suffix))
      })

      # Show currently-running algorithm (async path)
      current_line <- if (is_running && !is.null(isolate(bg$algo))) {
        div(style = "color:#ffcc00; font-size:12px; font-family:monospace; margin:3px 0;",
            paste0("\u27F3  ", isolate(bg$algo), "  \u2014  running..."))
      } else if (is_running && !.callr_available && done < total) {
        # Sync path: currently training algo is queue[1], not yet in log
        div(style = "color:#ffcc00; font-size:12px; font-family:monospace; margin:3px 0;",
            paste0("\u27F3  training..."))
      } else NULL

      header_label <- if (is_running)
        paste0("Training\u2026 (", done, " / ", total, " done)")
      else
        paste0("Training complete \u2014 ", done, " / ", total, " models")

      div(style = paste0(
            "background:#080808; border:1px solid #2a2a2a; ",
            "border-radius:8px; padding:14px 16px; margin-bottom:16px;"),
        tags$p(style = "color:#00ff00; font-size:13px; font-weight:bold; margin:0 0 10px 0;",
               header_label),
        div(style = "background:#1a1a1a; border-radius:4px; height:4px; margin-bottom:12px;",
          div(style = paste0("background:#00ff00; border-radius:4px; height:4px; width:",
                             pct, "%;"))
        ),
        do.call(tagList, log_items),
        if (!is.null(current_line)) current_line else NULL
      )
    })


    # ── Results panel ─────────────────────────────────────────────────────
    output$results_panel <- renderUI({
      if (!store$trained) {
        if (store$is_training) return(NULL)
        return(div(style = "color:#555; margin-top:60px; text-align:center; font-size:15px;",
                   "Configure settings on the left and click \u201cTrain & Benchmark\u201d."))
      }
      tabsetPanel(
        tabPanel("Benchmark",
          br(),
          withSpinner(DTOutput(ns("bench_dt"))),
          br(),
          uiOutput(ns("best_badge"))
        ),
        tabPanel("Model Details",
          br(),
          selectInput(ns("detail_model"), "View model:", choices = names(store$models)),
          uiOutput(ns("detail_ui"))
        ),
        tabPanel("Feature Importance",
          br(),
          uiOutput(ns("fi_ui"))
        ),
        tabPanel("Predict New Row",
          br(),
          div(style = "display:flex; gap:10px; margin-bottom:14px;",
            actionButton(ns("sample_btn"), "Fill with Test Sample", class = "btn-secondary"),
            actionButton(ns("predict_btn"), "Predict", class = "btn-success")
          ),
          uiOutput(ns("pred_form_ui")),
          br(),
          uiOutput(ns("pred_result_ui"))
        )
      )
    })


    # ── Benchmark table ───────────────────────────────────────────────────
    output$bench_dt <- renderDT({
      req(store$trained, length(store$metrics) > 0)
      pt <- store$pt

      rows <- lapply(names(store$metrics), function(algo) {
        m      <- store$metrics[[algo]]
        failed <- !is.null(m$err)
        st     <- m$status %||% (if (failed) "error" else "ok")
        status_html <- switch(st,
          ok      = '<span style="color:#00ff00">\u2713 ok</span>',
          stopped = '<span style="color:#ffaa00">\u25A0 stopped</span>',
          '<span style="color:#ff4444">\u2717 error</span>')

        if (failed) {
          if (pt == "Regression")
            data.frame(Algorithm=algo, RMSE=NA, MAE=NA, R2=NA, Time_s=NA,
                       Status=status_html, Note=m$err, stringsAsFactors=FALSE)
          else
            data.frame(Algorithm=algo, Accuracy=NA, Precision=NA, Recall=NA,
                       F1=NA, Time_s=NA, Status=status_html,
                       Note=m$err, stringsAsFactors=FALSE)
        } else {
          if (pt == "Regression")
            data.frame(Algorithm=algo, RMSE=m$RMSE, MAE=m$MAE, R2=m$R2,
                       Time_s=m$Time_s, Status=status_html, Note="",
                       stringsAsFactors=FALSE)
          else
            data.frame(Algorithm=algo, Accuracy=m$Accuracy, Precision=m$Precision,
                       Recall=m$Recall, F1=m$F1, Time_s=m$Time_s,
                       Status=status_html, Note="", stringsAsFactors=FALSE)
        }
      })
      df <- do.call(rbind, rows)

      valid  <- which(!is.na(if (pt == "Regression") df$RMSE else df$Accuracy))
      best_0 <- if (length(valid) > 0) {
        if (pt == "Regression") valid[which.min(df$RMSE[valid])] - 1L
        else                    valid[which.max(df$Accuracy[valid])] - 1L
      } else -1L

      status_col_idx <- which(names(df) == "Status")
      datatable(df, rownames = FALSE,
        escape = if (length(status_col_idx) == 1L) -status_col_idx else FALSE,
        options = list(
          pageLength = 15, dom = "t", scrollX = TRUE,
          rowCallback = DT::JS(paste0(
            "function(row,data,index){if(index===", best_0,
            "){$(row).css({'background':'#001a00','color':'#00ff00',",
            "'font-weight':'bold'});}}"
          ))
        ),
        class = "display compact cell-border stripe hover")
    })

    output$best_badge <- renderUI({
      req(store$trained, length(store$metrics) > 0)
      pt    <- store$pt
      valid <- Filter(function(n) is.null(store$metrics[[n]]$err), names(store$metrics))
      if (length(valid) == 0L) return(NULL)
      best  <- if (pt == "Regression")
        valid[which.min(sapply(valid, function(n) store$metrics[[n]]$RMSE))]
      else
        valid[which.max(sapply(valid, function(n) store$metrics[[n]]$Accuracy))]
      metric_str <- if (pt == "Regression")
        paste0("RMSE = ", store$metrics[[best]]$RMSE,
               "  |  R\u00b2 = ", store$metrics[[best]]$R2)
      else
        paste0("Accuracy = ", store$metrics[[best]]$Accuracy,
               "  |  F1 = ", store$metrics[[best]]$F1)
      div(style = paste0(
            "display:inline-block; background:#001400; border:1px solid #00ff00; ",
            "border-radius:6px; padding:8px 18px; margin-top:6px;"),
        tags$strong(style = "color:#00ff00; font-size:15px;",
                    paste0("\u2605 Best: ", best)),
        span(style = "color:#aaa; margin-left:12px; font-size:13px;", metric_str)
      )
    })


    # ── Model Details ─────────────────────────────────────────────────────
    output$detail_ui <- renderUI({
      req(input$detail_model, store$trained)
      if (store$pt == "Classification")
        withSpinner(plotOutput(ns("cm_plot"), height = "400px"))
      else
        tagList(withSpinner(plotOutput(ns("avp_plot"), height = "310px")),
                br(),
                withSpinner(plotOutput(ns("res_plot"), height = "260px")))
    })

    output$cm_plot <- renderPlot({
      req(input$detail_model, store$preds_test[[input$detail_model]], store$test_df)
      algo   <- input$detail_model
      preds  <- store$preds_test[[algo]]; actual <- store$test_df$.actual
      lv     <- union(levels(as.factor(actual)), levels(as.factor(preds)))
      cm_df  <- as.data.frame(table(Predicted = factor(preds, levels=lv),
                                    Actual    = factor(actual, levels=lv)))
      ggplot2::ggplot(cm_df, ggplot2::aes(x=Actual, y=Predicted, fill=Freq)) +
        ggplot2::geom_tile(color="#111111") +
        ggplot2::geom_text(ggplot2::aes(label=Freq), color="white", size=5, fontface="bold") +
        ggplot2::scale_fill_gradient(low="#0a1a0a", high="#00cc44") +
        .dark_theme() +
        ggplot2::labs(title = paste("Confusion Matrix \u2014", algo)) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust=1))
    })

    output$avp_plot <- renderPlot({
      req(input$detail_model, store$preds_test[[input$detail_model]], store$test_df)
      p <- as.numeric(store$preds_test[[input$detail_model]])
      a <- as.numeric(store$test_df$.actual)
      ggplot2::ggplot(data.frame(Actual=a, Predicted=p),
                     ggplot2::aes(x=Actual, y=Predicted)) +
        ggplot2::geom_point(alpha=0.65, color="#00ff00") +
        ggplot2::geom_abline(slope=1, intercept=0,
                             color="white", linetype="dashed", linewidth=0.7) +
        .dark_theme() +
        ggplot2::labs(title = paste("Actual vs Predicted \u2014", input$detail_model))
    })

    output$res_plot <- renderPlot({
      req(input$detail_model, store$preds_test[[input$detail_model]], store$test_df)
      r <- as.numeric(store$test_df$.actual) -
           as.numeric(store$preds_test[[input$detail_model]])
      ggplot2::ggplot(data.frame(Residual=r), ggplot2::aes(x=Residual)) +
        ggplot2::geom_histogram(fill="#00ff00", color="black", bins=30L, alpha=0.85) +
        ggplot2::geom_vline(xintercept=0, color="white",
                            linetype="dashed", linewidth=0.7) +
        .dark_theme() +
        ggplot2::labs(title = paste("Residual Distribution \u2014", input$detail_model))
    })


    # ── Feature Importance ────────────────────────────────────────────────
    # store$feat_imp is now data.frame(Feature, Importance) from ranger
    output$fi_ui <- renderUI({
      if (is.null(store$feat_imp))
        p(style = "color:#555; margin-top:20px;",
          "Feature importance is available after training a Random Forest model.")
      else
        withSpinner(plotOutput(ns("fi_plot"), height = "420px"))
    })

    output$fi_plot <- renderPlot({
      req(store$feat_imp)
      df <- store$feat_imp[order(store$feat_imp$Importance), ]
      df$Feature <- factor(df$Feature, levels = df$Feature)
      ggplot2::ggplot(df, ggplot2::aes(x=Feature, y=Importance)) +
        ggplot2::geom_bar(stat="identity", fill="#00ff00", alpha=0.85, color="black") +
        ggplot2::coord_flip() +
        .dark_theme() +
        ggplot2::labs(title="Feature Importance (Random Forest)",
                      x=NULL, y="Impurity Importance")
    })


    # ── Predict New Row ───────────────────────────────────────────────────
    output$pred_form_ui <- renderUI({
      req(store$trained, store$feat_cols, dataset())
      data  <- dataset(); fcols <- store$feat_cols
      inputs <- lapply(fcols, function(col) {
        x      <- data[[col]]
        inp_id <- ns(paste0("pred_", make.names(col)))
        if (is.numeric(x))
          numericInput(inp_id, col, value = round(mean(x, na.rm=TRUE), 3))
        else
          selectInput(inp_id, col, choices = sort(unique(as.character(x[!is.na(x)]))))
      })
      rows <- split(inputs, ceiling(seq_along(inputs) / 3))
      do.call(tagList, lapply(rows, function(r) fluidRow(lapply(r, function(i) column(4, i)))))
    })

    observeEvent(input$sample_btn, {
      req(store$test_df, store$feat_cols, store$feat_types)
      row <- store$test_df[sample(nrow(store$test_df), 1L), ]
      for (col in store$feat_cols) {
        val    <- row[[col]]
        inp_id <- paste0("pred_", make.names(col))
        if (store$feat_types[[col]] == "numeric")
          updateNumericInput(session, inp_id, value = as.numeric(val))
        else
          updateSelectInput(session, inp_id, selected = as.character(val))
      }
    })

    pred_results <- eventReactive(input$predict_btn, {
      req(store$trained, store$feat_cols, dataset())
      fcols <- store$feat_cols
      new_row <- as.data.frame(setNames(lapply(fcols, function(col) {
        val <- input[[paste0("pred_", make.names(col))]]
        if (store$feat_types[[col]] == "numeric") as.numeric(val) else val
      }), fcols), stringsAsFactors = FALSE)
      for (col in fcols)
        if (is.factor(store$tdf[[col]]))
          new_row[[col]] <- factor(new_row[[col]], levels = levels(store$tdf[[col]]))

      lapply(setNames(names(store$models), names(store$models)), function(algo) {
        tryCatch({
          m <- store$models[[algo]]
          if (algo == "KNN (k=5)") {
            num_c <- fcols[sapply(store$tdf[, fcols, drop=FALSE], is.numeric)]
            if (length(num_c) == 0L) stop("No numeric features for KNN.")
            as.character(class::knn(train=store$tdf[,num_c,drop=FALSE],
                                    test=new_row[,num_c,drop=FALSE],
                                    cl=store$tdf[[.LR_TARGET]], k=5L))
          } else if (algo == "Logistic Regression") {
            prob <- predict(m, newdata=new_row, type="response")
            lvl  <- levels(store$tdf[[.LR_TARGET]])
            as.character(ifelse(prob > 0.5, lvl[2], lvl[1]))
          } else if (algo == "Decision Tree") {
            tp <- if (store$pt == "Regression") "vector" else "class"
            as.character(predict(m, newdata=new_row, type=tp))
          } else if (algo == "Random Forest") {
            if (inherits(m, "ranger"))
              as.character(predict(m, data=new_row)$predictions)
            else
              as.character(predict(m, newdata=new_row))
          } else {
            as.character(predict(m, newdata=new_row))
          }
        }, error = function(e) paste0("Error: ", conditionMessage(e)))
      })
    })

    output$pred_result_ui <- renderUI({
      req(pred_results())
      res <- pred_results(); pt <- store$pt
      rows <- lapply(names(res), function(algo) {
        val    <- paste(as.character(res[[algo]]), collapse=", ")
        is_err <- startsWith(val, "Error:")
        div(style = paste0(
              "padding:8px 14px; margin:5px 0; border-radius:5px; background:#111; ",
              "border-left:3px solid ", if (is_err) "#cc3300" else "#00ff00", ";"),
          tags$strong(style="color:#00ff00; font-size:13px;", algo),
          tags$span(style=paste0("color:", if (is_err) "#ff6644" else "#ffffff",
                                 "; margin-left:14px; font-size:15px;"), val))
      })
      tagList(
        tags$p(style="color:#888; font-size:13px; margin-bottom:8px;",
               paste("Predicting:", input$target_var,
                     if (pt == "Classification") "(Classification)" else "(Regression)")),
        do.call(tagList, rows))
    })
  })
}
