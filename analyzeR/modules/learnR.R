# ─────────────────────────────────────────────────────────────────────────────
# learnR.R  —  ML Training, Benchmarking & Prediction module
#
# Packages required (install if missing):
#   install.packages(c("rpart", "randomForest", "e1071", "class"))
#
# Internal convention: the target column is renamed to `.lr_y` inside
# training data frames to avoid formula collisions with user column names.
# ─────────────────────────────────────────────────────────────────────────────

# ── File-scope helpers ────────────────────────────────────────────────────────

.LR_TARGET <- ".lr_y"   # internal target name used in all formula calls

# Align factor levels in `xdf` to match those in `tdf` (training data)
.align_factors <- function(xdf, tdf) {
  for (col in names(xdf)) {
    if (is.factor(tdf[[col]])) {
      xdf[[col]] <- factor(xdf[[col]], levels = levels(tdf[[col]]))
    }
  }
  xdf
}

# Train ONE algorithm; returns list(ok, model, preds, secs) or list(ok=F, err)
.train_one <- function(algo, tdf, xdf, feat_cols, pt) {
  t0 <- proc.time()["elapsed"]
  tryCatch({
    m <- NULL; preds <- NULL

    if (algo == "Linear Regression") {
      m     <- lm(as.formula(paste0("`", .LR_TARGET, "` ~ .")), data = tdf)
      preds <- as.numeric(predict(m, newdata = xdf))

    } else if (algo == "Logistic Regression") {
      nl <- nlevels(tdf[[.LR_TARGET]])
      if (nl != 2L)
        stop(sprintf("Binary only — dataset has %d classes.", nl))
      m     <- glm(as.formula(paste0("`", .LR_TARGET, "` ~ .")),
                   data = tdf, family = binomial())
      prob  <- predict(m, newdata = xdf, type = "response")
      lvl   <- levels(tdf[[.LR_TARGET]])
      preds <- factor(ifelse(prob > 0.5, lvl[2], lvl[1]), levels = lvl)

    } else if (algo == "Decision Tree") {
      mth   <- if (pt == "Regression") "anova" else "class"
      m     <- rpart::rpart(
        as.formula(paste0("`", .LR_TARGET, "` ~ .")),
        data = tdf, method = mth,
        control = rpart::rpart.control(cp = 0.01, minsplit = 5L))
      preds <- predict(m, newdata = xdf,
                       type = if (pt == "Classification") "class" else "vector")

    } else if (algo == "Random Forest") {
      m     <- randomForest::randomForest(
        as.formula(paste0("`", .LR_TARGET, "` ~ .")),
        data = tdf, ntree = 100L)
      preds <- predict(m, newdata = xdf)

    } else if (algo == "KNN (k=5)") {
      num_c <- feat_cols[sapply(tdf[, feat_cols, drop = FALSE], is.numeric)]
      if (length(num_c) == 0L) stop("KNN requires at least 1 numeric feature.")
      preds <- class::knn(
        train = tdf[, num_c, drop = FALSE],
        test  = xdf[, num_c, drop = FALSE],
        cl    = tdf[[.LR_TARGET]], k = 5L)

    } else if (algo == "SVM") {
      tp    <- if (pt == "Regression") "eps-regression" else "C-classification"
      m     <- e1071::svm(
        as.formula(paste0("`", .LR_TARGET, "` ~ .")),
        data = tdf, type = tp, kernel = "radial")
      preds <- predict(m, newdata = xdf)

    } else if (algo == "Naive Bayes") {
      m     <- e1071::naiveBayes(
        as.formula(paste0("`", .LR_TARGET, "` ~ .")),
        data = tdf)
      preds <- predict(m, newdata = xdf)
    }

    list(ok = TRUE, model = m, preds = preds,
         secs = round(proc.time()["elapsed"] - t0, 3))

  }, error = function(e) list(ok = FALSE, err = conditionMessage(e)))
}

# Compute regression or classification metrics
.calc_metrics <- function(preds, actual, pt, secs) {
  tryCatch({
    if (pt == "Regression") {
      a  <- as.numeric(actual)
      p  <- as.numeric(preds)
      st <- sum((a - mean(a, na.rm = TRUE))^2, na.rm = TRUE)
      list(
        RMSE      = round(sqrt(mean((a - p)^2, na.rm = TRUE)), 4),
        MAE       = round(mean(abs(a - p), na.rm = TRUE), 4),
        R2        = round(if (st == 0) 0 else max(0, 1 - sum((a-p)^2)/st), 4),
        Time_s    = secs, cm = NULL, err = NULL)
    } else {
      lv <- union(levels(as.factor(actual)), levels(as.factor(preds)))
      af <- factor(actual, levels = lv)
      pf <- factor(preds,  levels = lv)
      cm <- table(Predicted = pf, Actual = af)
      ac <- sum(diag(cm)) / sum(cm)
      pr <- mean(diag(cm) / pmax(rowSums(cm), 1), na.rm = TRUE)
      re <- mean(diag(cm) / pmax(colSums(cm), 1), na.rm = TRUE)
      f1 <- if ((pr + re) == 0) 0 else 2 * pr * re / (pr + re)
      list(
        Accuracy  = round(ac, 4), Precision = round(pr, 4),
        Recall    = round(re, 4), F1        = round(f1, 4),
        Time_s    = secs, cm = cm, err = NULL)
    }
  }, error = function(e) list(err = conditionMessage(e)))
}


# ── Module UI ─────────────────────────────────────────────────────────────────
learnRUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    fluidRow(

      # ── Left: Configuration ──────────────────────────────────────────────
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

          tags$p(style = "color:#00ff00; font-weight:bold; margin:0 0 6px 0;",
                 "4. Algorithms"),
          uiOutput(ns("algo_ui")),

          br(),
          actionButton(ns("train_btn"), "Train & Benchmark",
                       class = "btn-primary", style = "width:100%;")
        )
      ),

      # ── Right: Results ────────────────────────────────────────────────────
      column(9,
        uiOutput(ns("results_panel"))
      )
    )
  )
}


# ── Module Server ─────────────────────────────────────────────────────────────
learnRServer <- function(input, output, session, dataset) {
  ns <- session$ns

  store <- reactiveValues(
    models      = list(),   # algo → model object (NULL for KNN)
    metrics     = list(),   # algo → metrics list
    preds_test  = list(),   # algo → predictions on test set
    feat_imp    = NULL,     # Random Forest importance data.frame
    tdf         = NULL,     # training feature df + .lr_y (for KNN predict)
    test_df     = NULL,     # test set: features + .actual column
    feat_cols   = NULL,     # character vector of feature column names
    feat_types  = NULL,     # named list: col → "numeric"/"factor"
    pt          = NULL,     # "Regression" or "Classification"
    trained     = FALSE     # gates results panel
  )

  # ── Populate target selector when dataset changes ─────────────────────────
  observe({
    req(dataset())
    updateSelectInput(session, "target_var", choices = names(dataset()))
  })

  # ── Detect problem type ────────────────────────────────────────────────────
  prob_type <- reactive({
    req(dataset(), input$target_var, input$target_var %in% names(dataset()))
    if (is.numeric(dataset()[[input$target_var]])) "Regression" else "Classification"
  })

  output$problem_badge <- renderUI({
    req(prob_type())
    pt  <- prob_type()
    clr <- if (pt == "Regression") "#3399ff" else "#00ff00"
    bg  <- if (pt == "Regression") "#001633" else "#001a00"
    div(style = paste0(
          "display:inline-block; background:", bg, "; border:1px solid ", clr,
          "; border-radius:4px; padding:2px 10px; margin:4px 0 8px; ",
          "font-size:12px; color:", clr, ";"),
        pt)
  })

  # ── Feature checkboxes (exclude target) ───────────────────────────────────
  output$feature_ui <- renderUI({
    req(dataset(), input$target_var)
    fc <- setdiff(names(dataset()), input$target_var)
    checkboxGroupInput(ns("feature_vars"), NULL,
                       choices = fc, selected = fc)
  })

  # ── Algorithm checkboxes (filtered by problem type) ───────────────────────
  output$algo_ui <- renderUI({
    req(prob_type())
    algos <- if (prob_type() == "Regression")
      c("Linear Regression", "Decision Tree", "Random Forest", "SVM")
    else
      c("Logistic Regression", "Decision Tree", "Random Forest",
        "KNN (k=5)", "SVM", "Naive Bayes")
    checkboxGroupInput(ns("selected_algos"), NULL,
                       choices  = algos,
                       selected = algos[seq_len(min(3L, length(algos)))])
  })

  # ── Train & Benchmark ─────────────────────────────────────────────────────
  observeEvent(input$train_btn, {
    req(dataset(), input$target_var,
        length(input$feature_vars) >= 1L,
        length(input$selected_algos) >= 1L)

    data  <- dataset()
    pt    <- prob_type()
    tcol  <- input$target_var
    fcols <- input$feature_vars
    algos <- input$selected_algos

    # Subset + clean
    df <- na.omit(data[, c(fcols, tcol), drop = FALSE])
    if (nrow(df) < 20L) {
      showNotification("Need at least 20 clean rows after removing NAs.",
                       type = "error", duration = 5)
      return()
    }

    # Encode characters as factors; encode target for classification
    df <- as.data.frame(lapply(df, function(x) {
      if (is.character(x)) as.factor(x) else x
    }))
    if (pt == "Classification") df[[tcol]] <- as.factor(df[[tcol]])

    # Track feature types for the predict form later
    ftypes <- setNames(
      sapply(fcols, function(col) if (is.numeric(df[[col]])) "numeric" else "factor"),
      fcols)

    # Train / test split
    set.seed(42L)
    n      <- nrow(df)
    tr_idx <- sample(n, floor(n * input$split_ratio / 100))
    train  <- df[tr_idx,  ]
    test   <- df[-tr_idx, ]

    y_train <- train[[tcol]]
    y_test  <- test[[tcol]]

    # Build tdf (features + internal target .lr_y)
    tdf <- train[, fcols, drop = FALSE]
    tdf[[.LR_TARGET]] <- if (pt == "Classification") as.factor(y_train) else y_train

    # Build xdf (test features, factor-aligned to training)
    xdf <- .align_factors(test[, fcols, drop = FALSE], tdf)

    # ── Train each selected algorithm ──────────────────────────────────────
    mdls <- list(); mets <- list(); preds <- list(); fi <- NULL

    withProgress(message = "Training models\u2026", value = 0, {
      for (i in seq_along(algos)) {
        algo <- algos[i]
        incProgress(1 / length(algos), detail = algo)

        res <- .train_one(algo, tdf, xdf, fcols, pt)

        if (res$ok) {
          mdls[[algo]]  <- res$model
          preds[[algo]] <- res$preds
          mets[[algo]]  <- .calc_metrics(res$preds, y_test, pt, res$secs)

          # Capture Random Forest feature importance
          if (algo == "Random Forest" && !is.null(res$model)) {
            fi <- tryCatch(
              as.data.frame(randomForest::importance(res$model)),
              error = function(e) NULL)
          }
        } else {
          mets[[algo]] <- list(err = res$err)
          showNotification(paste0(algo, ": ", res$err),
                           type = "warning", duration = 6)
        }
      }
    })

    # Persist to store
    store$models     <- mdls
    store$metrics    <- mets
    store$preds_test <- preds
    store$feat_imp   <- fi
    store$tdf        <- tdf
    store$test_df    <- cbind(test[, fcols, drop = FALSE],
                              .actual = y_test)
    store$feat_cols  <- fcols
    store$feat_types <- ftypes
    store$pt         <- pt
    store$trained    <- TRUE

    n_ok <- sum(sapply(mets, function(m) is.null(m$err)))
    showNotification(
      paste0(n_ok, " / ", length(algos), " models trained successfully."),
      type = "message", duration = 4)

    # Update detail-model selector
    updateSelectInput(session, "detail_model", choices = names(mdls))
  })

  # ── Results panel scaffold (rendered once trained = TRUE) ─────────────────
  output$results_panel <- renderUI({
    if (!store$trained) {
      return(div(
        style = "color:#555; margin-top:60px; text-align:center; font-size:15px;",
        "Configure settings on the left and click \u201cTrain & Benchmark\u201d."))
    }
    tabsetPanel(
      tabPanel("Benchmark",
        br(),
        DTOutput(ns("bench_dt")),
        br(),
        uiOutput(ns("best_badge"))
      ),
      tabPanel("Model Details",
        br(),
        selectInput(ns("detail_model"), "View model:",
                    choices = names(store$models)),
        uiOutput(ns("detail_ui"))
      ),
      tabPanel("Feature Importance",
        br(),
        uiOutput(ns("fi_ui"))
      ),
      tabPanel("Predict New Row",
        br(),
        div(style = "display:flex; gap:10px; margin-bottom:14px;",
          actionButton(ns("sample_btn"), "Fill with Test Sample",
                       class = "btn-secondary"),
          actionButton(ns("predict_btn"), "Predict",
                       class = "btn-success")
        ),
        uiOutput(ns("pred_form_ui")),
        br(),
        uiOutput(ns("pred_result_ui"))
      )
    )
  })

  # ── Benchmark table ────────────────────────────────────────────────────────
  output$bench_dt <- renderDT({
    req(store$trained, length(store$metrics) > 0)
    pt <- store$pt

    rows <- lapply(names(store$metrics), function(algo) {
      m <- store$metrics[[algo]]
      if (!is.null(m$err)) {
        if (pt == "Regression")
          data.frame(Algorithm = algo, RMSE = NA, MAE = NA, R2 = NA,
                     Time_s = NA, Note = m$err, stringsAsFactors = FALSE)
        else
          data.frame(Algorithm = algo, Accuracy = NA, Precision = NA,
                     Recall = NA, F1 = NA, Time_s = NA,
                     Note = m$err, stringsAsFactors = FALSE)
      } else {
        if (pt == "Regression")
          data.frame(Algorithm = algo, RMSE = m$RMSE, MAE = m$MAE,
                     R2 = m$R2, Time_s = m$Time_s, Note = "",
                     stringsAsFactors = FALSE)
        else
          data.frame(Algorithm = algo, Accuracy = m$Accuracy,
                     Precision = m$Precision, Recall = m$Recall,
                     F1 = m$F1, Time_s = m$Time_s, Note = "",
                     stringsAsFactors = FALSE)
      }
    })
    df <- do.call(rbind, rows)

    # Determine best row (0-indexed for JS)
    valid <- which(!is.na(if (pt == "Regression") df$RMSE else df$Accuracy))
    best_0 <- if (length(valid) > 0) {
      if (pt == "Regression")
        valid[which.min(df$RMSE[valid])] - 1L
      else
        valid[which.max(df$Accuracy[valid])] - 1L
    } else -1L

    datatable(df, rownames = FALSE,
      options = list(
        pageLength = 15, dom = "t", scrollX = TRUE,
        rowCallback = DT::JS(paste0(
          "function(row, data, index) {",
          "  if (index === ", best_0, ") {",
          "    $(row).css({'background':'#001a00','color':'#00ff00',",
          "               'font-weight':'bold'});",
          "  }",
          "}"))
      ),
      class = "display compact cell-border stripe hover")
  })

  output$best_badge <- renderUI({
    req(store$trained, length(store$metrics) > 0)
    pt    <- store$pt
    valid <- Filter(function(n) is.null(store$metrics[[n]]$err), names(store$metrics))
    if (length(valid) == 0L) return(NULL)

    best <- if (pt == "Regression")
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
      span(style = "color:#aaa; margin-left:12px; font-size:13px;",
           metric_str)
    )
  })

  # ── Model Details ──────────────────────────────────────────────────────────
  output$detail_ui <- renderUI({
    req(input$detail_model, store$trained)
    if (store$pt == "Classification")
      plotOutput(ns("cm_plot"), height = "400px")
    else
      tagList(
        plotOutput(ns("avp_plot"), height = "310px"),
        br(),
        plotOutput(ns("res_plot"), height = "260px")
      )
  })

  # Confusion matrix heatmap
  output$cm_plot <- renderPlot({
    req(input$detail_model, store$preds_test[[input$detail_model]], store$test_df)
    algo   <- input$detail_model
    preds  <- store$preds_test[[algo]]
    actual <- store$test_df$.actual
    lv     <- union(levels(as.factor(actual)), levels(as.factor(preds)))
    cm_df  <- as.data.frame(
      table(Predicted = factor(preds, levels = lv),
            Actual    = factor(actual, levels = lv)))

    ggplot2::ggplot(cm_df, ggplot2::aes(x = Actual, y = Predicted, fill = Freq)) +
      ggplot2::geom_tile(color = "#111111") +
      ggplot2::geom_text(ggplot2::aes(label = Freq),
                         color = "white", size = 5, fontface = "bold") +
      ggplot2::scale_fill_gradient(low = "#0a1a0a", high = "#00cc44") +
      ggdark::dark_theme_minimal() +
      ggplot2::labs(title = paste("Confusion Matrix —", algo)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  })

  # Actual vs Predicted
  output$avp_plot <- renderPlot({
    req(input$detail_model, store$preds_test[[input$detail_model]], store$test_df)
    p <- as.numeric(store$preds_test[[input$detail_model]])
    a <- as.numeric(store$test_df$.actual)
    ggplot2::ggplot(data.frame(Actual = a, Predicted = p),
                   ggplot2::aes(x = Actual, y = Predicted)) +
      ggplot2::geom_point(alpha = 0.65, color = "#00ff00") +
      ggplot2::geom_abline(slope = 1, intercept = 0,
                           color = "white", linetype = "dashed", linewidth = 0.7) +
      ggdark::dark_theme_minimal() +
      ggplot2::labs(title = paste("Actual vs Predicted —", input$detail_model))
  })

  # Residuals histogram
  output$res_plot <- renderPlot({
    req(input$detail_model, store$preds_test[[input$detail_model]], store$test_df)
    r <- as.numeric(store$test_df$.actual) -
         as.numeric(store$preds_test[[input$detail_model]])
    ggplot2::ggplot(data.frame(Residual = r), ggplot2::aes(x = Residual)) +
      ggplot2::geom_histogram(fill = "#00ff00", color = "black",
                              bins = 30L, alpha = 0.85) +
      ggplot2::geom_vline(xintercept = 0, color = "white",
                          linetype = "dashed", linewidth = 0.7) +
      ggdark::dark_theme_minimal() +
      ggplot2::labs(title = paste("Residual Distribution —", input$detail_model))
  })

  # ── Feature Importance ─────────────────────────────────────────────────────
  output$fi_ui <- renderUI({
    if (is.null(store$feat_imp))
      p(style = "color:#555; margin-top:20px;",
        "Feature importance is available after training a Random Forest model.")
    else
      plotOutput(ns("fi_plot"), height = "420px")
  })

  output$fi_plot <- renderPlot({
    req(store$feat_imp)
    fi  <- store$feat_imp
    col <- names(fi)[ncol(fi)]   # main importance column (last one)
    df  <- data.frame(Feature = rownames(fi), Importance = fi[[col]])
    df  <- df[order(df$Importance), ]
    df$Feature <- factor(df$Feature, levels = df$Feature)

    ggplot2::ggplot(df, ggplot2::aes(x = Feature, y = Importance)) +
      ggplot2::geom_bar(stat = "identity", fill = "#00ff00", alpha = 0.85,
                        color = "black") +
      ggplot2::coord_flip() +
      ggdark::dark_theme_minimal() +
      ggplot2::labs(title = paste("Feature Importance (Random Forest)"),
                    x = NULL, y = col)
  })

  # ── Predict New Row ────────────────────────────────────────────────────────
  output$pred_form_ui <- renderUI({
    req(store$trained, store$feat_cols, dataset())
    data  <- dataset()
    fcols <- store$feat_cols

    inputs <- lapply(fcols, function(col) {
      x     <- data[[col]]
      inp_id <- ns(paste0("pred_", make.names(col)))
      if (is.numeric(x))
        numericInput(inp_id, col, value = round(mean(x, na.rm = TRUE), 3))
      else {
        lvls <- sort(unique(as.character(x[!is.na(x)])))
        selectInput(inp_id, col, choices = lvls)
      }
    })

    # Lay out in rows of 3
    rows <- split(inputs, ceiling(seq_along(inputs) / 3))
    do.call(tagList, lapply(rows, function(row_inputs) {
      fluidRow(lapply(row_inputs, function(inp) column(4, inp)))
    }))
  })

  # Fill form with a random test-set row
  observeEvent(input$sample_btn, {
    req(store$test_df, store$feat_cols, store$feat_types)
    row <- store$test_df[sample(nrow(store$test_df), 1L), ]
    for (col in store$feat_cols) {
      val   <- row[[col]]
      inp_id <- paste0("pred_", make.names(col))
      if (store$feat_types[[col]] == "numeric")
        updateNumericInput(session, inp_id, value = as.numeric(val))
      else
        updateSelectInput(session, inp_id, selected = as.character(val))
    }
  })

  # Collect form values and predict
  pred_results <- eventReactive(input$predict_btn, {
    req(store$trained, store$feat_cols, dataset())
    fcols <- store$feat_cols

    # Build new-row data frame from inputs
    new_row <- as.data.frame(
      setNames(lapply(fcols, function(col) {
        val <- input[[paste0("pred_", make.names(col))]]
        if (store$feat_types[[col]] == "numeric") as.numeric(val) else val
      }), fcols),
      stringsAsFactors = FALSE)

    # Align factor levels to training
    for (col in fcols) {
      if (is.factor(store$tdf[[col]])) {
        new_row[[col]] <- factor(new_row[[col]],
                                 levels = levels(store$tdf[[col]]))
      }
    }

    # Predict with every trained model
    lapply(setNames(names(store$models), names(store$models)), function(algo) {
      tryCatch({
        m <- store$models[[algo]]

        pred_val <- if (algo == "KNN (k=5)") {
          # Instance-based — needs training data
          num_c <- fcols[sapply(store$tdf[, fcols, drop = FALSE], is.numeric)]
          if (length(num_c) == 0L) stop("No numeric features for KNN.")
          as.character(class::knn(
            train = store$tdf[, num_c, drop = FALSE],
            test  = new_row[, num_c, drop = FALSE],
            cl    = store$tdf[[.LR_TARGET]], k = 5L))

        } else if (algo == "Logistic Regression") {
          prob <- predict(m, newdata = new_row, type = "response")
          lvl  <- levels(store$tdf[[.LR_TARGET]])
          ifelse(prob > 0.5, lvl[2], lvl[1])

        } else {
          as.character(predict(m, newdata = new_row))
        }

        pred_val
      }, error = function(e) paste0("Error: ", conditionMessage(e)))
    })
  })

  output$pred_result_ui <- renderUI({
    req(pred_results())
    res <- pred_results()
    pt  <- store$pt

    rows <- lapply(names(res), function(algo) {
      val <- res[[algo]]
      is_err <- grepl("^Error:", val)
      border <- if (is_err) "#cc3300" else "#00ff00"
      vclr   <- if (is_err) "#ff6644" else "#ffffff"
      div(style = paste0(
            "padding:8px 14px; margin:5px 0; border-radius:5px; ",
            "background:#111; border-left:3px solid ", border, ";"),
        tags$strong(style = "color:#00ff00; font-size:13px;", algo),
        tags$span(style = paste0(
                    "color:", vclr, "; margin-left:14px; font-size:15px;"),
                  val)
      )
    })

    tagList(
      tags$p(style = "color:#888; font-size:13px; margin-bottom:8px;",
             paste("Predicting:", input$target_var,
                   if (pt == "Classification") "(Classification)"
                   else "(Regression)")),
      do.call(tagList, rows)
    )
  })
}
