# Module UI
analyzeRUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    uiOutput(ns("tab_guard")),
    actionButton(ns("analyze_btn"), "Run Analysis", class = "btn-primary"),
    br(), br(),
    tabsetPanel(
      tabPanel("Correlation",
        br(),
        withSpinner(plotOutput(ns("corr_heatmap"), height = "400px"))
      ),
      tabPanel("Normality Tests",
        br(),
        withSpinner(DTOutput(ns("normality_table")))
      ),
      tabPanel("Outlier Detection",
        br(),
        withSpinner(DTOutput(ns("outlier_table")))
      ),
      tabPanel("Hypothesis Tests",
        br(),
        fluidRow(
          column(4,
            div(style = paste0(
                  "background:#0e0e0e; border:1px solid #2a2a2a; ",
                  "border-radius:8px; padding:16px 14px;"),
              tags$p(style = "color:#00ff00; font-weight:bold; margin:0 0 6px 0;",
                     "Test Category"),
              selectInput(ns("test_category"), NULL,
                choices = c("t-test", "ANOVA", "Chi-square", "Non-parametric")),
              uiOutput(ns("test_subtype_ui")),
              hr(style = "border-color:#333;"),
              tags$p(style = "color:#00ff00; font-weight:bold; margin:0 0 6px 0;",
                     "Variables"),
              uiOutput(ns("test_vars_ui")),
              hr(style = "border-color:#333;"),
              numericInput(ns("alpha"), "Significance level (\u03b1):",
                           value = 0.05, min = 0.001, max = 0.2, step = 0.01),
              br(),
              actionButton(ns("run_test_btn"), "Run Test",
                           class = "btn-primary", style = "width:100%;")
            )
          ),
          column(8,
            uiOutput(ns("test_results"))
          )
        )
      )
    )
  )
}

# Module Server
analyzeRServer <- function(id, dataset, reset_trigger, report_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── No-data guard ────────────────────────────────────────────────────
    output$tab_guard <- renderUI({
      if (!is.null(dataset())) return(NULL)
      .no_data_ui()
    })

    # Disable / enable buttons based on data presence
    observe({
      if (is.null(dataset())) {
        shinyjs::disable("analyze_btn")
        shinyjs::disable("run_test_btn")
      } else {
        shinyjs::enable("analyze_btn")
        shinyjs::enable("run_test_btn")
      }
    })

    # ── Existing analysis reactives ──────────────────────────────────────
    ran_analysis <- reactiveVal(FALSE)

    analysis_data <- eventReactive(input$analyze_btn, {
      req(dataset())
      dataset()
    })

    observeEvent(input$analyze_btn, {
      req(dataset())
      withProgress(message = "Analyzing data...", value = 0, {
        setProgress(0.35, detail = "Correlation")
        setProgress(0.70, detail = "Normality tests")
        setProgress(1.00, detail = "Outlier detection")
      })
      ran_analysis(TRUE)
    })
    observeEvent(reset_trigger(), { ran_analysis(FALSE) })

    output$corr_heatmap <- renderPlot({
      req(ran_analysis(), analysis_data())
      data     <- analysis_data()
      num_cols <- data[, sapply(data, is.numeric), drop = FALSE]

      if (ncol(num_cols) < 2) {
        return(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
              label = "Need at least 2 numeric columns for a correlation heatmap.",
              color = "white", size = 5, hjust = 0.5) +
            ggplot2::theme_void() +
            ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#1a1a1a", color = NA))
        )
      }

      corr_mat  <- cor(num_cols, use = "pairwise.complete.obs")
      corr_long <- as.data.frame(as.table(corr_mat))
      names(corr_long) <- c("Var1", "Var2", "Correlation")

      ggplot2::ggplot(corr_long, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
        ggplot2::geom_tile(color = "#111111") +
        ggplot2::geom_text(ggplot2::aes(label = round(Correlation, 2)),
                           color = "white", size = 3.5) +
        ggplot2::scale_fill_gradient2(low = "#d73027", mid = "#333333", high = "#00ff00",
                                      midpoint = 0, limits = c(-1, 1)) +
        ggplot2::labs(title = "Correlation Heatmap", x = NULL, y = NULL) +
        .dark_theme() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    })

    output$normality_table <- renderDT({
      req(ran_analysis(), analysis_data())
      data     <- analysis_data()
      num_cols <- data[, sapply(data, is.numeric), drop = FALSE]

      if (ncol(num_cols) == 0) return(NULL)

      results <- lapply(names(num_cols), function(col_name) {
        x <- num_cols[[col_name]]
        x <- x[!is.na(x)]
        if (length(x) > 5000) x <- sample(x, 5000)
        if (length(x) < 3)    return(NULL)
        tryCatch({
          test <- shapiro.test(x)
          data.frame(
            Variable    = col_name,
            W_Statistic = round(test$statistic, 4),
            p_value     = round(test$p.value, 4),
            Result      = ifelse(test$p.value >= 0.05, "Normal", "Non-normal"),
            stringsAsFactors = FALSE
          )
        }, error = function(e) NULL)
      })

      result_df <- do.call(rbind, results[!sapply(results, is.null)])
      if (is.null(result_df) || nrow(result_df) == 0) return(NULL)

      datatable(result_df, rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE, dom = "t"),
        class = "display compact cell-border stripe hover")
    })

    output$outlier_table <- renderDT({
      req(ran_analysis(), analysis_data())
      data     <- analysis_data()
      num_cols <- data[, sapply(data, is.numeric), drop = FALSE]

      if (ncol(num_cols) == 0) return(NULL)

      results <- lapply(names(num_cols), function(col_name) {
        x       <- num_cols[[col_name]]
        x_clean <- x[!is.na(x)]
        q1      <- quantile(x_clean, 0.25)
        q3      <- quantile(x_clean, 0.75)
        iqr     <- q3 - q1
        n_out   <- sum(x_clean < (q1 - 1.5 * iqr) | x_clean > (q3 + 1.5 * iqr))
        data.frame(
          Variable      = col_name,
          Outlier_Count = n_out,
          Pct_Outliers  = round(100 * n_out / length(x_clean), 2),
          stringsAsFactors = FALSE
        )
      })

      result_df <- do.call(rbind, results)
      datatable(result_df, rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE, dom = "t"),
        class = "display compact cell-border stripe hover")
    })

    # ── Hypothesis Tests ─────────────────────────────────────────────────
    ran_test   <- reactiveVal(FALSE)
    test_store <- reactiveValues(result = NULL)

    observeEvent(reset_trigger(), {
      ran_test(FALSE)
      test_store$result <- NULL
    })

    # Sub-type selector (only for t-test and Non-parametric)
    output$test_subtype_ui <- renderUI({
      req(input$test_category)
      cat <- input$test_category
      if (cat == "t-test") {
        tagList(
          tags$p(style = "color:#888; font-size:12px; margin:6px 0 2px 0;", "Sub-type"),
          selectInput(ns("test_subtype"), NULL,
            choices = c("One-sample", "Two-sample (independent)", "Paired"))
        )
      } else if (cat == "Non-parametric") {
        tagList(
          tags$p(style = "color:#888; font-size:12px; margin:6px 0 2px 0;", "Sub-type"),
          selectInput(ns("test_subtype"), NULL,
            choices = c("Mann-Whitney U", "Kruskal-Wallis"))
        )
      } else {
        NULL
      }
    })

    # Variable selectors — change dynamically by test type
    output$test_vars_ui <- renderUI({
      req(dataset(), input$test_category)
      data     <- dataset()
      cat      <- input$test_category
      sub      <- input$test_subtype
      num_cols <- names(data)[sapply(data, is.numeric)]
      cat_cols <- names(data)[!sapply(data, is.numeric)]   # for Chi-square only

      # Grouping candidates: any column with 2–20 unique non-NA values (numeric or not).
      # This covers integer-coded groups (e.g. quality 0/1, type 1/2/3) in fully numeric
      # datasets like wine quality where !is.numeric would return nothing.
      grp_n    <- vapply(data, function(x) length(unique(x[!is.na(x)])), integer(1))
      grp_cols <- names(data)[grp_n >= 2L & grp_n <= 20L]

      no_grp_msg <- p(style = "color:#888; font-size:12px;",
        "No grouping column found. Need a column with 2\u201320 unique values ",
        "(e.g. a type, class, or binary flag column).")

      if (length(num_cols) == 0 && cat != "Chi-square") {
        return(p(style = "color:#888; font-size:12px;",
                 "Dataset has no numeric columns."))
      }
      if (length(cat_cols) == 0 && cat == "Chi-square") {
        return(p(style = "color:#888; font-size:12px;",
                 "Dataset has no categorical columns."))
      }

      if (cat == "t-test") {
        sub <- if (is.null(sub)) "One-sample" else sub
        if (sub == "One-sample") {
          tagList(
            selectInput(ns("test_var1"), "Numeric column:", choices = num_cols),
            numericInput(ns("test_mu"), "Hypothesized mean (\u03bc\u2080):", value = 0)
          )
        } else if (sub == "Two-sample (independent)") {
          if (length(grp_cols) == 0) return(no_grp_msg)
          tagList(
            selectInput(ns("test_var1"), "Numeric column:", choices = num_cols),
            selectInput(ns("test_grp"), "Grouping column:", choices = grp_cols),
            uiOutput(ns("grp_level_ui"))
          )
        } else {   # Paired
          tagList(
            selectInput(ns("test_var1"), "First numeric column:", choices = num_cols),
            selectInput(ns("test_var2"), "Second numeric column:",
                        choices = num_cols,
                        selected = if (length(num_cols) > 1) num_cols[2] else num_cols[1])
          )
        }

      } else if (cat == "ANOVA") {
        if (length(grp_cols) == 0) return(no_grp_msg)
        tagList(
          selectInput(ns("test_var1"), "Numeric column:", choices = num_cols),
          selectInput(ns("test_grp"), "Grouping column:", choices = grp_cols)
        )

      } else if (cat == "Chi-square") {
        if (length(cat_cols) < 2) {
          return(p(style = "color:#888; font-size:12px;",
                   "Chi-square requires at least 2 categorical columns."))
        }
        tagList(
          selectInput(ns("test_var1"), "First categorical column:", choices = cat_cols),
          selectInput(ns("test_var2"), "Second categorical column:",
                      choices = cat_cols,
                      selected = cat_cols[2])
        )

      } else {   # Non-parametric
        sub <- if (is.null(sub)) "Mann-Whitney U" else sub
        if (sub == "Mann-Whitney U") {
          if (length(grp_cols) == 0) return(no_grp_msg)
          tagList(
            selectInput(ns("test_var1"), "Numeric column:", choices = num_cols),
            selectInput(ns("test_grp"), "Grouping column:", choices = grp_cols),
            uiOutput(ns("grp_level_ui"))
          )
        } else {   # Kruskal-Wallis
          if (length(grp_cols) == 0) return(no_grp_msg)
          tagList(
            selectInput(ns("test_var1"), "Numeric column:", choices = num_cols),
            selectInput(ns("test_grp"), "Grouping column:", choices = grp_cols)
          )
        }
      }
    })

    # Level picker — appears when grouping column has more than 2 unique values
    output$grp_level_ui <- renderUI({
      req(dataset(), input$test_grp,
          input$test_category %in% c("t-test", "Non-parametric"))
      sub <- input$test_subtype
      req(!is.null(sub),
          sub %in% c("Two-sample (independent)", "Mann-Whitney U"))

      data    <- dataset()
      grp_col <- input$test_grp
      if (!grp_col %in% names(data)) return(NULL)

      lvls <- sort(unique(as.character(data[[grp_col]][!is.na(data[[grp_col]])])))
      if (length(lvls) <= 2L) return(NULL)   # exactly 2 already — no picker needed

      tagList(
        tags$p(style = "color:#ffaa00; font-size:12px; margin:10px 0 4px;",
               paste0("\u26a0 ", length(lvls), " levels found. Pick 2 to compare:")),
        selectInput(ns("grp_lvl1"), "Group 1:", choices = lvls, selected = lvls[1]),
        selectInput(ns("grp_lvl2"), "Group 2:", choices = lvls, selected = lvls[2])
      )
    })

    # Run the selected test
    observeEvent(input$run_test_btn, {
      req(dataset(), input$test_category)
      data  <- dataset()
      cat   <- input$test_category
      sub   <- input$test_subtype
      alpha <- if (is.null(input$alpha) || is.na(input$alpha)) 0.05 else input$alpha

      # Guard: Chi-square same-column check
      if (cat == "Chi-square") {
        req(input$test_var1, input$test_var2)
        if (input$test_var1 == input$test_var2) {
          showNotification(
            "Chi-square test requires two different columns. Please select distinct variables.",
            type = "error", duration = 5)
          return()
        }
      }

      tryCatch({
        result <- list(cat = cat, sub = sub, alpha = alpha,
                       p_value = NULL, stat_df = NULL, interp = "",
                       tukey_df = NULL, cont_tbl = NULL)

        # ── t-tests ─────────────────────────────────────────────────────
        if (cat == "t-test") {
          sub <- if (is.null(sub)) "One-sample" else sub

          if (sub == "One-sample") {
            req(input$test_var1)
            x        <- na.omit(data[[input$test_var1]])
            mu       <- if (is.null(input$test_mu) || is.na(input$test_mu)) 0 else input$test_mu
            tst      <- t.test(x, mu = mu)
            result$p_value <- tst$p.value
            result$stat_df <- data.frame(
              Statistic = round(tst$statistic, 4),
              df        = round(tst$parameter, 2),
              p_value   = round(tst$p.value, 4),
              CI_lower  = round(tst$conf.int[1], 4),
              CI_upper  = round(tst$conf.int[2], 4),
              stringsAsFactors = FALSE)
            result$interp <- sprintf(
              "One-sample t-test on \u2018%s\u2019 vs. hypothesized mean %g. Sample mean = %.4f.",
              input$test_var1, mu, mean(x))
            result$plot_obj <- local({
              pd <- data.frame(x = x)
              ggplot2::ggplot(pd, ggplot2::aes(x = x)) +
                ggplot2::geom_density(fill = "#00ff00", alpha = 0.35, color = "#00ff00") +
                ggplot2::geom_vline(xintercept = mean(x), color = "#00ff00",
                                    linetype = "solid", linewidth = 1) +
                ggplot2::geom_vline(xintercept = mu, color = "#ff4444",
                                    linetype = "dashed", linewidth = 1) +
                ggplot2::annotate("text", x = mean(x), y = Inf, vjust = 2,
                                  label = paste0("mean = ", round(mean(x), 3)),
                                  color = "#00ff00", size = 3.5, hjust = -0.1) +
                ggplot2::annotate("text", x = mu, y = Inf, vjust = 2,
                                  label = paste0("\u03bc\u2080 = ", mu),
                                  color = "#ff4444", size = 3.5, hjust = 1.1) +
                .dark_theme() +
                ggplot2::labs(title = paste0("One-Sample t-test: ", input$test_var1),
                              x = input$test_var1, y = "Density")
            })

          } else if (sub == "Two-sample (independent)") {
            req(input$test_var1, input$test_grp)
            sub_df  <- na.omit(data[, c(input$test_var1, input$test_grp), drop = FALSE])
            all_lvls <- sort(unique(as.character(sub_df[[input$test_grp]])))

            # If >2 levels, filter to the two chosen by the level picker
            if (length(all_lvls) > 2L) {
              req(input$grp_lvl1, input$grp_lvl2)
              if (input$grp_lvl1 == input$grp_lvl2) {
                showNotification("Please select two different levels.", type = "error", duration = 5)
                return()
              }
              keep <- as.character(sub_df[[input$test_grp]]) %in%
                        c(input$grp_lvl1, input$grp_lvl2)
              sub_df <- sub_df[keep, , drop = FALSE]
            }

            sub_df[[input$test_grp]] <- factor(as.character(sub_df[[input$test_grp]]))
            lvls <- levels(sub_df[[input$test_grp]])
            frm  <- as.formula(paste0("`", input$test_var1, "` ~ `", input$test_grp, "`"))
            tst  <- t.test(frm, data = sub_df)
            result$p_value <- tst$p.value
            result$stat_df <- data.frame(
              Statistic = round(tst$statistic, 4),
              df        = round(tst$parameter, 2),
              p_value   = round(tst$p.value, 4),
              CI_lower  = round(tst$conf.int[1], 4),
              CI_upper  = round(tst$conf.int[2], 4),
              stringsAsFactors = FALSE)
            result$interp <- sprintf(
              "Two-sample t-test: \u2018%s\u2019 compared between groups \u2018%s\u2019 and \u2018%s\u2019.",
              input$test_var1, lvls[1], lvls[2])
            result$plot_obj <- local({
              vn <- input$test_var1; gn <- input$test_grp
              ggplot2::ggplot(sub_df,
                  ggplot2::aes(x = as.factor(.data[[gn]]), y = .data[[vn]],
                               fill = as.factor(.data[[gn]]))) +
                ggplot2::geom_boxplot(alpha = 0.7, color = "#888888") +
                ggplot2::scale_fill_manual(values = c("#00ff00", "#00bfff")) +
                .dark_theme() +
                ggplot2::labs(title = paste0("Two-Sample t-test: ", vn, " by ", gn),
                              x = gn, y = vn, fill = gn)
            })

          } else {   # Paired
            req(input$test_var1, input$test_var2)
            df_pair <- na.omit(data[, c(input$test_var1, input$test_var2), drop = FALSE])
            tst     <- t.test(df_pair[[input$test_var1]], df_pair[[input$test_var2]], paired = TRUE)
            result$p_value <- tst$p.value
            result$stat_df <- data.frame(
              Statistic = round(tst$statistic, 4),
              df        = round(tst$parameter, 2),
              p_value   = round(tst$p.value, 4),
              CI_lower  = round(tst$conf.int[1], 4),
              CI_upper  = round(tst$conf.int[2], 4),
              stringsAsFactors = FALSE)
            result$interp <- sprintf(
              "Paired t-test comparing \u2018%s\u2019 and \u2018%s\u2019.",
              input$test_var1, input$test_var2)
            result$plot_obj <- local({
              n1 <- input$test_var1; n2 <- input$test_var2
              diffs <- df_pair[[n1]] - df_pair[[n2]]
              pd <- data.frame(diff = diffs)
              ggplot2::ggplot(pd, ggplot2::aes(x = diff)) +
                ggplot2::geom_density(fill = "#00ff00", alpha = 0.35, color = "#00ff00") +
                ggplot2::geom_vline(xintercept = 0, color = "#ff4444",
                                    linetype = "dashed", linewidth = 1) +
                ggplot2::geom_vline(xintercept = mean(diffs), color = "#00ff00",
                                    linetype = "solid", linewidth = 0.8) +
                ggplot2::annotate("text", x = mean(diffs), y = Inf, vjust = 2,
                                  label = paste0("mean diff = ", round(mean(diffs), 3)),
                                  color = "#00ff00", size = 3.5, hjust = -0.1) +
                .dark_theme() +
                ggplot2::labs(title = paste0("Paired t-test: ", n1, " \u2212 ", n2),
                              x = "Difference", y = "Density")
            })
          }

        # ── ANOVA + Tukey ────────────────────────────────────────────────
        } else if (cat == "ANOVA") {
          req(input$test_var1, input$test_grp)
          df_a <- na.omit(data[, c(input$test_var1, input$test_grp), drop = FALSE])
          df_a[[input$test_grp]] <- as.factor(df_a[[input$test_grp]])
          frm     <- as.formula(paste0("`", input$test_var1, "` ~ `", input$test_grp, "`"))
          aov_obj <- aov(frm, data = df_a)
          aov_sum <- summary(aov_obj)[[1]]
          f_stat  <- round(aov_sum[["F value"]][1], 4)
          p_val   <- round(aov_sum[["Pr(>F)"]][1], 4)
          df_num  <- aov_sum[["Df"]][1]
          df_den  <- aov_sum[["Df"]][2]

          tukey_raw <- TukeyHSD(aov_obj)
          tukey_key <- names(tukey_raw)[1]

          result$p_value  <- aov_sum[["Pr(>F)"]][1]
          result$stat_df  <- data.frame(
            F_statistic = f_stat,
            df_between  = df_num,
            df_within   = df_den,
            p_value     = p_val,
            stringsAsFactors = FALSE)
          result$tukey_df <- as.data.frame(round(tukey_raw[[tukey_key]], 4))
          result$interp   <- sprintf(
            "One-way ANOVA: effect of \u2018%s\u2019 on \u2018%s\u2019. F(%d, %d) = %.4f.",
            input$test_grp, input$test_var1, df_num, df_den, f_stat)
          result$plot_obj <- local({
            vn <- input$test_var1; gn <- input$test_grp
            ggplot2::ggplot(df_a,
                ggplot2::aes(x = as.factor(.data[[gn]]), y = .data[[vn]],
                             fill = as.factor(.data[[gn]]))) +
              ggplot2::geom_boxplot(alpha = 0.7, color = "#888888") +
              ggplot2::scale_fill_viridis_d(option = "D", begin = 0.1) +
              .dark_theme() +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                             legend.position = "none") +
              ggplot2::labs(title = paste0("ANOVA: ", vn, " by ", gn), x = gn, y = vn)
          })

        # ── Chi-square ───────────────────────────────────────────────────
        } else if (cat == "Chi-square") {
          req(input$test_var1, input$test_var2)
          tbl <- table(data[[input$test_var1]], data[[input$test_var2]])
          tst <- chisq.test(tbl)
          result$p_value  <- tst$p.value
          result$stat_df  <- data.frame(
            Chi_squared = round(tst$statistic, 4),
            df          = tst$parameter,
            p_value     = round(tst$p.value, 4),
            stringsAsFactors = FALSE)
          result$cont_tbl <- as.data.frame.matrix(tbl)
          result$interp   <- sprintf(
            "Chi-square test of independence between \u2018%s\u2019 and \u2018%s\u2019.",
            input$test_var1, input$test_var2)
          result$plot_obj <- local({
            v1 <- input$test_var1; v2 <- input$test_var2
            tbl_long <- as.data.frame(tbl)
            names(tbl_long) <- c(v1, v2, "Count")
            ggplot2::ggplot(tbl_long,
                ggplot2::aes(x = .data[[v1]], y = Count, fill = .data[[v2]])) +
              ggplot2::geom_bar(stat = "identity", position = "dodge",
                                color = "black", alpha = 0.85) +
              ggplot2::scale_fill_viridis_d(option = "D", begin = 0.1) +
              .dark_theme() +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
              ggplot2::labs(title = paste0("Chi-square: ", v1, " vs ", v2),
                            x = v1, y = "Count", fill = v2)
          })

        # ── Non-parametric ───────────────────────────────────────────────
        } else {
          sub <- if (is.null(sub)) "Mann-Whitney U" else sub

          if (sub == "Mann-Whitney U") {
            req(input$test_var1, input$test_grp)
            df_np    <- na.omit(data[, c(input$test_var1, input$test_grp), drop = FALSE])
            all_lvls <- sort(unique(as.character(df_np[[input$test_grp]])))

            # If >2 levels, filter to the two chosen by the level picker
            if (length(all_lvls) > 2L) {
              req(input$grp_lvl1, input$grp_lvl2)
              if (input$grp_lvl1 == input$grp_lvl2) {
                showNotification("Please select two different levels.", type = "error", duration = 5)
                return()
              }
              keep  <- as.character(df_np[[input$test_grp]]) %in%
                         c(input$grp_lvl1, input$grp_lvl2)
              df_np <- df_np[keep, , drop = FALSE]
            }

            df_np[[input$test_grp]] <- factor(as.character(df_np[[input$test_grp]]))
            lvls <- levels(df_np[[input$test_grp]])
            frm  <- as.formula(paste0("`", input$test_var1, "` ~ `", input$test_grp, "`"))
            tst  <- wilcox.test(frm, data = df_np)
            result$p_value <- tst$p.value
            result$stat_df <- data.frame(
              W_statistic = round(tst$statistic, 4),
              p_value     = round(tst$p.value, 4),
              stringsAsFactors = FALSE)
            result$interp <- sprintf(
              "Mann-Whitney U test: \u2018%s\u2019 between \u2018%s\u2019 and \u2018%s\u2019.",
              input$test_var1, lvls[1], lvls[2])
            result$plot_obj <- local({
              vn <- input$test_var1; gn <- input$test_grp
              ggplot2::ggplot(df_np,
                  ggplot2::aes(x = as.factor(.data[[gn]]), y = .data[[vn]],
                               fill = as.factor(.data[[gn]]))) +
                ggplot2::geom_boxplot(alpha = 0.7, color = "#888888") +
                ggplot2::scale_fill_manual(values = c("#00ff00", "#00bfff")) +
                .dark_theme() +
                ggplot2::labs(title = paste0("Mann-Whitney U: ", vn, " by ", gn),
                              x = gn, y = vn, fill = gn)
            })

          } else {   # Kruskal-Wallis
            req(input$test_var1, input$test_grp)
            df_np <- na.omit(data[, c(input$test_var1, input$test_grp), drop = FALSE])
            frm   <- as.formula(paste0("`", input$test_var1, "` ~ `", input$test_grp, "`"))
            tst   <- kruskal.test(frm, data = df_np)
            result$p_value <- tst$p.value
            result$stat_df <- data.frame(
              Chi_squared = round(tst$statistic, 4),
              df          = tst$parameter,
              p_value     = round(tst$p.value, 4),
              stringsAsFactors = FALSE)
            result$interp <- sprintf(
              "Kruskal-Wallis test: effect of \u2018%s\u2019 on \u2018%s\u2019.",
              input$test_grp, input$test_var1)
            result$plot_obj <- local({
              vn <- input$test_var1; gn <- input$test_grp
              ggplot2::ggplot(df_np,
                  ggplot2::aes(x = as.factor(.data[[gn]]), y = .data[[vn]],
                               fill = as.factor(.data[[gn]]))) +
                ggplot2::geom_boxplot(alpha = 0.7, color = "#888888") +
                ggplot2::scale_fill_viridis_d(option = "D", begin = 0.1) +
                .dark_theme() +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                               legend.position = "none") +
                ggplot2::labs(title = paste0("Kruskal-Wallis: ", vn, " by ", gn),
                              x = gn, y = vn)
            })
          }
        }

        # Capture test parameters explicitly for the report
        result$params <- Filter(Negate(is.null), list(
          "Test"             = paste0(cat, if (!is.null(sub) && nchar(sub) > 0)
                                           paste0(" \u2014 ", sub) else ""),
          "Variable"         = if (!is.null(input$test_var1)) input$test_var1 else NULL,
          "Variable 2"       = if (cat %in% c("Chi-square") ||
                                   (cat == "t-test" && !is.null(sub) && sub == "Paired"))
                                 input$test_var2 else NULL,
          "Group by"         = if (cat %in% c("ANOVA") ||
                                   (cat == "t-test" && !is.null(sub) &&
                                    sub == "Two-sample (independent)") ||
                                   (cat == "Non-parametric"))
                                 input$test_grp else NULL,
          "Groups compared"  = if (!is.null(input$grp_lvl1) && !is.null(input$grp_lvl2))
                                 paste0(input$grp_lvl1, " vs ", input$grp_lvl2) else NULL,
          "H\u2080 mean (\u03bc\u2080)" = if (cat == "t-test" &&
                                              (is.null(sub) || sub == "One-sample"))
                                 as.character(input$test_mu %||% 0) else NULL,
          "\u03b1 (significance)" = as.character(alpha)
        ))

        test_store$result <- result
        ran_test(TRUE)
        if (!is.null(report_rv)) {
          n_prev <- length(report_rv$hyp_results %||% list())
          lbl <- paste0(
            n_prev + 1L, ". ", cat,
            if (!is.null(sub) && nchar(sub) > 0) paste0(" \u2014 ", sub) else "",
            ": ", (result$params[["Variable"]] %||% ""))
          cur_list           <- report_rv$hyp_results %||% list()
          cur_list[[lbl]]    <- result
          report_rv$hyp_results <- cur_list
        }

      }, error = function(e) {
        showNotification(paste("Test failed:", conditionMessage(e)),
                         type = "error", duration = 6)
      })
    })

    # Results panel
    output$test_results <- renderUI({
      if (!ran_test()) {
        return(div(style = "color:#555; margin-top:60px; text-align:center; font-size:15px;",
                   "Configure the test on the left and click \u201cRun Test\u201d."))
      }

      res    <- test_store$result
      pval   <- res$p_value
      alpha  <- res$alpha
      reject <- !is.null(pval) && !is.na(pval) && pval < alpha

      badge_bg  <- if (reject) "#001a00" else "#1a1a1a"
      badge_bdr <- if (reject) "#00ff00" else "#888888"
      badge_clr <- if (reject) "#00ff00" else "#aaaaaa"
      badge_lbl <- if (reject) "Reject H\u2080" else "Fail to reject H\u2080"
      badge_exp <- if (!is.null(pval) && !is.na(pval)) {
        if (reject)
          paste0("p = ", round(pval, 4), " < \u03b1 = ", alpha,
                 ". Statistically significant evidence against H\u2080.")
        else
          paste0("p = ", round(pval, 4), " \u2265 \u03b1 = ", alpha,
                 ". Insufficient evidence to reject H\u2080.")
      } else ""

      extra <- NULL
      if (res$cat == "ANOVA" && !is.null(res$tukey_df)) {
        extra <- tagList(
          hr(style = "border-color:#333; margin-top:18px;"),
          tags$p(style = "color:#00ff00; font-weight:bold;", "Tukey HSD Pairwise Comparisons:"),
          DTOutput(ns("tukey_table"))
        )
      } else if (res$cat == "Chi-square" && !is.null(res$cont_tbl)) {
        extra <- tagList(
          hr(style = "border-color:#333; margin-top:18px;"),
          tags$p(style = "color:#00ff00; font-weight:bold;", "Contingency Table (Observed Counts):"),
          DTOutput(ns("cont_table"))
        )
      }

      tagList(
        tags$p(style = "color:#888; font-size:13px; margin-bottom:10px;", res$interp),
        DTOutput(ns("stat_table")),
        br(),
        div(style = paste0("background:", badge_bg, "; border:1px solid ", badge_bdr,
                           "; border-radius:6px; padding:10px 16px; margin:10px 0;"),
          tags$strong(style = paste0("color:", badge_clr, "; font-size:16px;"), badge_lbl),
          br(),
          tags$span(style = "color:#aaa; font-size:13px;", badge_exp)
        ),
        extra
      )
    })

    output$stat_table <- renderDT({
      req(ran_test(), !is.null(test_store$result$stat_df))
      datatable(test_store$result$stat_df, rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE),
        class = "display compact cell-border")
    })

    output$tukey_table <- renderDT({
      req(ran_test(), !is.null(test_store$result$tukey_df))
      df <- test_store$result$tukey_df
      df$Comparison <- rownames(df)
      df <- df[, c("Comparison", setdiff(names(df), "Comparison")), drop = FALSE]
      datatable(df, rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE),
        class = "display compact cell-border stripe")
    })

    output$cont_table <- renderDT({
      req(ran_test(), !is.null(test_store$result$cont_tbl))
      datatable(test_store$result$cont_tbl, rownames = TRUE,
        options = list(dom = "t", scrollX = TRUE),
        class = "display compact cell-border stripe")
    })
  })
}
