# Module UI
analyzeRUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    actionButton(ns("analyze_btn"), "Run Analysis", class = "btn-primary"),
    br(), br(),
    tabsetPanel(
      tabPanel("Correlation",
        br(),
        plotOutput(ns("corr_heatmap"), height = "400px")
      ),
      tabPanel("Normality Tests",
        br(),
        DTOutput(ns("normality_table"))
      ),
      tabPanel("Outlier Detection",
        br(),
        DTOutput(ns("outlier_table"))
      )
    )
  )
}

# Module Server
analyzeRServer <- function(input, output, session, dataset) {
  ns <- session$ns

  analysis_data <- eventReactive(input$analyze_btn, {
    req(dataset())
    dataset()
  })

  output$corr_heatmap <- renderPlot({
    req(analysis_data())
    data     <- analysis_data()
    num_cols <- data[, sapply(data, is.numeric), drop = FALSE]

    # Return an informative ggplot when there aren't enough numeric columns
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
      ggdark::dark_theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  })

  output$normality_table <- renderDT({
    req(analysis_data())
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
    req(analysis_data())
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
}
