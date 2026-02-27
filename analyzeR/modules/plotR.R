# Module UI
plotRUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),

    # ── Plot type ────────────────────────────────────────────────────────────
    selectInput(ns("plot_type"), "Plot Type",
      choices = c("Scatter", "Histogram", "Bar Chart", "Box Plot")),

    # ── Variable selectors (conditional on plot type) ─────────────────────
    conditionalPanel(
      condition = paste0("input['", ns("plot_type"), "'] == 'Scatter' || input['",
                         ns("plot_type"), "'] == 'Box Plot'"),
      selectInput(ns("x_var"), "X Variable", choices = NULL),
      selectInput(ns("y_var"), "Y Variable", choices = NULL)
    ),
    conditionalPanel(
      condition = paste0("input['", ns("plot_type"), "'] == 'Histogram' || input['",
                         ns("plot_type"), "'] == 'Bar Chart'"),
      selectInput(ns("single_var"), "Variable", choices = NULL)
    ),
    conditionalPanel(
      condition = paste0("input['", ns("plot_type"), "'] == 'Histogram'"),
      sliderInput(ns("bins"), "Bins", min = 5, max = 100, value = 30)
    ),

    hr(),

    # ── Facet options ────────────────────────────────────────────────────────
    h5("Facet Options"),
    fluidRow(
      column(6,
        selectInput(ns("facet_var"), "Facet By",
          choices  = c("None" = ""),
          selected = "")
      ),
      column(6,
        selectInput(ns("facet_scales"), "Facet Scales",
          choices  = c("Fixed" = "fixed", "Free Y" = "free_y",
                       "Free X" = "free_x", "Free (both)" = "free"),
          selected = "fixed")
      )
    ),

    # ── Sample options (only rendered when dataset > 1000 rows) ─────────────
    uiOutput(ns("sample_ui")),

    br(),
    actionButton(ns("generate_plot"), "Generate Plot", class = "btn-primary"),
    br(), br(),
    plotOutput(ns("plot_output"), height = "450px")
  )
}

# ── Module Server ─────────────────────────────────────────────────────────────
plotRServer <- function(input, output, session, dataset) {
  ns <- session$ns

  # Show sample slider only for large datasets (> 1000 rows)
  output$sample_ui <- renderUI({
    req(dataset())
    n <- nrow(dataset())
    if (n <= 1000) return(NULL)
    tagList(
      sliderInput(ns("sample_pct"),
        label    = paste0("Sample % of Dataset (", n, " rows)"),
        min = 5, max = 100, value = 100, step = 5,
        post = "%"),
      div(
        style = "color: #888; font-size: 12px; margin-top: -8px; margin-bottom: 10px;",
        textOutput(ns("sample_info_text"))
      )
    )
  })

  output$sample_info_text <- renderText({
    req(dataset())
    n_total <- nrow(dataset())
    pct     <- if (is.null(input$sample_pct)) 100 else input$sample_pct
    n_use   <- if (pct < 100) max(10L, floor(n_total * pct / 100L)) else n_total
    if (pct < 100) paste0("Using ", n_use, " of ", n_total, " rows")
    else           paste0("Using all ", n_total, " rows")
  })

  # ── Keep ALL selectors fresh whenever dataset or plot type changes ─────────
  observe({
    req(dataset())
    data     <- dataset()
    num_vars <- names(data)[sapply(data, is.numeric)]
    all_vars <- names(data)
    ptype    <- if (is.null(input$plot_type)) "Scatter" else input$plot_type

    # x_var / y_var (Scatter, Box Plot)
    updateSelectInput(session, "x_var", choices = all_vars,
      selected = if (length(all_vars) >= 1) all_vars[1] else NULL)
    updateSelectInput(session, "y_var", choices = num_vars,
      selected = if (length(num_vars) >= 2) num_vars[2]
                 else if (length(num_vars) >= 1) num_vars[1] else NULL)

    # single_var (Histogram → numeric only, Bar Chart → all)
    single_choices <- if (ptype == "Histogram") num_vars else all_vars
    updateSelectInput(session, "single_var", choices = single_choices,
      selected = if (length(single_choices) >= 1) single_choices[1] else NULL)

    # facet_var — all columns as options
    updateSelectInput(session, "facet_var",
      choices  = c("None" = "", all_vars),
      selected = "")
  })

  # ── Build plot on button click ────────────────────────────────────────────
  plot_obj <- eventReactive(input$generate_plot, {
    req(dataset())
    data  <- dataset()
    ptype <- input$plot_type

    # --- Sample if requested (only possible when slider is shown) -----------
    n_total <- nrow(data)
    pct     <- if (is.null(input$sample_pct)) 100 else input$sample_pct
    n_use   <- if (pct < 100) max(10L, floor(n_total * pct / 100L)) else n_total
    if (n_use < n_total) {
      set.seed(42)
      data <- data[sample(n_total, n_use), ]
    }

    # --- Base plot by type --------------------------------------------------
    p <- if (ptype == "Scatter") {
      req(input$x_var, input$y_var)
      validate(
        need(input$x_var %in% names(data), "X variable not found in dataset."),
        need(input$y_var %in% names(data), "Y variable not found in dataset.")
      )
      ggplot2::ggplot(data,
          ggplot2::aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
        ggplot2::geom_point(alpha = 0.6, color = "#00ff00") +
        ggdark::dark_theme_minimal() +
        ggplot2::labs(title = paste("Scatter:", input$x_var, "vs", input$y_var))

    } else if (ptype == "Histogram") {
      req(input$single_var)
      validate(need(input$single_var %in% names(data), "Variable not found in dataset."))
      ggplot2::ggplot(data, ggplot2::aes(x = .data[[input$single_var]])) +
        ggplot2::geom_histogram(bins = input$bins, fill = "#00ff00",
                                color = "black", alpha = 0.85) +
        ggdark::dark_theme_minimal() +
        ggplot2::labs(title = paste("Histogram of", input$single_var))

    } else if (ptype == "Bar Chart") {
      req(input$single_var)
      validate(need(input$single_var %in% names(data), "Variable not found in dataset."))
      ggplot2::ggplot(data, ggplot2::aes(x = .data[[input$single_var]])) +
        ggplot2::geom_bar(fill = "#00ff00", color = "black", alpha = 0.85) +
        ggdark::dark_theme_minimal() +
        ggplot2::labs(title = paste("Bar Chart of", input$single_var)) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    } else if (ptype == "Box Plot") {
      req(input$x_var, input$y_var)
      validate(
        need(input$x_var %in% names(data), "X variable not found in dataset."),
        need(input$y_var %in% names(data), "Y variable not found in dataset.")
      )
      ggplot2::ggplot(data,
          ggplot2::aes(x = as.factor(.data[[input$x_var]]),
                       y = .data[[input$y_var]])) +
        ggplot2::geom_boxplot(fill = "#1a1a1a", color = "#00ff00") +
        ggdark::dark_theme_minimal() +
        ggplot2::labs(
          title = paste("Box Plot:", input$y_var, "by", input$x_var),
          x     = input$x_var
        ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }

    # --- Add facet if a variable is selected --------------------------------
    fv <- input$facet_var
    if (!is.null(fv) && nchar(fv) > 0 && fv %in% names(data)) {
      p <- p +
        ggplot2::facet_wrap(
          ggplot2::vars(.data[[fv]]),
          scales = input$facet_scales
        ) +
        ggplot2::theme(
          strip.background = ggplot2::element_rect(fill = "#0d1a0d", color = "#00ff00"),
          strip.text       = ggplot2::element_text(color = "#00ff00",
                                                   size  = 11,
                                                   face  = "bold")
        )
    }

    p
  })

  output$plot_output <- renderPlot({
    plot_obj()
  })
}
