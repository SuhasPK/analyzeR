# ── File-scope helper: build the right ggplot2 color/fill scale ───────────────
.make_color_scale <- function(cv, data, palette, is_fill) {
  is_disc <- !is.numeric(data[[cv]])
  vir_map  <- c(Viridis = "D", Plasma = "C", Magma = "A", Inferno = "B", Cividis = "E")
  brewer_q <- c("Dark2", "Set1", "Set2", "Accent", "Spectral")

  if (palette %in% names(vir_map)) {
    opt <- vir_map[[palette]]
    if (is_fill) {
      if (is_disc) ggplot2::scale_fill_viridis_d(option = opt, begin = 0.1)
      else         ggplot2::scale_fill_viridis_c(option = opt, begin = 0.1)
    } else {
      if (is_disc) ggplot2::scale_color_viridis_d(option = opt, begin = 0.1)
      else         ggplot2::scale_color_viridis_c(option = opt, begin = 0.1)
    }
  } else if (!is_disc && palette %in% brewer_q) {
    # Qualitative palette on continuous → fallback to Viridis
    if (is_fill) ggplot2::scale_fill_viridis_c()
    else         ggplot2::scale_color_viridis_c()
  } else {
    if (is_fill) {
      if (is_disc) ggplot2::scale_fill_brewer(palette = palette)
      else         ggplot2::scale_fill_distiller(palette = palette)
    } else {
      if (is_disc) ggplot2::scale_color_brewer(palette = palette)
      else         ggplot2::scale_color_distiller(palette = palette)
    }
  }
}

# ── Helper: apply a list of filters to a data frame ───────────────────────────
.apply_plot_filters <- function(data, filter_ids, input) {
  for (fid in filter_ids) {
    col <- input[[paste0("fcol_", fid)]]
    if (is.null(col) || nchar(col) == 0 || !col %in% names(data)) next

    x <- data[[col]]
    if (is.numeric(x)) {
      rng <- input[[paste0("frange_", fid)]]
      if (!is.null(rng) && length(rng) == 2)
        data <- data[!is.na(x) & x >= rng[1] & x <= rng[2], , drop = FALSE]
    } else {
      lvls <- input[[paste0("flevels_", fid)]]
      if (!is.null(lvls) && length(lvls) > 0)
        data <- data[as.character(x) %in% lvls, , drop = FALSE]
    }
  }
  data
}


# ── Module UI ──────────────────────────────────────────────────────────────────
plotRUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    uiOutput(ns("tab_guard")),

    # ── Plot type ────────────────────────────────────────────────────────
    selectInput(ns("plot_type"), "Plot Type",
      choices = c("Scatter", "Histogram", "Bar Chart", "Box Plot")),

    # ── Variable selectors (conditional on plot type) ────────────────
    conditionalPanel(
      condition = paste0("input['", ns("plot_type"), "'] == 'Scatter' || input['",
                         ns("plot_type"), "'] == 'Box Plot'"),
      selectInput(ns("x_var"), "X / Group Variable", choices = NULL),
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

    # ── Color options ────────────────────────────────────────────────────
    h5("Color Options"),
    fluidRow(
      column(6,
        selectInput(ns("color_var"), "Color By",
          choices = c("None" = ""), selected = "")
      ),
      column(6,
        uiOutput(ns("color_palette_ui"))
      )
    ),

    hr(),

    # ── FilterR ──────────────────────────────────────────────────────────
    h5("FilterR"),
    p(style = "color:#666; font-size:12px; margin:-4px 0 10px;",
      "Filters are applied to the data before plotting."),
    uiOutput(ns("filter_rows_ui")),
    fluidRow(
      column(6,
        actionButton(ns("add_filter"), "Filter +",
                     class = "btn-secondary btn-sm", style = "width:100%;")
      ),
      column(6,
        actionButton(ns("clear_filters"), "Clear All",
                     class = "btn-danger btn-sm", style = "width:100%;")
      )
    ),
    uiOutput(ns("filter_status_ui")),

    hr(),

    # ── Facet options ─────────────────────────────────────────────────────
    h5("Facet Options"),
    selectInput(ns("facet_scales"), "Facet Scales",
      choices  = c("Fixed" = "fixed", "Free Y" = "free_y",
                   "Free X" = "free_x", "Free (both)" = "free"),
      selected = "fixed"),
    uiOutput(ns("facet_rows_ui")),
    fluidRow(
      column(6,
        actionButton(ns("add_facet"), "Facet +",
                     class = "btn-secondary btn-sm", style = "width:100%;")
      ),
      column(6,
        actionButton(ns("rm_facet"), "Facet \u2212",
                     class = "btn-danger btn-sm", style = "width:100%;")
      )
    ),

    # ── Sample options ────────────────────────────────────────────────────
    uiOutput(ns("sample_ui")),

    hr(),

    # ── Plot title ────────────────────────────────────────────────────────
    h5("Plot Title"),
    textInput(ns("plot_title"), NULL, value = "",
              placeholder = "Leave blank to use auto-generated title"),

    br(),
    actionButton(ns("generate_plot"), "Generate Plot", class = "btn-primary"),
    br(), br(),
    withSpinner(plotOutput(ns("plot_output"), height = "450px"))
  )
}


# ── Module Server ──────────────────────────────────────────────────────────────
plotRServer <- function(id, dataset, reset_trigger, report_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Closure-scoped trackers — plain variables, modified with <<-
    reg_rm_obs      <- character(0)   # filter IDs whose remove-observer is registered
    reg_val_outputs <- character(0)   # filter IDs whose value-renderUI is registered

    # ── No-data guard ────────────────────────────────────────────────
    output$tab_guard <- renderUI({
      if (!is.null(dataset())) return(NULL)
      .no_data_ui()
    })

    observe({
      if (is.null(dataset())) shinyjs::disable("generate_plot")
      else                    shinyjs::enable("generate_plot")
    })

    show_plot    <- reactiveVal(FALSE)
    n_facets     <- reactiveVal(1L)
    plot_counter <- reactiveVal(0L)
    filter_ids   <- reactiveVal(integer(0))
    filter_next  <- reactiveVal(1L)

    # ── Reset all state ──────────────────────────────────────────────
    observeEvent(reset_trigger(), {
      show_plot(FALSE)
      n_facets(1L)
      plot_counter(0L)
      filter_ids(integer(0))
      filter_next(1L)
    })

    # ── Generate button ──────────────────────────────────────────────
    observeEvent(input$generate_plot, {
      req(dataset())
      withProgress(message = "Generating plot...", value = 0, {
        setProgress(0.5, detail = "Rendering")
        setProgress(1.0)
      })
      show_plot(TRUE)
    })

    # ── Facet controls ───────────────────────────────────────────────
    observeEvent(input$add_facet, {
      if (n_facets() < 3L)
        n_facets(n_facets() + 1L)
      else
        showNotification("Maximum 3 facets supported.", type = "warning", duration = 3)
    })
    observeEvent(input$rm_facet, {
      if (n_facets() > 1L) n_facets(n_facets() - 1L)
    })

    # ── Filter controls ──────────────────────────────────────────────
    observeEvent(input$add_filter, {
      req(dataset())
      fid <- filter_next()
      filter_ids(c(filter_ids(), fid))
      filter_next(fid + 1L)
    })

    observeEvent(input$clear_filters, {
      filter_ids(integer(0))
    })

    # Register a remove-observer for each new filter ID (once per ID)
    observe({
      ids <- filter_ids()
      new_ids <- setdiff(as.character(ids), reg_rm_obs)
      lapply(new_ids, function(sfid) {
        fid <- as.integer(sfid)
        local({
          lid <- fid
          observeEvent(input[[paste0("rm_filter_", lid)]], {
            filter_ids(setdiff(filter_ids(), lid))
          }, ignoreInit = TRUE, once = TRUE)
        })
        reg_rm_obs <<- c(reg_rm_obs, sfid)
      })
    })

    # Register a value-renderUI for each new filter ID (once per ID)
    observe({
      ids <- filter_ids()
      new_ids <- setdiff(as.character(ids), reg_val_outputs)
      lapply(new_ids, function(sfid) {
        fid <- as.integer(sfid)
        local({
          lid <- fid
          output[[paste0("fval_", lid)]] <- renderUI({
            req(dataset())
            col_sel <- input[[paste0("fcol_", lid)]]
            if (is.null(col_sel) || col_sel == "" || !col_sel %in% names(dataset()))
              return(p(style = "color:#555; font-size:12px; margin:4px 0 0;",
                       "Select a column above to configure the filter."))

            x <- dataset()[[col_sel]]
            if (is.numeric(x)) {
              rng <- range(x, na.rm = TRUE)
              if (rng[1] == rng[2]) rng[2] <- rng[1] + 1
              stp <- signif(diff(rng) / 100, 2)
              sliderInput(ns(paste0("frange_", lid)), NULL,
                min   = rng[1], max = rng[2],
                value = rng,    step = max(stp, .Machine$double.eps))
            } else {
              lvls <- sort(unique(as.character(x[!is.na(x)])))
              if (length(lvls) <= 10) {
                checkboxGroupInput(ns(paste0("flevels_", lid)), NULL,
                  choices  = lvls, selected = lvls, inline = TRUE)
              } else {
                selectInput(ns(paste0("flevels_", lid)), NULL,
                  choices   = lvls, selected  = lvls,
                  multiple  = TRUE, selectize = TRUE)
              }
            }
          })
        })
        reg_val_outputs <<- c(reg_val_outputs, sfid)
      })
    })

    # ── Filter card rows ─────────────────────────────────────────────
    output$filter_rows_ui <- renderUI({
      ids <- filter_ids()
      if (length(ids) == 0) return(NULL)
      req(dataset())
      all_vars <- names(dataset())

      rows <- lapply(ids, function(fid) {
        col_id  <- ns(paste0("fcol_", fid))
        cur_col <- isolate(input[[paste0("fcol_", fid)]])
        sel_col <- if (!is.null(cur_col) && cur_col %in% c("", all_vars)) cur_col else ""

        div(
          style = paste0(
            "margin-bottom:10px; padding:10px 12px; ",
            "background:#080808; border:1px solid #2a2a2a; ",
            "border-left:3px solid #00ff00; border-radius:6px;"
          ),
          fluidRow(
            column(9,
              selectInput(col_id, NULL,
                choices  = c("Select column..." = "", all_vars),
                selected = sel_col)
            ),
            column(3,
              div(style = "margin-top:5px;",
                actionButton(ns(paste0("rm_filter_", fid)), "\u00d7",
                  class = "btn-danger btn-sm",
                  style = "width:100%; font-weight:bold;"))
            )
          ),
          uiOutput(ns(paste0("fval_", fid)))
        )
      })
      do.call(tagList, rows)
    })

    # ── Filter status bar ─────────────────────────────────────────────
    output$filter_status_ui <- renderUI({
      ids <- filter_ids()
      if (length(ids) == 0 || is.null(dataset())) return(NULL)

      n_before <- nrow(dataset())
      n_after  <- nrow(.apply_plot_filters(dataset(), ids, input))
      pct      <- round(100 * n_after / n_before)

      clr <- if (n_after == 0) "#ff4444"
             else if (n_after < n_before) "#ffcc00"
             else "#00cc44"
      div(
        style = paste0("margin:8px 0 2px; padding:5px 10px; font-size:12px; color:", clr, ";",
                       " background:#0a0a0a; border-radius:4px; border:1px solid #1e1e1e;"),
        icon_chr <- if (n_after == 0) "\u26a0" else if (n_after < n_before) "\u2192" else "\u2713",
        paste0(icon_chr, "  ",
               format(n_after, big.mark = ","), " of ",
               format(n_before, big.mark = ","),
               " rows pass active filters (", pct, "%)")
      )
    })

    # ── Dynamic color palette selector ────────────────────────────────
    output$color_palette_ui <- renderUI({
      cv <- input$color_var
      if (is.null(cv) || cv == "") {
        selectInput(ns("accent_color"), "Accent Color",
          choices = c(
            "Lime Green"  = "#00ff00",
            "Cyan"        = "#00ccff",
            "Orange"      = "#ff6600",
            "Gold"        = "#ffcc00",
            "Purple"      = "#cc44ff",
            "Hot Pink"    = "#ff44aa",
            "Crimson"     = "#ff4444",
            "White"       = "#eeeeee"
          ),
          selected = "#00ff00"
        )
      } else {
        selectInput(ns("color_palette"), "Palette",
          choices = list(
            "Viridis Family (any data type)" = list(
              "Viridis"  = "Viridis",
              "Plasma"   = "Plasma",
              "Magma"    = "Magma",
              "Inferno"  = "Inferno",
              "Cividis"  = "Cividis"
            ),
            "ColorBrewer (best for categorical)" = list(
              "Dark2"    = "Dark2",
              "Set1"     = "Set1",
              "Set2"     = "Set2",
              "Accent"   = "Accent",
              "Spectral" = "Spectral"
            )
          ),
          selected = "Dark2"
        )
      }
    })

    # ── Dynamic facet row selectors ───────────────────────────────────
    output$facet_rows_ui <- renderUI({
      req(dataset())
      all_vars <- names(dataset())
      nf       <- n_facets()
      rows <- lapply(seq_len(nf), function(i) {
        fid  <- ns(paste0("facet_var_", i))
        cur  <- isolate(input[[paste0("facet_var_", i)]])
        choices <- c("None" = "", all_vars)
        sel  <- if (!is.null(cur) && cur %in% choices) cur else ""
        selectInput(fid, paste0("Facet ", i, ":"), choices = choices, selected = sel)
      })
      do.call(tagList, rows)
    })

    # ── Mirror plot into report_rv ────────────────────────────────────
    observeEvent(plot_obj(), {
      if (!is.null(report_rv)) {
        ptype <- input$plot_type
        label_suffix <- switch(ptype,
          "Scatter"   = paste0(input$x_var, " vs ", input$y_var),
          "Histogram" = input$single_var,
          "Bar Chart" = input$single_var,
          "Box Plot"  = paste0(input$y_var, " by ", input$x_var),
          ""
        )
        plot_counter(plot_counter() + 1L)
        key      <- paste0(plot_counter(), ". ", ptype, ": ", label_suffix)
        cur_list <- report_rv$plot_list
        if (is.null(cur_list)) cur_list <- list()
        cur_list[[key]] <- list(obj = plot_obj(), type = ptype)
        report_rv$plot_list <- cur_list
      }
    }, ignoreNULL = TRUE)

    # ── Sample slider (large datasets) ───────────────────────────────
    output$sample_ui <- renderUI({
      req(dataset())
      n <- nrow(dataset())
      if (n <= 1000) return(NULL)
      tagList(
        sliderInput(ns("sample_pct"),
          label = paste0("Sample % of Dataset (", format(n, big.mark = ","), " rows)"),
          min = 5, max = 100, value = 100, step = 5, post = "%"),
        div(style = "color:#888; font-size:12px; margin-top:-8px; margin-bottom:10px;",
            textOutput(ns("sample_info_text")))
      )
    })

    output$sample_info_text <- renderText({
      req(dataset())
      n_total <- nrow(dataset())
      pct     <- if (is.null(input$sample_pct)) 100 else input$sample_pct
      n_use   <- if (pct < 100) max(10L, floor(n_total * pct / 100L)) else n_total
      if (pct < 100) paste0("Using ", format(n_use, big.mark = ","),
                             " of ", format(n_total, big.mark = ","), " rows")
      else           paste0("Using all ", format(n_total, big.mark = ","), " rows")
    })

    # ── Keep x/y/single/color selectors fresh ────────────────────────
    observe({
      req(dataset())
      data     <- dataset()
      num_vars <- names(data)[sapply(data, is.numeric)]
      all_vars <- names(data)
      ptype    <- if (is.null(input$plot_type)) "Scatter" else input$plot_type

      grp_vars <- names(data)[sapply(data, function(x) {
        is.character(x) || is.factor(x) || length(unique(x[!is.na(x)])) <= 20
      })]
      if (length(grp_vars) == 0) grp_vars <- all_vars

      cur_x    <- isolate(input$x_var)
      cur_y    <- isolate(input$y_var)
      cur_sngl <- isolate(input$single_var)
      cur_cv   <- isolate(input$color_var)

      x_choices <- if (ptype == "Box Plot") grp_vars else all_vars
      sel_x <- if (!is.null(cur_x) && cur_x %in% x_choices) cur_x
               else if (length(x_choices) >= 1) x_choices[1] else NULL
      updateSelectInput(session, "x_var", choices = x_choices, selected = sel_x)

      sel_y <- if (!is.null(cur_y) && cur_y %in% num_vars) cur_y
               else if (length(num_vars) >= 2) num_vars[2]
               else if (length(num_vars) >= 1) num_vars[1] else NULL
      updateSelectInput(session, "y_var", choices = num_vars, selected = sel_y)

      single_choices <- switch(ptype,
        "Histogram" = num_vars, "Bar Chart" = grp_vars, all_vars)
      sel_sngl <- if (!is.null(cur_sngl) && cur_sngl %in% single_choices) cur_sngl
                  else if (length(single_choices) >= 1) single_choices[1] else NULL
      updateSelectInput(session, "single_var", choices = single_choices, selected = sel_sngl)

      sel_cv <- if (!is.null(cur_cv) && cur_cv %in% c("", all_vars)) cur_cv else ""
      updateSelectInput(session, "color_var",
        choices = c("None" = "", all_vars), selected = sel_cv)
    })

    # ── Build plot on button click ────────────────────────────────────
    plot_obj <- eventReactive(input$generate_plot, {
      req(dataset())
      data  <- dataset()
      ptype <- input$plot_type

      # ── 1. Apply filters ─────────────────────────────────────────
      ids  <- filter_ids()
      data <- .apply_plot_filters(data, ids, input)
      validate(need(nrow(data) > 0,
        "No rows remain after filtering \u2014 adjust your FilterR settings."))

      # ── 2. Sample large datasets ─────────────────────────────────
      n_total <- nrow(data)
      pct     <- if (is.null(input$sample_pct)) 100 else input$sample_pct
      n_use   <- if (pct < 100) max(10L, floor(n_total * pct / 100L)) else n_total
      if (n_use < n_total) {
        set.seed(42)
        data <- data[sample(n_total, n_use), ]
      }

      # ── 3. Color inputs ──────────────────────────────────────────
      cv      <- if (is.null(input$color_var)) "" else input$color_var
      accent  <- if (is.null(input$accent_color)) "#00ff00" else input$accent_color
      palette <- if (is.null(input$color_palette)) "Dark2" else input$color_palette
      use_cv  <- cv != "" && cv %in% names(data)

      # ── 4. Auto title (overridable by user) ──────────────────────
      auto_title <- switch(ptype,
        "Scatter"   = paste("Scatter:",
                            (input$x_var %||% "?"), "vs", (input$y_var %||% "?")),
        "Histogram" = paste("Histogram of", (input$single_var %||% "?")),
        "Bar Chart" = paste("Bar Chart of", (input$single_var %||% "?")),
        "Box Plot"  = paste("Box Plot:",
                            (input$y_var %||% "?"), "by", (input$x_var %||% "?")),
        "Plot"
      )

      # ── 5. Base plot per type ─────────────────────────────────────
      p <- if (ptype == "Scatter") {
        req(input$x_var, input$y_var)
        xv <- input$x_var; yv <- input$y_var
        validate(
          need(xv %in% names(data), "X variable not found in dataset."),
          need(yv %in% names(data), "Y variable not found in dataset.")
        )
        base <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[xv]], y = .data[[yv]])) +
          .dark_theme() +
          ggplot2::labs(title = paste("Scatter:", xv, "vs", yv))
        if (use_cv)
          base +
            ggplot2::geom_point(alpha = 0.6, ggplot2::aes(color = .data[[cv]])) +
            .make_color_scale(cv, data, palette, is_fill = FALSE)
        else
          base + ggplot2::geom_point(alpha = 0.6, color = accent)

      } else if (ptype == "Histogram") {
        req(input$single_var)
        sv <- input$single_var
        validate(need(sv %in% names(data), "Variable not found in dataset."))
        base <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[sv]])) +
          .dark_theme() +
          ggplot2::labs(title = paste("Histogram of", sv))
        if (use_cv)
          base +
            ggplot2::geom_histogram(bins = input$bins, color = "black", alpha = 0.65,
                                    position = "identity",
                                    ggplot2::aes(fill = .data[[cv]])) +
            .make_color_scale(cv, data, palette, is_fill = TRUE)
        else
          base +
            ggplot2::geom_histogram(bins = input$bins, fill = accent,
                                    color = "black", alpha = 0.85)

      } else if (ptype == "Bar Chart") {
        req(input$single_var)
        sv <- input$single_var
        validate(
          need(sv %in% names(data), "Variable not found in dataset."),
          need(length(unique(data[[sv]])) <= 50,
               paste0("'", sv, "' has too many unique values for a bar chart.\n",
                      "Select a categorical or low-cardinality column."))
        )
        base <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[sv]])) +
          .dark_theme() +
          ggplot2::labs(title = paste("Bar Chart of", sv)) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
        if (use_cv)
          base +
            ggplot2::geom_bar(color = "black", alpha = 0.9,
                              ggplot2::aes(fill = .data[[cv]])) +
            .make_color_scale(cv, data, palette, is_fill = TRUE)
        else
          base + ggplot2::geom_bar(fill = accent, color = "black", alpha = 0.85)

      } else if (ptype == "Box Plot") {
        req(input$x_var, input$y_var)
        xv <- input$x_var; yv <- input$y_var
        n_groups <- length(unique(data[[xv]]))
        validate(
          need(xv %in% names(data), "X variable not found in dataset."),
          need(yv %in% names(data), "Y variable not found in dataset."),
          need(n_groups <= 40,
               paste0("'", xv, "' has too many unique values for a box plot.\n",
                      "Select a categorical or low-cardinality column for X."))
        )
        base <- ggplot2::ggplot(data,
            ggplot2::aes(x = as.factor(.data[[xv]]), y = .data[[yv]])) +
          .dark_theme() +
          ggplot2::labs(title = paste("Box Plot:", yv, "by", xv), x = xv) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
        if (use_cv)
          base +
            ggplot2::geom_boxplot(alpha = 0.8, color = "#888888",
                                  ggplot2::aes(fill = .data[[cv]])) +
            .make_color_scale(cv, data, palette, is_fill = TRUE)
        else
          base + ggplot2::geom_boxplot(fill = "#1a1a1a", color = accent)
      }

      # ── 6. Apply facets ──────────────────────────────────────────
      fvars <- vapply(seq_len(n_facets()), function(i) {
        v <- input[[paste0("facet_var_", i)]]
        if (is.null(v) || nchar(v) == 0 || !v %in% names(data)) "" else v
      }, character(1))
      fvars <- fvars[nchar(fvars) > 0]

      if (length(fvars) > 0) {
        facet_theme <- ggplot2::theme(
          strip.background = ggplot2::element_rect(fill = "#0d1a0d", color = "#00ff00"),
          strip.text       = ggplot2::element_text(color = "#00ff00", size = 11, face = "bold")
        )
        if (length(fvars) == 1) {
          p <- p +
            ggplot2::facet_wrap(ggplot2::vars(.data[[fvars[1]]]),
                                scales = input$facet_scales) +
            facet_theme
        } else if (length(fvars) == 2) {
          p <- p +
            ggplot2::facet_grid(rows   = ggplot2::vars(.data[[fvars[1]]]),
                                cols   = ggplot2::vars(.data[[fvars[2]]]),
                                scales = input$facet_scales) +
            facet_theme
        } else {
          fml <- as.formula(paste("~",
            paste(paste0("`", fvars, "`"), collapse = " + ")))
          p <- p +
            ggplot2::facet_wrap(fml, scales = input$facet_scales) +
            facet_theme
        }
      }

      # ── 7. Dynamic title + caption ───────────────────────────────
      user_title  <- trimws(if (!is.null(input$plot_title)) input$plot_title else "")
      final_title <- if (nchar(user_title) > 0) user_title else auto_title

      caption_parts <- character(0)

      # Color
      if (use_cv) {
        caption_parts <- c(caption_parts,
          paste0("Color: ", cv, " \u2014 ", palette, " palette"))
      } else {
        accent_names <- c(
          "#00ff00" = "Lime Green", "#00ccff" = "Cyan",   "#ff6600" = "Orange",
          "#ffcc00" = "Gold",       "#cc44ff" = "Purple", "#ff44aa" = "Hot Pink",
          "#ff4444" = "Crimson",    "#eeeeee" = "White")
        aname <- accent_names[accent]
        if (!is.na(aname))
          caption_parts <- c(caption_parts, paste0("Color: ", aname))
      }

      # Facets
      if (length(fvars) > 0)
        caption_parts <- c(caption_parts,
          paste0("Faceted by: ", paste(fvars, collapse = " \u00d7 "),
                 " (", input$facet_scales, " scales)"))

      # Row count
      if (n_use < n_total)
        caption_parts <- c(caption_parts,
          paste0(format(n_use, big.mark = ","), " of ",
                 format(n_total, big.mark = ","), " rows (sampled)"))
      else
        caption_parts <- c(caption_parts,
          paste0(format(n_total, big.mark = ","), " rows"))

      caption_str <- paste(caption_parts, collapse = "  \u00b7  ")

      p +
        ggplot2::labs(title = final_title, caption = caption_str) +
        ggplot2::theme(
          plot.title   = ggplot2::element_text(
            color = "#00ff00", size = 15, face = "bold",
            margin = ggplot2::margin(b = 8)),
          plot.caption = ggplot2::element_text(
            color = "#666666", size = 11, hjust = 0,
            margin = ggplot2::margin(t = 10))
        )
    })

    output$plot_output <- renderPlot({
      req(show_plot())
      plot_obj()
    })
  })
}
