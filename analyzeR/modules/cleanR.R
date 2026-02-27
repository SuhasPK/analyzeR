# Module UI
cleanRUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Clean",
        br(),
        actionButton(ns("remove_dups"), "Remove Duplicates", class = "btn-warning"),
        hr(),
        selectInput(ns("na_action"), "Handle Missing Values",
          choices = c("None", "Drop rows with NA", "Fill with Mean",
                      "Fill with Median", "Fill with Mode")),
        actionButton(ns("apply_na"), "Apply NA Strategy", class = "btn-primary"),
        hr(),
        uiOutput(ns("col_select_ui")),
        actionButton(ns("drop_cols"), "Drop Selected Columns", class = "btn-danger"),
        hr(),
        h5("Action Log"),
        verbatimTextOutput(ns("clean_log"))
      ),
      tabPanel("Preview",
        br(),
        DTOutput(ns("clean_preview"))
      )
    )
  )
}

# Module Server
cleanRServer <- function(input, output, session, rv) {
  ns <- session$ns

  log_entries <- reactiveVal(character(0))

  add_log <- function(msg) {
    log_entries(c(log_entries(), paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg)))
  }

  # --- Remove Duplicates ---
  observeEvent(input$remove_dups, {
    req(rv$data)
    before   <- nrow(rv$data)
    rv$data  <- rv$data[!duplicated(rv$data), ]
    removed  <- before - nrow(rv$data)
    add_log(paste0("Removed duplicates: ", removed, " row(s) removed. Now ", nrow(rv$data), " rows."))
    showNotification(paste0("Removed ", removed, " duplicate row(s)."), type = "message", duration = 4)
  })

  # --- NA Strategy ---
  observeEvent(input$apply_na, {
    req(rv$data)
    action <- input$na_action
    if (action == "None") return()

    data <- rv$data

    if (action == "Drop rows with NA") {
      before  <- nrow(data)
      data    <- tidyr::drop_na(data)
      removed <- before - nrow(data)
      add_log(paste0("Dropped rows with NA: ", removed, " row(s) removed."))
      showNotification(paste0("Dropped ", removed, " row(s) with NA."), type = "message", duration = 4)

    } else if (action == "Fill with Mean") {
      data <- dplyr::mutate(data, dplyr::across(where(is.numeric),
        ~ tidyr::replace_na(., mean(., na.rm = TRUE))))
      add_log("Filled numeric NA values with column mean.")
      showNotification("Filled numeric NAs with mean.", type = "message", duration = 4)

    } else if (action == "Fill with Median") {
      data <- dplyr::mutate(data, dplyr::across(where(is.numeric),
        ~ tidyr::replace_na(., median(., na.rm = TRUE))))
      add_log("Filled numeric NA values with column median.")
      showNotification("Filled numeric NAs with median.", type = "message", duration = 4)

    } else if (action == "Fill with Mode") {
      mode_val <- function(x) {
        ux <- unique(x[!is.na(x)])
        if (length(ux) == 0) return(NA)
        ux[which.max(tabulate(match(x, ux)))]
      }
      # Use direct assignment to avoid type-mismatch issues with replace_na
      for (nm in names(data)) {
        col <- data[[nm]]
        if (any(is.na(col))) {
          mv <- mode_val(col)
          if (!is.na(mv)) {
            col[is.na(col)] <- mv
            data[[nm]] <- col
          }
        }
      }
      add_log("Filled NA values with column mode.")
      showNotification("Filled NAs with mode.", type = "message", duration = 4)
    }

    rv$data <- data
  })

  # --- Dynamic column selector ---
  output$col_select_ui <- renderUI({
    req(rv$data)
    checkboxGroupInput(ns("cols_to_drop"), "Select Columns to Drop",
      choices = names(rv$data), inline = TRUE)
  })

  # --- Drop selected columns ---
  observeEvent(input$drop_cols, {
    req(rv$data, input$cols_to_drop)
    cols    <- input$cols_to_drop
    rv$data <- rv$data[, !names(rv$data) %in% cols, drop = FALSE]
    add_log(paste0("Dropped column(s): ", paste(cols, collapse = ", ")))
    showNotification(paste0("Dropped: ", paste(cols, collapse = ", ")), type = "warning", duration = 4)
  })

  # --- Action log output ---
  output$clean_log <- renderPrint({
    entries <- log_entries()
    if (length(entries) == 0) cat("No actions taken yet.") else cat(paste(entries, collapse = "\n"))
  })

  # --- Preview ---
  output$clean_preview <- renderDT({
    req(rv$data)
    datatable(rv$data, options = list(
      pageLength = 10,
      lengthMenu = c(10, 25, 50),
      scrollX    = TRUE
    ), class = "display compact cell-border stripe hover")
  })
}
