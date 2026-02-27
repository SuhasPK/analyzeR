# Module UI
loadRUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML("
      .table-name {
        font-size: 12px;
        font-weight: bold;
        text-align: center;
        margin-bottom: 16px;
      }
    "))),
    # Wrapped in uiOutput so it re-renders (and clears) when reset_counter fires
    uiOutput(ns("file_input_ui")),
    verbatimTextOutput(ns("file_info")),
    h3(textOutput(ns("table_name")), class = "table-name", style = "text-align: center;"),
    div(DTOutput(ns("preview")), style = "width: 100%; margin: 0 auto; overflow-x: scroll;")
  )
}

# Module Server
# reset_trigger: a reactive() that increments when the dataset is cleared
loadRServer <- function(input, output, session, reset_trigger) {
  ns <- session$ns

  # Re-render the fileInput each time reset_trigger fires.
  # A fresh fileInput has no value, so input$file becomes NULL,
  # which cascades to clear all outputs automatically via req().
  output$file_input_ui <- renderUI({
    reset_trigger()  # take dependency — re-renders on reset
    fileInput(ns("file"), "Upload Dataset",
              accept = c(".csv", ".txt", ".xlsx", ".tsv"))
  })

  dataset <- reactive({
    req(input$file)

    ext  <- tolower(tools::file_ext(input$file$name))
    path <- input$file$datapath

    tryCatch({
      if (ext == "csv") {
        read.csv(path, stringsAsFactors = FALSE)

      } else if (ext == "tsv") {
        read.delim(path, stringsAsFactors = FALSE)

      } else if (ext == "txt") {
        first_line <- readLines(path, n = 1, warn = FALSE)
        sep <- if (grepl("\t", first_line)) "\t" else ","
        read.table(path, header = TRUE, sep = sep,
                   stringsAsFactors = FALSE, fill = TRUE, quote = '"')

      } else if (ext == "xlsx") {
        as.data.frame(read_excel(path))

      } else {
        showNotification(paste0("Unsupported file type: .", ext),
                         type = "error", duration = 5)
        NULL
      }
    }, error = function(e) {
      showNotification(paste("Failed to read file:", conditionMessage(e)),
                       type = "error", duration = 6)
      NULL
    })
  })

  output$file_info <- renderPrint({
    req(input$file)
    file      <- input$file
    file_type <- file$type
    if (str_detect(file$name, "\\.tsv$") && file_type == "") {
      file_type <- "text/tab-separated-values"
    }
    list(
      Name = file$name,
      Size = paste(format(file$size, big.mark = ","), "bytes"),
      Type = if (nchar(file_type) > 0) file_type else "unknown"
    )
  })

  output$table_name <- renderText({
    req(input$file)
    paste("Table:", input$file$name)
  })

  output$preview <- renderDT({
    req(dataset())
    datatable(head(dataset(), 10), options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 20),
      autoWidth  = TRUE,
      scrollX    = TRUE,
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    ), class = "display compact cell-border stripe hover")
  })

  return(dataset)
}
