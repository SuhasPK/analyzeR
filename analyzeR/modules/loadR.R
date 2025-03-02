# Module UI
loadRUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Custom CSS to decrease the font size of the filename display
    tags$head(tags$style(HTML("
      .table-name {
        font-size: 12px; /* Adjust font size */
        font-weight: bold;
        text-align: center;
        margin-bottom: 16px;
      }
    "))),
    fileInput(ns("file"), "Upload Dataset", accept = c(".csv", ".txt", ".xlsx", ".tsv")),
    verbatimTextOutput(ns("file_info")),
    h3(textOutput(ns("table_name")), class = "table-name", style = "text-align: center;"),
    div(DTOutput(ns("preview")), style = "width: 100%; margin: 0 auto; overflow-x: scroll;")
  )
}

# Module Server
loadRServer <- function(input, output, session) {
  ns <- session$ns
  
  dataset <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = read.csv(input$file$datapath),
           tsv = read.delim(input$file$datapath),
           txt = read.delim(input$file$datapath),
           xlsx = read_excel(input$file$datapath),
           stop("Unsupported file type"))
  })
  
  output$file_info <- renderPrint({
    req(input$file)
    file <- input$file
    
    file_type <- file$type
    if (str_detect(file$name, "\\.tsv$") & file_type == "") {
      file_type = "text/tab-separated-values"
    }
    
    formatted_size <- paste(format(file$size, big.mark=","), "bytes")
    
    list(
      Name = file$name,
      Size = formatted_size,
      Type = file_type
    )
  })
  
  output$table_name <- renderText({
    req(input$file)
    file <- input$file
    paste("Table:", file$name)
  })
  
  output$preview <- renderDT({
    req(dataset())
    datatable(head(dataset(), 10), options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 20),
      autoWidth = TRUE,
      scrollX = TRUE,
      columnDefs = list(
        list(className = 'dt-center', targets = '_all')
      )
    ), class = "display compact cell-border stripe hover")
  })
  
  return(dataset)  # Return the dataset
}
