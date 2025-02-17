# Module UI
loadRUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload Dataset", accept = c(".csv", ".txt", ".xlsx", ".tsv")),
    verbatimTextOutput(ns("file_info")),
    h3("Preview"),  # Add the heading for the preview
    div(DTOutput(ns("preview")), style = "width: 50%;")  # Add custom width to make the table smaller
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
    
    # Determine file type if not automatically recognized
    file_type <- file$type
    if (str_detect(file$name, "\\.tsv$") & file_type == "") {
      file_type <- "text/tab-separated-values"
    }
    
    # Format the file size with "bytes"
    formatted_size <- paste(format(file$size, big.mark=","), "bytes")
    
    list(
      Name = file$name,
      Size = formatted_size,
      Type = file_type
    )
  })
  
  output$preview <- renderDT({
    req(dataset())
    head(dataset(), 10)  # Display the first 10 rows as a preview
  })
}