# Module UI
readRUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("read_data_button"), "Read Data"),
    DTOutput(ns("dataset_summary"))
  )
}

# Module Server
readRServer <- function(input, output, session, dataset) {
  ns <- session$ns
  
  observeEvent(input$read_data_button, {
    showModal(modalDialog(
      title = "Read Data",
      "Do you want readR to read the data?",
      footer = tagList(
        modalButton("No"),
        actionButton(ns("go_to_readR"), "Yes")
      )
    ))
  })
  
  observeEvent(input$go_to_readR, {
    removeModal()
    
    output$dataset_summary <- renderDT({
      req(dataset())
      
      # Create a custom summary
      summary_df <- custom_summary(dataset())
      
      datatable(summary_df, options = list(
        pageLength = 10,
        lengthMenu = c(10, 20, 50),
        autoWidth = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ), class = "display compact cell-border stripe hover")
    })
  })
}
