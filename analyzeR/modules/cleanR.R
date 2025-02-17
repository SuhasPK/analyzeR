# Module UI
cleanRUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("clean"), "Clean Data"),
    verbatimTextOutput(ns("clean_status"))
  )
}

# Module Server
cleanRServer <- function(input, output, session) {
  observeEvent(input$clean, {
    output$clean_status <- renderPrint({
      "Data cleaning process will be executed here."
    })
  })
}
