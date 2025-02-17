# Module UI
readRUI <- function(id) {
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("dataset_summary"))
  )
}

# Module Server
readRServer <- function(input, output, session) {
  output$dataset_summary <- renderPrint({
    "Dataset summary will be displayed here."
  })
}
