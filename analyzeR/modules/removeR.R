# Module UI
removeRUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    div(
      style = "background-color: #3a1a1a; border: 1px solid #cc0000; border-radius: 6px; padding: 16px; margin-bottom: 20px;",
      tags$strong(style = "color: #ff4444; font-size: 16px;", "Warning"),
      p(style = "color: #ffaaaa; margin-top: 8px; margin-bottom: 0;",
        "This will clear the dataset from memory AND reset the file input. You will need to re-upload to continue.")
    ),
    actionButton(ns("confirm_remove"), "Reset Dataset", class = "btn-danger"),
    br(), br(),
    verbatimTextOutput(ns("remove_status"))
  )
}

# Module Server
# reset_counter: a reactiveVal(0L) in server.R — incrementing it causes loadR to
# re-render its fileInput widget, which resets input$file to NULL and clears the
# LoadR tab visually.
removeRServer <- function(input, output, session, rv, reset_counter) {
  observeEvent(input$confirm_remove, {
    rv$data <- NULL
    reset_counter(reset_counter() + 1L)   # triggers LoadR fileInput re-render
    showNotification(
      "Dataset cleared. Upload a new file in the LoadR tab.",
      type = "message",
      duration = 5
    )
  })

  output$remove_status <- renderPrint({
    if (is.null(rv$data)) {
      cat("Status: No dataset loaded.")
    } else {
      cat(paste0("Status: Dataset loaded — ",
                 nrow(rv$data), " rows x ", ncol(rv$data), " columns."))
    }
  })
}
