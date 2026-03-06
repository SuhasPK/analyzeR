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
    actionButton(ns("reset_btn"), "Reset Dataset", class = "btn-danger"),
    br(),
    uiOutput(ns("remove_status"))
  )
}

# Module Server
# reset_counter: a reactiveVal(0L) in server.R — incrementing it causes loadR to
# re-render its fileInput widget, which resets input$file to NULL and clears the
# LoadR tab visually.
removeRServer <- function(id, rv, reset_counter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show confirmation modal before doing anything destructive
    observeEvent(input$reset_btn, {
      showModal(modalDialog(
        title = tags$span(style = "color: #ff4444;", "\u26a0 Reset Dataset"),
        tags$p(
          style = "font-size: 15px; color: #e0e0e0;",
          "This will permanently clear the loaded dataset from memory and reset all tabs."
        ),
        tags$p(
          style = "color: #ff9900; font-size: 14px; margin-top: 6px;",
          "Any cleaning or transformations applied in this session will be lost."
        ),
        tags$p(
          style = "color: #00bfff; font-size: 14px; margin-top: 6px;",
          tags$strong("Tip: "),
          "Download your report from the ",
          tags$strong("ReportR"),
          " tab before resetting — it captures all analysis done this session."
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_reset"), "Yes, Reset Everything",
                       class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    # Only reset after confirmation — reload the session to wipe all output
    observeEvent(input$confirm_reset, {
      removeModal()
      session$reload()
    })

    output$remove_status <- renderUI({
      if (is.null(rv$data)) {
        div(class = "status-badge status-empty", "No dataset loaded")
      } else {
        div(class = "status-badge status-loaded",
            paste0(nrow(rv$data), " rows \u00d7 ", ncol(rv$data), " columns loaded"))
      }
    })
  })
}
