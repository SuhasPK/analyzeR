# Module UI
plotRUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x_var"), "Select X-axis Variable", choices = NULL),
    selectInput(ns("y_var"), "Select Y-axis Variable", choices = NULL),
    actionButton(ns("plot"), "Generate Plot"),
    plotOutput(ns("plot_output"))
  )
}

# Module Server
plotRServer <- function(input, output, session) {
  observe({
    updateSelectInput(session, "x_var", choices = c("Variable1", "Variable2"))
    updateSelectInput(session, "y_var", choices = c("Variable1", "Variable2"))
  })
  
  output$plot_output <- renderPlot({
    req(input$x_var, input$y_var)
    ggplot2::ggplot(data.frame(x = rnorm(100), y = rnorm(100)),
                    ggplot2::aes_string(x = "x", y = "y")) +
      ggplot2::geom_point()
  })
}
