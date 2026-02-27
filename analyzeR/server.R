server <- function(input, output, session) {
  rv            <- reactiveValues(data = NULL)
  reset_counter <- reactiveVal(0L)

  raw_dataset <- callModule(loadRServer, "load", reactive(reset_counter()))

  observe({ rv$data <- raw_dataset() })

  callModule(readRServer,    "read",    reactive(rv$data))
  callModule(cleanRServer,   "clean",   rv)
  callModule(analyzeRServer, "analyze", reactive(rv$data))
  callModule(plotRServer,    "plot",    reactive(rv$data))
  callModule(learnRServer,   "learn",   reactive(rv$data))
  callModule(removeRServer,  "remove",  rv, reset_counter)
}
