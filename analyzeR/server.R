#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# server.R
server <- function(input, output, session) {
  dataset <- callModule(loadRServer, "load")
  callModule(readRServer, "read", dataset)
  callModule(cleanRServer, "clean")
  #callModule(analyzeRServer, "analyze")
  callModule(plotRServer, "plot")
  #callModule(removeRServer, "remove")
}






