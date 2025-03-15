#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# ui.R
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Ubuntu+Mono&display=swap"),
    tags$style(HTML("
      body, h1, h2, h3, h4, h5, h6, p {
        font-family: 'Ubuntu Mono', monospace;
      }
      p {
        font-size: 18px;
        color: lightgray;
        line-height: 1.6;
      }
      body {
        font-size: 16px;
        background-color: #000000; /* Dark background */
        color: #FFFFFF;           /* Light text */
      }
      .title {
        color: lime;
        font-size: 52px;
        text-align: center;
        margin-bottom: 20px;
      }
      .nav-tabs .nav-link {
        color: lime !important;
      }
      .nav-tabs .nav-link.active {
        background-color: lime;
        color: black !important;
      }
      .jumbotron {
        background-color: #222222;
        color: #FFFFFF;
        padding: 30px;
        border-radius: 10px;
        box-shadow: 0 4px 10px rgba(0, 0, 0, 0.5);
        margin-top: 30px;
      }
    "))
  ),
  titlePanel(
    div(class = "title", "analyzeR")
  ),
  # First Jumbotron for main panel contents
  div(class = "jumbotron",
      h2("Welcome to analyzeR!"),
      div(
        style = "margin-bottom: 20px;",
        p("A simple user-friendly web app which lets users upload datasets and start looking for insights.")
      ),
      h3("Features"),
      tags$ul(
        tags$li("LoadR: Upload your datasets in various formats like CSV, TXT, XLSX, and TSV."),
        tags$li("ReadR: Get a comprehensive summary of your dataset."),
        tags$li("CleanR: Perform essential data cleaning operations."),
        tags$li("PlotR: Create insightful visualizations.")
      )
  ),
  # Second Jumbotron for functionality tabs
  div(class = "jumbotron",
      h2("Data Operations"),
      tabsetPanel(
        id = "tabs",
        tabPanel("LoadR", loadRUI("load")),
        tabPanel("ReadR", readRUI("read")),
        tabPanel("CleanR", cleanRUI("clean")),
        tabPanel("PlotR", plotRUI("plot"))
      )
  )
)
