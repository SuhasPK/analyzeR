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
      p{
      font-size: 18px;
      }
      body{
      font-size: 16px;
      }
      .title {
        color: lime;
        font-size: 52px;
      }
      code, pre, .shiny-text-output, .shiny-html-output {
        font-size: 16px;
      }
      p {
        color: grey;
      }
      .nav-tabs .nav-link {
        color: lime !important;
      }
      .nav-tabs .nav-link:hover, .nav-tabs .nav-link:focus {
        color: lime !important;
      }
      .nav-tabs .nav-item.show .nav-link, .nav-tabs .nav-link.active {
        color: lime !important;
      }
    "))
  ),
  includeCSS("www/custom.css"),
  titlePanel(
    div(class = "title", "analyzeR")
  ),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("LoadR", loadRUI("load")),
        tabPanel("ReadR", readRUI("read")),
        tabPanel("CleanR", cleanRUI("clean")),
        #tabPanel("AnalyzeR", analyzeRUI("analyze")),
        tabPanel("PlotR", plotRUI("plot")),
        #tabPanel("RemoveR", removeRUI("remove"))
      ),
      width = 6  # Adjust this value to control the width of the sidebar
    ),
    mainPanel(
      h2("Welcome to analyzeR!"),
      p("A simple user-friendly web app which lets users upload datasets and start looking for some insights."),
      p("The analyzeR web app is designed to help users seamlessly upload, read, clean, analyze, and visualize datasets. Here’s a quick rundown of its functionality:"),
      p("1. LoadR: Upload your datasets in various formats like CSV, TXT, XLSX, and TSV."),
      p("2. ReadR: Get a comprehensive summary of your dataset, including statistics and frequency tables."),
      p("3. CleanR: Perform essential data cleaning operations to prepare your dataset for analysis."),
      p("4. PlotR: Create insightful visualizations to explore data trends and patterns."),
      p("The intuitive interface ensures that users, regardless of their technical expertise, can quickly and efficiently extract meaningful insights from their data. Dive into the analyzeR app to start your data exploration journey!"),
      width = 6  # Adjust this value to control the width of the main panel
    )
  )
)






