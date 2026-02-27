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
        background-color: #000000;
        color: #FFFFFF;
      }
      h4 { font-size: 20px; }
      h5 { font-size: 17px; }
      .btn { font-size: 15px; padding: 6px 14px; }
      .nav-tabs .nav-link { font-size: 15px; }
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
      tags$li("AnalyzeR: Run correlation, normality, and outlier analysis."),
      tags$li("PlotR: Create insightful visualizations with facet support."),
      tags$li("LearnR: Train ML models, benchmark results, and predict new observations."),
      tags$li("RemoveR: Clear the dataset from memory.")
    )
  ),
  div(class = "jumbotron",
    h2("Data Operations"),
    tabsetPanel(
      id = "tabs",
      tabPanel("LoadR",    loadRUI("load")),
      tabPanel("ReadR",    readRUI("read")),
      tabPanel("CleanR",   cleanRUI("clean")),
      tabPanel("AnalyzeR", analyzeRUI("analyze")),
      tabPanel("PlotR",    plotRUI("plot")),
      tabPanel("LearnR",   learnRUI("learn")),
      tabPanel("RemoveR",  removeRUI("remove"))
    )
  ),
  tags$footer(
    style = "text-align: center; padding: 16px; color: #888; font-size: 13px; margin-top: 20px;",
    paste0("analyzeR v", app_version, " \u2014 Built with R Shiny")
  )
)
