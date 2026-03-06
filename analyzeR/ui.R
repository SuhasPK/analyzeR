ui <- fluidPage(
  theme = shinytheme("cyborg"),
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Ubuntu+Mono&display=swap"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  useShinyjs(),
  div(class = "app-header",
    div(class = "app-title", "analyzeR"),
    div(class = "app-subtitle", paste0("upload \u00b7 explore \u00b7 model \u00b7 v", app_version)),
    uiOutput("dataset_status")
  ),
  div(class = "main-content",
    tabsetPanel(
      id = "tabs",
      tabPanel("Home",     homeRUI("home")),
      tabPanel("LoadR",    loadRUI("load")),
      tabPanel("ReadR",    readRUI("read")),
      tabPanel("CleanR",   cleanRUI("clean")),
      tabPanel("AnalyzeR", analyzeRUI("analyze")),
      tabPanel("PlotR",    plotRUI("plot")),
      tabPanel("TextR",    textRUI("text")),
      tabPanel("LearnR",   learnRUI("learn")),
      tabPanel("ReportR",  reportRUI("report")),
      tabPanel("RemoveR",  removeRUI("remove"))
    )
  ),

  # ── MonitR — persistent compute stats widget ──────────────────────────────
  div(id = "monitr-bar",
    div(class = "monitr-header",
      tags$button(
        class = "monitr-toggle-btn",
        HTML("&#9650;"),
        onclick = "var p=document.getElementById('monitr-panel');if(p.style.display==='none'){p.style.display='block';this.innerHTML='&#9660;';}else{p.style.display='none';this.innerHTML='&#9650;';}"
      ),
      span(class = "monitr-label", "\u26a1 MonitR"),
      uiOutput("monitr_stats")
    ),
    div(id = "monitr-panel", style = "display:none;",
      uiOutput("monitr_log_ui")
    )
  )
)
