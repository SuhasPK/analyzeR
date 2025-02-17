#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("global.R")

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  includeCSS("www/custom.css"),
  navbarPage("analyzeR",
             tabPanel("LoadR", loadRUI("load")),
             tabPanel("ReadR", readRUI("read")),
             tabPanel("CleanR", cleanRUI("clean")),
             #tabPanel("AnalyzeR", analyzeRUI("analyze")),
             tabPanel("PlotR", plotRUI("plot")),
             #tabPanel("RemoveR", removeRUI("remove"))
  )
)
