library(shiny)
library(plotly)

source("ui_panel_continuous.R", local=TRUE)
source("ui_panel_discrete.R", local=TRUE)

shinyUI(fluidPage(
  
  # theme="bootstrap.css",
  
  titlePanel(
    fluidRow(
      column(2),
      column(8,
             h2("MA 677/681 Discussion - Common Distribution (Demo)"),
             h5("Ziyan (Mark) Li"))
    )),
  
  navbarPage(
    title="",
    tabPanel("Discrete Distribution", shiny_panel_discrete),
    tabPanel("Continuous Distribution", shiny_panel_continuous),
    tabPanel("Relationships among Common Distributions", img(src="distribution.png", height=700, width=1120)),
    tabPanel("(to be continued...)")
  )
)
)
