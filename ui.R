library(shiny)
library(shiny.i18n)
library(shinythemes)
library(shinyCNC)

shinyUI(
  bootstrapPage(
    tags$head( tags$link(rel = "stylesheet", type = "text/css", href = "calc.css?v=7") ),
    #shinybusy::add_busy_spinner(spin = "orbit", color = "#009ee0"),
    shiny_cnc_UI(),
    #shinythemes::themeSelector(),
    theme = shinytheme("yeti"),
    uiOutput("localizedUI")
))