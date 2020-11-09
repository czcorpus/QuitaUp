library(shinyBS)

localizedUI <- function(i18n) {
  fluidPage(withMathJax(),
    tagList(
      titlePanel(
        title=div(img(src="QuitaUp_logo.svg", height=44, style="margin: 15px 0px 15px 3px;")),
                  windowTitle = i18n$t("title")
      ),
      
      sidebarLayout(
        sidebarPanel(width=3,
          fileInput("file", i18n$t("fileinput"), multiple = TRUE,
            buttonLabel = i18n$t("fileinputbut"), placeholder = i18n$t("fileinputph")),
          selectInput("langsel", i18n$t("langsel"), selected = "cs",
            choices = {
              choices = languages
              names(choices) = sapply(languages, i18n$t)
              choices
            }),
          selectInput("unit", i18n$t("units"), selected = default_unit,
            choices = {
              choices = units_available
              names(choices) = sapply(units_available, i18n$t)
              choices
            }),
        checkboxInput("punct", i18n$t("punct"), value = TRUE)
        ),
        
        
        mainPanel(width=9, 
          tabsetPanel(selected="about", id="mainpanel",
            tabPanel(i18n$t("preview"), value = "preview",
              conditionalPanel(condition = "output.checkupload",
                h3(i18n$t("uploadedfiles")),
                tableOutput("filesOverview"),
                h3(i18n$t("controlpreview")),
                radioButtons("previewType", i18n$t("previewType"), inline=T,
                  choices = {
                    choices = c(1,2)
                    names(choices) = sapply(c("orig", "vert"), i18n$t)
                    choices
                    }),
                uiOutput("textPanelsPreview"),
                htmlOutput("UDmodel")
                ),
              ),
            tabPanel(i18n$t("indices"), value = "indices",
              uiOutput("textPanelsIndices")
              ),
            tabPanel(i18n$t("about"), value = "about",
              h3(i18n$t("welcome hdln")),
              HTML(i18n$t("info text")),
              h3(i18n$t("manual")),
              h4(i18n$t("vlozenitextu")),
              HTML(i18n$t("vlozenitextutext")),
              h4(i18n$t("zpractextu")),
              HTML(i18n$t("zpractextutext")),
              h4(i18n$t("vystupy")),
              p(HTML(i18n$t("out N")), class="vystupy"),
              p(HTML(i18n$t("out V")), class="vystupy"),
              p(HTML(i18n$t("out TTR")), class="vystupy"),
              p(HTML(i18n$t("out h")), class="vystupy"),
              p(HTML(i18n$t("out hap")), class="vystupy"),
              p(HTML(i18n$t("out haptok")), class="vystupy"),
              p(HTML(i18n$t("out entro")), class="vystupy"),
              p(HTML(i18n$t("out VD")), class="vystupy"),
              p(HTML(i18n$t("out Q")), class="vystupy"),
              p(HTML(i18n$t("out D")), class="vystupy"),
              p(HTML(i18n$t("out ATL")), class="vystupy"),
              p(HTML(i18n$t("out TC")), class="vystupy"),
              p(HTML(i18n$t("out STC")), class="vystupy"),
              p(HTML(i18n$t("out MATTR")), class="vystupy"),
              p(HTML(i18n$t("out MAMR")), class="vystupy"),
              h3(i18n$t("historie")),
              p(HTML(i18n$t("historietext"))),
              h3(i18n$t("literatura")),
              p(HTML(i18n$t("literaturatext")))
              )
            )
          )
        ),
      tags$hr(),
      tags$p(
        tags$small(
          i18n$t("title"), 
          #stringr::str_to_title(appName), 
          i18n$t("ver"), appVer[1], names(appVer[1]), 
          HTML("&copy; <a href='https://www.korpus.cz'>"), i18n$t("cnc"), HTML("</a>"), HTML("Václav Cvrček"), 
          HTML("&bull; <a href='https://kcj.osu.cz/'>"), i18n$t("kcjffou"), HTML("</a>"),
          HTML(paste0("&bull; <a href='"), bugReportUrl, "' target='_blank'>"), i18n$t("error"), HTML("</a>") 
        )
      )
    )
  )
}
