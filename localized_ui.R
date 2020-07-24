library(shinyBS)

localizedUI <- function(i18n) {
  fluidPage(
    tagList(
      titlePanel(title = i18n$t("title"),
        windowTitle = i18n$t("title")),
      
      sidebarLayout(
        sidebarPanel(width=3,
          fileInput("file", i18n$t("fileinput"), multiple = TRUE,
            buttonLabel = i18n$t("fileinputbut"), placeholder = i18n$t("fileinputph")),
          selectInput("langsel", i18n$t("langsel"),
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
          tabsetPanel(
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
                uiOutput("textPanelsPreview")
                ),
              ),
            tabPanel(i18n$t("indices"), value = "indices",
              uiOutput("textPanelsIndices")
              ),
            tabPanel(i18n$t("about"), value = "about",
              h3(i18n$t("welcome hdln")),
              HTML(i18n$t("welcome text"))
              )
            )
          )
        ),
      tags$hr(),
      tags$p(
        tags$small(
          i18n$t("title"), 
          #stringr::str_to_title(appName), 
          i18n$t("ver"), appVer[1], HTML("&copy; <a href='https://www.korpus.cz'>"), i18n$t("cnc"), HTML("</a>"),
          HTML("Václav Cvrček"), names(appVer[1]), HTML(paste0("&bull; <a href='"), bugReportUrl, "' target='_blank'>"), i18n$t("error"), HTML("</a>") 
        )
      )
    )
  )
}
