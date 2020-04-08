library(shinyBS)

localizedUI <- function(i18n) {
  fluidPage(tagList(
  
  titlePanel(title = i18n$t("title"),
             windowTitle = i18n$t("title")),
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(width=3,
        fileInput("file", i18n$t("fileinput"), multiple = TRUE,
          buttonLabel = i18n$t("fileinputbut"), placeholder = i18n$t("fileinputph")),
        selectInput("langsel", i18n$t("langsel"),
          choices = {
            choices = languages
            names(choices) = sapply(languages, i18n$t)
            choices
            }
          ),
        selectInput("unit", i18n$t("units"),
          choices = {
            choices = units_available
            names(choices) = sapply(units_available, i18n$t)
            choices
            }
          ),
        radioButtons("punct", i18n$t("punct"), selected = 1, inline = T,
          choices = {
            choices = c(0,1)
            names(choices) = sapply(c("no", "yes"), i18n$t)
            choices
          }
          )
        #actionButton("Go", i18n$t("go"))
        ),
      mainPanel(width=9,
        tabsetPanel(
          tabPanel(i18n$t("preview"), value = "preview",
            h3(i18n$t("uploadedfiles")),
            tableOutput("filesOverview"),
            radioButtons("previewType", i18n$t("previewType"), inline=T,
              choices = {
                choices = c(1,2)
                names(choices) = sapply(c("orig", "vert"), i18n$t)
                choices
                }),
            uiOutput("textPanelsPreview")
          ),
          tabPanel(i18n$t("indices"), value = "indices",
            h3(i18n$t("indextext")),
            uiOutput("textPanelsIndices")
            )
        )
      )
      
    )
  ),
  tags$hr(),
  tags$p(
    tags$small( appName, appVer[1], HTML("&copy; <a href='https://www.korpus.cz'>"), i18n$t("cnc"), HTML("</a>"), 
                HTML("Václav Cvrček"), names(appVer[1]), HTML(paste0("&bull; <a href='"), bugReportUrl, "' target='_blank'>"), i18n$t("error"), HTML("</a>") )
  )
  ))
}
