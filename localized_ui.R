library(shinyBS)

localizedUI <- function(i18n) {
  fluidPage(tagList(
  
  titlePanel(title = i18n$t("title"),
             windowTitle = i18n$t("title")),
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput("file", i18n$t("fileinput"), multiple = TRUE, 
                  buttonLabel = i18n$t("fileinputbutton"), placeholder = i18n$t("fileinputph")),
        selectInput("langsel", i18n$t("langsel"), 
                    choices = {
                      choices = languages
                      names(choices) = sapply(languages, i18n$t)
                      choices
                    }
                  )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(i18n$t("preview"), value = "preview",
              uiOutput("textSelector"),
              uiOutput("typeSwitch"),
              verbatimTextOutput("textViewer")
          )
        )
      )
    )
  )
  ))
}
