library(shinyBS)

localizedUI <- function(i18n) {
  fluidPage(tagList(
  
  titlePanel(title = i18n$t("title"),
             windowTitle = i18n$t("title")),
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(width=3,
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
          tabPanel(
            i18n$t("indices"), value = "indices",
            h3(i18n$t("indextext")),
            uiOutput("textPanelsIndices")
            )
        )
      )
    )
  )
  ))
}
