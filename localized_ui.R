library(shinyBS)

localizedUI <- function(i18n) {
  fluidPage(tagList(
  
  titlePanel(title = i18n$t("title"),
             windowTitle = i18n$t("title"))
  
  ))
}
