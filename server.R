library(shiny)
library(shinyBS)
library(tidyverse)
library(shiny.i18n)
library(shinyCNC)
library(readtext)
library(udpipe)
source("localized_ui.R")

shinyServer(function(input, output, session) {
  
  if(!init_shiny_cnc(appName))  { return() }
  
  lang <- get_lang(session)
  i18n <- Translator$new(translation_json_path = "data/translation.json")  
  i18n$set_translation_language(lang)
  user <- get_user(session)
  
  output$localizedUI <- renderUI(localizedUI(i18n))
  
  getOriginals <- reactive({
    req(input$file)
    texts <- list(language = input$langsel)
    for(i in 1:nrow(input$file)) {
      up.text <- readtext::readtext(input$file[i,]$datapath)
      texts$originals[i] <- up.text$text
      texts$names <- c(texts$names, input$file[i,]$name)
      texts$type <- c(texts$type, input$file[i,]$type)
    }
    texts
  })
  
  getVertical <- reactive({
    req(input$file, input$langsel)
    texts <- getOriginals()
    vert <- list()
    for (i in 1:length(texts$originals)) {
      #browser()
      vert[[i]] <- udpipe(x = texts$originals[i], object = paste0(udModelDir, udModels["cs"]))
    }
    vert
  })
  
  output$textSelector <- renderUI({
    req(input$file)
    texts <- getOriginals()
    selectInput("textsel", i18n$t("textsel"),
      choices = {
        choices = 1:length(texts$names)
        names(choices) = texts$names
        choices
        }
      )
  })
  
  output$typeSwitch <- renderUI({
    req(input$file)
    radioButtons("previewType", i18n$t("previewType"), inline=T,
      choices = {
        choices = c(1,2)
        names(choices) = sapply(c("orig", "vert"), i18n$t)
        choices
        }
      )
  })
  
  output$textViewer <- renderPrint({
    req(input$file, input$textsel, input$previewType)
    texts <- getOriginals()
    if (input$previewType == 1) {
      texts$originals[as.numeric(input$textsel)]
    } else {
      vertical <- getVertical()
      head(vertical[[as.numeric(input$textsel)]])
    }
  })
  
})

