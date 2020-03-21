library(shiny)
library(shinyBS)
library(tidyverse)
library(shiny.i18n)
library(shinyCNC)
source("localized_ui.R")

shinyServer(function(input, output, session) {
  
  if(!init_shiny_cnc(appName))  { return() }
  
  lang <- get_lang(session)
  i18n <- Translator$new(translation_json_path = "data/translation.json")  
  i18n$set_translation_language(lang)
  user <- get_user(session)
  
  output$localizedUI <- renderUI(localizedUI(i18n))
  
  
})

