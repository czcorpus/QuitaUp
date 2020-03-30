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
  
  textCounts <- reactiveValues(valid = 0, all = 0)
  
  getOriginals <- reactive({
    req(input$file)
    texts <- list(language = input$langsel)
    for(i in 1:nrow(input$file)) {
      if (is.null( tryCatch(readtext::readtext(input$file[i,]$datapath), error = function(e) {}) )) {
        showNotification(paste(i18n$t("failedtoload"), input$file[i,]$name), type="error")
      } else {
        up.text <- readtext::readtext(input$file[i,]$datapath)
        texts$originals <- c(texts$originals, up.text$text)
        texts$names <- c(texts$names, input$file[i,]$name)
        if (nchar(input$file[i,]$name) > maxCharFileName) {
          sname <- paste0(substr(input$file[i,]$name, 1, maxCharFileName - 8), 
                          "...", 
                          substr(input$file[i,]$name, nchar(input$file[i,]$name) - 3, nchar(input$file[i,]$name)))
          texts$shortnames <- c(texts$shortnames, sname)
        } else {
          texts$shortnames <- c(texts$shortnames, input$file[i,]$name)
        }
        texts$type <- c(texts$type, input$file[i,]$type)
      }
    }
    textCounts$all <- nrow(input$file)
    textCounts$valid <- length(texts$names)
    texts
  })
  
  getVertical <- reactive({
    req(input$file, input$langsel)
    withProgress(message = i18n$t("vertprogress"), value = 0, {
      texts <- getOriginals()
      vert <- list()
      prgTot = 2 * length(texts$originals) + 1
      prgInc = 1
      incProgress(1/prgTot, detail = paste0(i18n$t("Hotovo"), " ", round(100 * prgInc/prgTot, digits=1), " %"))
      for (i in 1:length(texts$originals)) {
        udout <- udpipe(x = texts$originals[i], object = paste0(udModelDir, udModels[input$langsel]), parallel.cores = udParallel.cores)
        prgInc = prgInc + 1
        incProgress(1/prgTot, detail = paste0(i18n$t("Hotovo"), " ", round(100 * prgInc/prgTot, digits=1), " %"))
        vert[[i]] <- retokenizeUDpipe(udout)
        prgInc = prgInc + 1
        incProgress(1/prgTot, detail = paste0(i18n$t("Hotovo"), " ", round(100 * prgInc/prgTot, digits=1), " %"))
      }
    })
    vert
  })
  
  output$filesOverview <- renderTable({
    req(input$file)
    texts <- getOriginals()
    df <- input$file[,1:3] %>% filter(name %in% texts$names)
    colnames(df) <- sapply(c("filename", "filesize", "filetype"), i18n$t)
    df
  })
  
  output$textPanelsPreview <- renderUI({
    req(input$file, input$previewType)
    texts <- getOriginals()
    if (input$previewType == 1) { # view original files
      lapply(1:length(texts$names), function(i) output[[texts$names[i]]] <- renderText(prepareTextPreview(texts$originals[[i]])))
      panels <- mapply(function(name, id) tabPanel(name, textOutput(id)),
                       texts$shortnames, texts$names,
                       SIMPLIFY = FALSE, USE.NAMES = FALSE)
    } else { # view vertical
      vertical <- getVertical()
      lapply(1:length(texts$names), function(i) output[[texts$names[i]]] <- renderTable( prepareVerticalPreview(vertical[[i]]) ))
      panels <- mapply(function(name, id) tabPanel(name, tableOutput(id)),
                       texts$shortnames, texts$names,
                       SIMPLIFY = FALSE, USE.NAMES = FALSE)
    }
    do.call(navlistPanel, c(list(id = "textSelectorPanel", well=F, widths=c(2,7)), panels) )
  })
  
  observeEvent(input$previewType, {
    selectedTab <- input$textSelectorPanel
    updateNavlistPanel(session, "textSelectorPanel", selected = selectedTab)
  })
  
  prepareTextPreview <- function(charvector) {
    if (nchar(charvector) > maxCharTextPreview) {
      paste(substr(charvector, 1, maxCharTextPreview), "...")
    } else {
      charvector
    }
  }
  
  prepareVerticalPreview <- function(df) {
      select(df, sentence_id, token, lemma, upos, xpos, dep_rel) %>% head(n = maxLineVerticalPreview)
  }
  
  # =======================================================================================================
  
  output$textPanelsIndices <- renderUI({
    req(input$file, input$previewType)
    texts <- getOriginals()
    
    lapply(1:length(texts$names), function(i) {
      ids = paste0("idx", texts$names)
      output[[ ids[i] ]] <- renderTable(getIdxTable(i))
    })
    
    panels <- mapply(function(name, id) tabPanel(name, tableOutput(id)),
                     texts$shortnames, paste0("idx", texts$names),
                     SIMPLIFY = FALSE, USE.NAMES = FALSE)
    
    do.call(navlistPanel, c(list(id = "textSelectorPanelIndices", well=F, widths=c(2,7)), panels) )
  })
  
  getIdxTable <- function(nText) {
    results <- getIndices()
    resultsTable <- results[[nText]]
    resultsTable$idx <- i18n$t(resultsTable$idx)
    colnames(resultsTable) <- i18n$t(colnames(resultsTable))
    return(resultsTable)
  }
  
  getIndices <- reactive({
    vertical <- getVertical()
    results = lapply(vertical, function (x) data.frame("idx" = "tokens", "val" = nrow(x), stringsAsFactors = F))
    types <- sapply(vertical, function (x) length(table(x$token)))
    results <- addIndex(results, types, "types.word.cs")
    types <- sapply(vertical, function (x) length(table(tolower(x$token))) )
    results <- addIndex(results, types, "types.word.ci")
    types <- sapply(vertical, function (x) length(table(x$lemma)) )
    results <- addIndex(results, types, "types.lemma")
    
    return(results)
  })
  
})

