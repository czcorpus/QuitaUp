library(shiny)
library(shinyBS)
library(tidyverse)
library(shiny.i18n)
library(shinyCNC)
library(readtext)
library(udpipe)
library(stringr)
source("localized_ui.R")

shinyServer(function(input, output, session) {
  
  if(!init_shiny_cnc(appName))  { return() }
  
  lang <- get_lang(session)
  i18n <- Translator$new(translation_json_path = "data/translation.json")  
  i18n$set_translation_language(lang)
  user <- get_user(session)
  
  output$localizedUI <- renderUI(localizedUI(i18n))
  
  textCounts <- reactiveValues(valid = 0, all = 0)
  
  #getOriginals <- eventReactive(input$Go,{
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
    if (nrow(input$file) == 1) {
      if (input$previewType == 1) { # view original files
        HTML(prepareTextPreview(texts$originals[[1]]))
      } else { # view vertical
        vertical <- getVertical()
        list(
          output[[texts$names[1]]] <- renderTable(prepareVerticalPreview(vertical[[1]]))
        )
      }
    } else {
      if (input$previewType == 1) { # view original files
        lapply(1:length(texts$names), function(i) output[[texts$names[i]]] <- renderText(prepareTextPreview(texts$originals[[i]])))
        panels <- mapply(function(name, id) tabPanel(name, htmlOutput(id)),
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
    }
  })
  
  observeEvent(input$previewType, {
    selectedTab <- input$textSelectorPanel
    updateNavlistPanel(session, "textSelectorPanel", selected = selectedTab)
  })
  
  prepareTextPreview <- function(charvector) {
    charvector <- stringr::str_replace_all(charvector, '\n', '<br/>')
    if (nchar(charvector) > maxCharTextPreview) {
      paste0("<pre class='textpreview'>", substr(charvector, 1, maxCharTextPreview), "...</pre>")
    } else {
      paste0("<pre class='textpreview'>", charvector, "</pre>")
    }
  }
  
  prepareVerticalPreview <- function(df) {
      select(df, sentence_id, token, lemma, upos, xpos, dep_rel) %>% head(n = maxLineVerticalPreview)
  }
  
  # =======================================================================================================
  
  output$textPanelsIndices <- renderUI({
    req(input$file, input$previewType)
    texts <- getOriginals()
    ids = paste0("idx", texts$names)
    if (nrow(input$file) == 1) {
      list(
        output[[ ids[1] ]] <- renderTable(getIdxTable(1), spacing = "m", digits = 3)
      )
    } else {
      lapply(1:length(texts$names), function(i) {
        #ids = paste0("idx", texts$names)
        output[[ ids[i] ]] <- renderTable(getIdxTable(i), spacing = "m", digits = 3)
      })
      panels <- mapply(function(name, id) tabPanel(name, tableOutput(id)),
                       texts$shortnames, paste0("idx", texts$names),
                       SIMPLIFY = FALSE, USE.NAMES = FALSE)
      do.call(navlistPanel, c(list(id = "textSelectorPanelIndices", well=F, widths=c(2,7)), panels) )
    }
    
  })
  
  getIdxTable <- function(nText) {
    results <- getIndices()
    resultsTable <- results[[nText]]
    resultsTable$idx <- sapply(resultsTable$idx, i18n$t)
    colnames(resultsTable) <- sapply(colnames(resultsTable), i18n$t)
    return(resultsTable)
  }
  
  adjVertical <- reactive({
    req(input$file, input$langsel, input$unit, input$punct)
    vertical <- getVertical()
    # trim pucntuation
    if (input$punct == 1) {
      adj <- lapply(vertical, function (x) dplyr::filter(x, upos != "PUNCT"))
    } else {
      adj <- vertical
    }
    # create form column
    if (input$unit == "lc") {
      adj <- lapply(adj, function (x) { x$form <- stringr::str_to_lower(x$token, locale = input$langsel); return(x) } )
    } else if (input$unit == "lemma") {
      adj <- lapply(adj, function (x) { x$form <- x$lemma; return(x) } )
    } else {
      adj <- lapply(adj, function (x) { x$form <- x$token; return(x) } )
    }
    return(adj)
  })
  
  getIndices <- reactive({
    req(input$file, input$unit)
    # trim punctuation?, create form column
    vertical <- adjVertical()
    #tokens
    results = lapply(vertical, function (x) data.frame("idx" = "tokens", "val" = nrow(x), stringsAsFactors = F))
    #types
    types <- sapply(vertical, function (x) length(table(x$form)))
    results <- addIndex(results, types, "types")
    #ttr
    ttr <- sapply(results, function (x) x[ x$idx == "types", ]$val / x[ x$idx == "tokens", ]$val)
    results <- addIndex(results, ttr, "ttr")
    #hpoint
    # form + upos? rank or adjRank?
    hp <- sapply(vertical, function (x) hpoint(x, attr = "form"))
    results <- addIndex(results, hp, "hpoint")
    #hapax
    hap <- sapply(vertical, function (x) sum(table(x$form) == 1) )
    results <- addIndex(results, hap, "hapax")
    # hapax-token ratio
    hl <- sapply(results, function (x) x[ x$idx == "hapax", ]$val / x[ x$idx == "tokens", ]$val)
    results <- addIndex(results, hl, "hl")
    # entropy
    entro <- sapply(vertical, function (x) countEntropy(x, attr = "form"))
    results <- addIndex(results, entro[1,], "entropy")
    #results <- addIndex(results, entro[2,], "varH")
    #VD - verb distances
    # specific verbTag for each language?
    vd <- sapply(vertical, function (x) verbDistance(x))
    results <- addIndex(results, vd, "vdist")
    # activity and descriptivity
    # specific adjTag for each language?
    q <- sapply(vertical, function (x) countActivity(x))
    results <- addIndex(results, q, "activity")
    results <- addIndex(results, 1-q, "descriptivity")
    # average token length
    # what is a token and what is a character? language specific?
    atl <- sapply(vertical, function (x) countATL(x, attr = "form"))
    results <- addIndex(results, atl, "atl")
    # TC
    # definition of autosemantics
    tc <- mapply(function (x, y) countTC(x, h = y, attr = "form"),
                 vertical, hp)
    results <- addIndex(results, tc, "tc")
    # Secondary TC
    stc <- mapply(function (x, y) countTC(x, h = 2*y, attr = "form"),
                  vertical, hp)
    results <- addIndex(results, stc, "stc")
    # MATTR
    # size of a windown L?
    ma <- sapply(vertical, function (x) mattr(x, attr = "form", L = 100))
    results <- addIndex(results, ma, "mattr")
    
    return(results)
  })
  
})

