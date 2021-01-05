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
  
  output$checkupload <- reactive({ # for hiding/showing conditional panel
    nrow(input$file)
  })
  outputOptions(output, "checkupload", suspendWhenHidden = FALSE) 
  
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
  
  observeEvent(input$file, {
    if (input$mainpanel == "about") {
      updateTabsetPanel(session, "mainpanel", selected = "preview")
    }
  })
  
  
  getVertical <- reactive({
    req(input$file, input$langsel)
    #shinybusy::show_modal_spinner(text = i18n$t("vertprogress"), spin = "orbit", color = "#009ee0", session)
    shinybusy::show_modal_gif(src = "rotujici_lupa_mensi_V2.gif", text = i18n$t("vertprogress"), height = "42px", width = "34px")
    texts <- getOriginals()
    #timestamp()
    texts.df <- data.frame(doc_id = texts$names, text = texts$originals)
    udout <- udpipe(x = texts.df, object = paste0(udModelDir, udModels[input$langsel]), parallel.cores = udParallel.cores, parser = "none")
    # if (lang_retokenize[input$langsel]) {
    #   udout.cor <- retokenizeUDpipe(udout)
    #   vert <- split(udout.cor, f = udout.cor$doc_id)
    # } else {
    #   vert <- split(udout, f = udout$doc_id)
    # }
    vert <- split(udout, f = udout$doc_id)
    #shinybusy::remove_modal_spinner()
    shinybusy::remove_modal_gif()
    #timestamp()
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
  
  output$UDmodel <- renderText({
    req(input$file, input$previewType, input$langsel)
    if (input$previewType != 1) { # view vertical
      paste0("<div class='note'>", i18n$t("udmodeltext"), " ", udModels[input$langsel],".</div>")
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
      select(df, sentence_id, token, lemma, upos, xpos) %>% head(n = maxLineVerticalPreview)
  }
  
  # =======================================================================================================
  
  output$textPanelsIndices <- renderUI({
    req(input$file, input$previewType)
    texts <- getOriginals()
    ids <- paste0("idx", texts$names)
    idstw <- paste0(ids, "tw")
    idstwempty <- paste0(ids, "twempty")
    idsvert <- paste0(texts$names, ".vert")
    idsvals <- paste0(texts$names, ".vals")
    if (nrow(input$file) == 1) {
      list(
        fluidRow(
          column(width=6, 
            h3(i18n$t("indextext")),
            output[[ ids[1] ]] <- renderTable(getIdxTable(1), spacing = "m", digits = 3),
            output[[ idsvert[1] ]] <- downloadHandler(idsvert[1], function(file) prepareDownload(file, 1), 
                                                      contentType = "text/csv", outputArgs = list(label = i18n$t("vertikala"))),
            output[[ idsvals[1] ]] <- downloadHandler(idsvals[1], function(file) write.csv(getIdxTable(1), file, row.names = FALSE), 
                                                      contentType = "text/csv", outputArgs = list(label = i18n$t("indexydwld")))
            ),
          column(width=6, 
            h3(i18n$t("twtext")),
            output[[ idstwempty[1] ]] <- renderText({if (nrow(getTwTable(1)) == 0) { return(i18n$t("NoTW")) } }),
            output[[ idstw[1] ]] <- renderTable(getTwTable(1), spacing = "m", digits = 4),
            tags$div(i18n$t("linktoUDPOStext"),
                     tags$a(href="https://universaldependencies.org/u/pos/index.html", target="_blank",
                            i18n$t("linktoUDPOS")), class="note")
            )
        )
      )
    } else {
      lapply(1:length(texts$names), function(i) {
        output[[ ids[i] ]] <- renderTable(getIdxTable(i), spacing = "m", digits = 3)
        output[[ idstwempty[i] ]] <- renderText({if (nrow(getTwTable(i)) == 0) { return(i18n$t("NoTW")) } })
        output[[ idstw[i] ]] <- renderTable(getTwTable(i), spacing = "m", digits = 4)
        output[[ idsvert[i] ]] <- downloadHandler(idsvert[i], 
                                                  function(file) prepareDownload(file, i), contentType = "text/csv")
        output[[ idsvals[i] ]] <- downloadHandler(idsvals[i], 
                                                  function(file) write.csv(getIdxTable(i), file, row.names = FALSE), 
                                                  contentType = "text/csv")
      })
      panels <- mapply(function(name, id, idtw, idtwempty, idver, idin) tabPanel(name, 
                                                   fluidRow(
                                                     column(width=6,
                                                       h3(i18n$t("indextext")),
                                                       tableOutput(id),
                                                       downloadButton(idver, label = i18n$t("vertikala")),
                                                       downloadButton(idin, label = i18n$t("indexydwld"))
                                                       ),
                                                     column(width=6, 
                                                       h3(i18n$t("twtext")),
                                                       textOutput(idtwempty),
                                                       tableOutput(idtw),
                                                       tags$div(i18n$t("linktoUDPOStext"),
                                                                tags$a(href="https://universaldependencies.org/u/pos/index.html", 
                                                                       i18n$t("linktoUDPOS")), class="note")
                                                       )
                                                     )
                                                   ),
                       texts$shortnames, ids, idstw, idstwempty, idsvert, idsvals,
                       SIMPLIFY = FALSE, USE.NAMES = FALSE)
      do.call(navlistPanel, c(list(id = "textSelectorPanelIndices", well=F, widths=c(2,10)), panels) )
    }
  })
  
  prepareDownload = function(file, nText) { 
    vertical <- adjVertical()
    write.csv(vertical[[nText]], file, row.names = FALSE)
  }
  
  getTwTable <- function(nText) {
    #data.frame(a = rep(1, nText), b = rep(20, nText), c = rep(300, nText))
    vertical <- adjVertical()
    hp <- sapply(vertical, function (x) hpoint(x, attr = hptc_attr))
    tc <- mapply(function (x, y) TW(x, h = y, attr = hptc_attr),
                 vertical, hp, SIMPLIFY = FALSE)
    stc <- mapply(function (x, y) TW(x, h = 2*y, attr = hptc_attr),
                  vertical, hp, SIMPLIFY = FALSE)
    if (nrow(tc[[nText]]) == 0 & nrow(stc[[nText]]) > 0) {
      tmp <- stc[[nText]] 
      tmp$tw = NA
      tc[[nText]] <- tmp
    }
    if (nrow(stc[[nText]]) > 0) {
      twtable <- full_join(
        select(tc[[nText]], 1, upos, tw),
        select(stc[[nText]], 1, upos, tw),
        by = c(hptc_attr, "upos"),
        suffix = c(".1hp", ".2hp")
      )
      colnames(twtable) <- sapply(colnames(twtable), i18n$t)
    } else {
     twtable <- data.frame() 
    }
    return(twtable)
  }
  
  getIdxTable <- function(nText) {
    results <- getIndices()
    resultsTable <- results[[nText]]
    resultsTable$idx <- sapply(resultsTable$idx, i18n$t)
    colnames(resultsTable) <- sapply(colnames(resultsTable), i18n$t)
    return(resultsTable)
  }
  
  
  adjVertical <- reactive({
    req(input$file, input$langsel, input$unit)
    vertical <- getVertical()
    # trim pucntuation
    if (input$punct == TRUE) {
      adj <- lapply(vertical, function (x) dplyr::filter(x, upos != "PUNCT"))
    } else {
      adj <- vertical
    }
    # create lc and form column
    adj <- lapply(adj, function (x) { x$lc <- stringr::str_to_lower(x$token, locale = input$langsel); return(x) } )
    if (input$unit == "lc") {
      adj <- lapply(adj, function (x) { x$form <- x$lc; return(x) } )
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
    types <- sapply(vertical, function (x) length(table(x$form)) )
    results <- addIndex(results, types, "types")
    #ttr
    ttr <- sapply(results, function (x) x[ x$idx == "types", ]$val / x[ x$idx == "tokens", ]$val)
    results <- addIndex(results, ttr, "ttr")
    #hpoint
    # form + upos? rank or adjRank?
    hp <- sapply(vertical, function (x) hpoint(x, attr = hptc_attr))
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
    q <- sapply(vertical, function (x) countActivity(x, verbTag = c("VERB")))
    results <- addIndex(results, q, "activity")
    results <- addIndex(results, 1-q, "descriptivity")
    # average token length
    # what is a token and what is a character? language specific?
    atl <- sapply(vertical, function (x) countATL(x, attr = "form"))
    results <- addIndex(results, atl, "atl")
    # TC
    # definition of autosemantics
    tc <- mapply(function (x, y) countTC(x, h = y, attr = hptc_attr),
                 vertical, hp)
    results <- addIndex(results, tc, "tc")
    # Secondary TC
    stc <- mapply(function (x, y) countTC(x, h = 2*y, attr = hptc_attr),
                  vertical, hp)
    results <- addIndex(results, stc, "stc")
    # MATTR
    # size of a windown L?
    ma <- sapply(vertical, function (x) mattr(x, attr = "form", L = 100))
    results <- addIndex(results, ma, "mattr")
    ma5 <- sapply(vertical, function (x) mattr(x, attr = "form", L = 500))
    results <- addIndex(results, ma5, "mattr5")
    # zTTR
    if (!exists("koeficienty")) { load("data/zTTR-coeffs_2019-08-07.RData") }
    zttr <- sapply(vertical, function (x) countzttr(x, koeficienty, lang = input$langsel, att = input$unit, att_vert = "form"))
    results <- addIndex(results, zttr, "zttr")
    # MAMR
    if (input$unit == "lc") {
      ma.wf <- ma
      ma5.wf <- ma5
      ma.l <- sapply(vertical, function (x) mattr(x, attr = "lemma", L = 100))
      ma5.l <- sapply(vertical, function (x) mattr(x, attr = "lemma", L = 500))
    } else if (input$unit == "lemma") {
      ma.wf <- sapply(vertical, function (x) mattr(x, attr = "lc", L = 100))
      ma5.wf <- sapply(vertical, function (x) mattr(x, attr = "lc", L = 500))
      ma.l <- ma
      ma5.l <- ma5
    } else {
      ma.wf <- sapply(vertical, function (x) mattr(x, attr = "lc", L = 100))
      ma5.wf <- sapply(vertical, function (x) mattr(x, attr = "lc", L = 500))
      ma.l <- sapply(vertical, function (x) mattr(x, attr = "lemma", L = 100))
      ma5.l <- sapply(vertical, function (x) mattr(x, attr = "lemma", L = 500))
    }
    mamr <- ma.wf - ma.l
    mamr5 <- ma5.wf - ma5.l
    results <- addIndex(results, mamr, "mamr")
    results <- addIndex(results, mamr5, "mamr5")
    #browser()
    return(results)
  })
  
})

