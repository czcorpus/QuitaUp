library(shiny)
library(shiny.i18n)
library(tidyverse)


# ============== general parameters =================


httr::set_config(httr::config(http_version = 0)) # problem s nginex
modulenames <- c("OwOc" = 1, "TwOc" = 2, "TwTc" = 3, "SaRe" = 4, "zTTR" = 5, "Gr" = 6, "Ngrams" = 7, "about" = 8)
appName <- "quita+"
appVer = c("3/2020" = "0.01")
#logFile = "access.log"
#bugReportUrl <- "https://podpora.korpus.cz/projects/calc/issues/new"
#enableBookmarking(store = "url")

languages = c("cs", "en")
udModelDir = "data/udmodels/"
udModels = c("cs" = "czech-pdt-ud-2.4-190531.udpipe",
             "en" = "english-ewt-ud-2.4-190531.udpipe")
udParallel.cores = 2
maxCharFileName = 20 
maxCharTextPreview = 1000
maxLineVerticalPreview = 20

# ====

retokenizeUDpipe <- function(df) {
  begin <- 0
  end <- 0
  cats_to_add_max <-  c("lemma", "upos", "xpos", "feats", "head_token_id", "dep_rel", "deps")
  cats_to_add <- cats_to_add_max[ cats_to_add_max %in% names(df) ]
  new_df <- df[1,]
  for (i in 1:nrow(df)) {
    if( grepl('\\d+-\\d+', df[i,]$token_id) ) {
      tmp <- stringr::str_match(df[i,]$token_id, '(\\d+)-(\\d+)')
      begin <- as.numeric(tmp[2])
      end <- as.numeric(tmp[3])
      zasobnik <- df[i,]
    } else if (as.numeric(df[i,]$token_id) == begin) {
      zasobnik[, cats_to_add] <- df[i, cats_to_add]
    } else if (as.numeric(df[i,]$token_id) > begin & as.numeric(df[i,]$token_id) < end) {
    } else if (as.numeric(df[i,]$token_id) == end) {
      new_df[nrow(new_df) + 1,] <- zasobnik
      begin = 0
      end = 0
    } else {
      new_df[nrow(new_df) + 1,] <- df[i,]
    }
  }
  return(new_df[-1,])
}

# ======

addIndex <- function(mylist, myvector, myname) {
  for (n in 1:length(mylist)) {
    mylist[[n]] <- bind_rows(
      mylist[[n]],
      data.frame("idx" = myname, "val" = myvector[n], stringsAsFactors = F)
    )
  }
  return(mylist)
}
