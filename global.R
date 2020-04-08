library(shiny)
library(shiny.i18n)
library(tidyverse)
library(stringr)

# ============== general parameters =================


httr::set_config(httr::config(http_version = 0)) # problem s nginex
modulenames <- c("OwOc" = 1, "TwOc" = 2, "TwTc" = 3, "SaRe" = 4, "zTTR" = 5, "Gr" = 6, "Ngrams" = 7, "about" = 8)
appName <- "quita+"
appVer = c("3/2020" = "0.01")
#logFile = "access.log"
bugReportUrl <- "https://podpora.korpus.cz/projects/xxxx"
#enableBookmarking(store = "url")

languages = c("cs", "en", "de", "fr")
units_available = c("word", "lc", "lemma")
udModelDir = "data/udmodels/"
udModels = c("cs" = "czech-pdt-ud-2.4-190531.udpipe",
             "en" = "english-ewt-ud-2.4-190531.udpipe",
             "de" = "german-gsd-ud-2.4-190531.udpipe",
             "fr" = "french-gsd-ud-2.4-190531.udpipe")
udParallel.cores = 10
maxCharFileName = 20 
maxCharTextPreview = 3000
maxLineVerticalPreview = 50

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

# =====

adjRanking <- function(df, attr = "token") {
  attr <- as.name(attr)
  fqdist <- df %>% group_by(!!attr, upos) %>% count(name = "fq") %>% ungroup() %>% 
    arrange(desc(fq)) %>% mutate(rank = 1:nrow(.))
  left_join(
    fqdist,
    fqdist %>% group_by(fq) %>% summarise(adjrank = mean(rank)),
    by = "fq"
  )
}

hpoint <- function(df, attr = "token") {
  fqlist <- adjRanking(df, attr)
  attr <- as.name(attr)
  #fqlist <- df %>% group_by(!!attr) %>% count(name = "fq") %>%
  #  arrange(desc(fq)) %>% ungroup() %>% mutate(rank = seq(1, nrow(.)))
  if (nrow(filter(fqlist, fq == rank)) > 0) {
    h <- filter(fqlist, fq == rank) %>% slice(1) %>% pull(fq)
  } else {
    f1 <- filter(fqlist, fq > rank) %>% arrange(fq) %>% select(-!!attr) %>% slice(1) %>% pull(fq)
    r1 <- filter(fqlist, fq > rank) %>% arrange(fq) %>% select(-!!attr) %>% slice(1) %>% pull(rank)
    f2 <- filter(fqlist, fq < rank) %>% select(-!!attr) %>% slice(1) %>% pull(fq)
    r2 <- filter(fqlist, fq < rank) %>% select(-!!attr) %>% slice(1) %>% pull(rank)
    h <- (f1 * r2 - f2 * r1) / (r2 - r1 + f1 - f2)
  }
  return(h)
}

# =====

countEntropy <- function(df, attr = "token") {
  attr <- as.name(attr)
  fqdist <- filter(df, upos != "PUNCT") %>% group_by(!!attr) %>% count(name = "fq")
  N <- sum(fqdist$fq)
  H <- log2(N) - (1/N) * sum(fqdist$fq * log2(fqdist$fq))
  varH <- (1/N) * (sum(fqdist$fq/N * (log2(fqdist$fq/N)^2)) - H^2)
  return(c(H, varH))
}

# =========

verbDistance <- function(df, verbTag = c("VERB", "AUX")) {
  df$poradi <- 1:nrow(df)
  dplyr::filter(df, upos %in% verbTag) %>% 
    mutate(lagPoradi = lag(poradi, 1), diff = poradi - lagPoradi - 1) %>%
    summarise(mean(diff, na.rm=T)) %>% 
    pull()
}

# ========

countActivity <- function(df, verbTag = c("VERB", "AUX"), adjTag = c("ADJ")) {
  fq_adj <- filter(df, upos %in% adjTag) %>% count() %>% pull()
  fq_verb <- filter(df, upos %in% verbTag) %>% count() %>% pull()
  q <- fq_verb / (fq_verb + fq_adj)
  return(q)
}

# ========

countATL <- function(df, attr = "token") {
  attr <- as.name(attr)
  atl <- pull(df, attr) %>% stringr::str_length() %>% mean(na.rm=T)
  return(atl)
}

# =====

countTC <- function(df, h, attr = "token", autosemanticupos = c("ADJ", "ADV", "NOUN", "VERB", "PROPN")) {
  rankfqdist <- adjRanking(df, attr)
  #print(h)
  f1 <- max(rankfqdist$fq)
  tcwords <- filter(rankfqdist, rank <= h, upos %in% autosemanticupos)
  #print(tcwords)
  if (nrow(tcwords) > 0) {
    tc <- sum( 2 * ( (h - tcwords$adjrank)*tcwords$fq ) / ( h * (h-1) * f1 ) )
  } else {
    tc <- 0
  }
  #print(tc)
  return(tc)
}

# =====

mattr <- function(df, attr = "token", L = 100) {
  attr <- as.name(attr)
  tokens <- pull(df, !!attr)
  N <- length(tokens)
  if (N > L) {
    types <- 0
    for(i in 1:(N-L)) {
      vi <- length(base::unique(tokens[i:(i+L)]))
      types <- types + vi
    }
    mattr <- types / (L * (N - L + 1))
  } else {
    mattr <- NA
  }
  return(mattr)
}