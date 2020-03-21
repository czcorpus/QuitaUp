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


