# zzz.R
# ::rtemisbio::
# 2024 EDG rtemis.org

# Get internal rtemis functions
msg2 <- getFromNamespace("msg2", "rtemis")
msg20 <- getFromNamespace("msg20", "rtemis")
hilite <- getFromNamespace("hilite", "rtemis")
bold <- getFromNamespace("bold", "rtemis")

rtemisbio.version <- packageVersion("rtemisbio")

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste0(
      "  .:", pkgname, " ", rtemisbio.version, " \U1F30A", " ", sessionInfo()[[2]]
    )
  )
} # rtemisbio::.onAttach

#' \pkg{rtemisbio}: Bioinformatics ops
#'
#' @description
#' Bioinformatics utilities
#' @name rtemisbio-package
#' @import rtemis
"_PACKAGE"

NULL