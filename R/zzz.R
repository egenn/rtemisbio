# zzz.R
# ::rtemisbio::
# 2024 EDG rtemis.org

# Get internal rtemis functions
msg2 <- getFromNamespace("msg2", "rtemis")
msg20 <- getFromNamespace("msg20", "rtemis")
hilite <- getFromNamespace("hilite", "rtemis")
bold <- getFromNamespace("bold", "rtemis")
italic <- getFromNamespace("italic", "rtemis")
orange <- getFromNamespace("orange", "rtemis")
green <- getFromNamespace("green", "rtemis")
inherits_test <- getFromNamespace("inherits_test", "rtemis")
dependency_check <- getFromNamespace("dependency_check", "rtemis")

rtemisbio.version <- packageVersion("rtemisbio")

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste0(
      "  .:", pkgname, " ", rtemisbio.version, " \U1F9EC", " ", sessionInfo()[[2]]
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