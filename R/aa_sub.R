# aasub.R
# ::rtemisbio::
# 2024 EDG rtemis.org

#' Perform amino acid substitutions
#'
#' @param x Character or charactr vector: Amino acid sequence. e.g. `"ARND"` or 
#' `c("A", "R", "N", "D")`.
#' @param substitutions Character vector: Substitutions to perform in the format
#' "OriginalPositionNew", e.g. `c("C291A", "C322A")`.
#'
#' @return Character vector with substitutions performed.
#' 
#' @author EDG
#' @export

aa_sub <- function(x, substitutions, verbosity = 1) {
  stopifnot(is.character(x), is.character(substitutions))
  # Split x into characters
  if (length(x) == 1) {
    x <- unlist(strsplit(x, ""))
  }
  for (s in substitutions) {
    strngs <- strsplit(s, "")[[1]]
    from <- strngs[1]
    to <- strngs[length(strngs)]
    pos <- as.numeric(strngs[2:(length(strngs) - 1)] |> paste(collapse = ""))
    if (verbosity > 0) {
      msg2("Substituting", hilite(from), "at position", hilite(pos), "with", hilite(to))
    }
    x[pos] <- to
  }
  if (verbosity > 0) {
    msg2("All done.")
  }
  x
} # rtemisbio::aa-sub
