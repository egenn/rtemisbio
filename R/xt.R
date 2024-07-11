# xt.R
# ::rtemisbio::
# 2024 EDG rtemis.org

#' Create an `xt` object
#'
#' Creates an `xt` object from time series data.
#'
#' @param x Datetime vector or list of datetime vectors.
#' @param y Numeric vector or list of numeric vectors. When plotted, these will correspond to the
#' left y-axis.
#' @param y2 Numeric vector or list of numeric vectors. When plotted, these will correspond to the
#' right y-axis.
#' @param xunits Character: Units for `x`.
#' @param yunits Character: Units for `y`.
#' @param y2units Character: Units for `y2`.
#' @param reference Character: Link to reference (journal publication, preprint, etc.)
#'
#' @author EDG
#' @return `xt` object
#' @export

toxt <- function(
    x,
    y,
    x2 = NULL,
    y2 = NULL,
    group = NULL,
    shade = NULL,
    xunits = NULL,
    yunits = NULL,
    y2units = NULL,
    reference = NULL) {
  # Check types
  # inherits_test(x, "list")
  # inherits_test(y, "list")
  # inherits_test(y2, "list")
  inherits_test(xunits, "character")
  inherits_test(yunits, "character")
  inherits_test(y2units, "character")
  inherits_test(reference, "character")

  if (!is.list(x)) {
    x <- list(x)
  }

  if (!is.list(y)) {
    y <- list(y)
  }

  if (!is.null(y2)) {
    if (!is.list(y2)) {
      y2 <- list(y2)
    }
  }

  if (!is.null(group)) {
    if (!is.list(group)) {
      group <- list(group)
    }
  }

  # Convert to `xt` object
  xt <- list(
    x = x,
    x2 = x2,
    y = y,
    y2 = y2,
    group = group,
    shade = shade,
    xunits = xunits,
    yunits = yunits,
    y2units = y2units,
    reference = reference
  )

  class(xt) <- c("xt", "list")
  return(xt)
} # /rtemisbio::toxt

#' Print method for `xt` object
#'
#' @method print xt
#' @param x `xt` object.
#' @param ... Not used.
#'
#' @author EDG
#' @export

print.xt <- function(x, head.n = 10, ...) {
  cat(".:", orange("xt", bold = TRUE), " timeseries object\n", sep = "")
  # Length of timeseries
  length_x <- length(x$x)
  cat(
    "  There ", ngettext(length_x, "is ", "are "), hilite(length_x),
    " time ", ngettext(length_x, "vector", "vectors"), " with ",
    ngettext(length_x, "length ", "lengths "),
    hilite(paste(sapply(x$x, length), collapse = ", ")), ".\n",
    sep = ""
  )

  length_x2 <- length(x$x2)
  if (length_x2 > 0) {
    cat(
      "  There ", ngettext(length_x2, "is ", "are "), hilite(length_x2),
      " time ", ngettext(length_x2, "vector", "vectors"), " with ",
      ngettext(length_x2, "length ", "lengths "),
      hilite(paste(sapply(x$x2, length), collapse = ", ")), ".\n",
      sep = ""
    )
  }

  # Number of y and y2 timeseries
  length_y <- length(x$y)
  cat(
    "  There ", ngettext(length_y, "is ", "are "), hilite(length_y), " y timeseries: ",
    hilite(names(x$y)), "\n", sep = ""
  )
  # if (!is.null(x$yunits)) {
  #   cat("  Units of y timeseries:", hilite(x$yunits), "\n")
  # }
  length_y2 <- length(x$y2)
  if (length_y2 > 0) {
    cat(
      "  There ", ngettext(length_y2, "is ", "are "), hilite(length_y2), " y2 timeseries: ",
      hilite(names(x$y2)), "\n", sep = ""
    )
  }
  
  # if (!is.null(x$y2units)) {
  #   cat("  Units of y2 timeseries:", hilite(x$y2units), "\n")
  # }
  if (!is.null(x$group)) {
    cat(
      "  There ", ngettext(length(x$group), "is ", "are "), hilite(length(x$group)), " groupings: ",
      hilite(names(x$group)), "\n",
      sep = ""
    )
  }
  if (!is.null(x$Reference)) {
    cat("    Reference:", hilite(x$Reference), "\n")
  }
} # /rtemisbio::print.xt


#' as.xt
#'
#' @param x Object to convert to `xt`.
#'
#' @author EDG
#' @return `xt` object.
#' @export

as.xt <- function(x) {
  UseMethod("as.xt")
} # /rtemisbio::as.xt

#' as.xt
#'
#' @param x Object to convert to `xt`.
#'
#' @author EDG
#' @return `xt` object.
#' @export

as.xt.default <- function(x) {
  inherits_test(x, "list")
  as.xt.list(x)
} # /rtemisbio::as.xt


#' as.xt.list method
#'
#' @param x List: Named list with elements `x`, `y`, `y2`, `xunits`, `yunits`, `y2units`, `reference`.
#'
#' @author EDG
#' @return `xt` object.
#' @export

as.xt.list <- function(x) {
  # Check types
  inherits_test(x, "list")
  # inherits_test(x$x, "list")
  # inherits_test(x$y, "list")
  # inherits_test(x$y2, "list")
  inherits_test(x$xunits, "character")
  inherits_test(x$yunits, "character")
  inherits_test(x$y2units, "character")
  inherits_test(x$reference, "character")

  # Create `xt` object
  xt <- toxt(
    x = x$x,
    y = x$y,
    y2 = x$y2,
    group = x$group,
    xunits = x$xunits,
    yunits = x$yunits,
    y2units = x$y2units,
    reference = x$reference
  )
  return(xt)
} # /rtemisbio::as.xt.list


#' Plot method for `xt` object
#'
#' @param x `xt` object.
#' @param ... Additional arguments passed to [rtemis::dplot3_xt].
#'
#' @author EDG
#' @export

plot.xt <- function(x, ...) {
  dplot3_xt(x, ...)
} # /rtemisbio::plot.xt

# #' Aggregate method for `xt` object
# #'
# #' @param x `xt` object.
# #' @param group Character: Grouping variable.
# #' @param fn Function: Function to apply to each group.
# #'
# #' @author EDG
# #' @export
# aggregate.xt <- function(x, group, fn = mean) {
#   inherits_test(x, "xt")
#   # Aggregate all y and y2 timeseries by grouping in `group`
# }
