# xt.R
# ::rtemisbio::
# 2024 EDG rtemis.org

#' Create an `xt` object
#'
#' Creates an `xt` object from time series data.
#'
#' @param x Named list of datetime vectors.
#' @param y Named list of numeric vectors: When plotted, these will correspond to the
#' left y-axis.
#' @param x2 Named list of datetime vectors: When plotted, these will correspond to the
#' right x-axis. If not provided, `x` will be used for both `y` and `y2`.
#' @param y2 Named list of numeric vectors: When plotted, these will correspond to the
#' right y-axis.
#' @param zt Numeric vector: Zeitgeber time. If provided, this will be used to label x-axis ticks.
#' Assumes a single datetime vector in `x`. Elements in `zt` must correspond to elements in `x`.
#' @param shade Binary vector: `0` indicates no shading, `1` indicates shading. If provided,
#' this will be used to shade the plot.
#' @param group Named list of factors: Grouping variable(s).
#' @param xunits Character: Units for `x`.
#' @param yunits Character: Units for `y`.
#' @param y2units Character: Units for `y2`.
#' @param description Character: Description of the data / experiment.
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
    zt = NULL,
    shade = NULL,
    group = NULL,
    xunits = NULL,
    yunits = NULL,
    y2units = NULL,
    description = NULL,
    reference = NULL) {
  # Check types
  # inherits_check(x, "list")
  # inherits_check(y, "list")
  # inherits_check(y2, "list")
  inherits_check(xunits, "character")
  inherits_check(yunits, "character")
  inherits_check(y2units, "character")
  inherits_check(description, "character")
  inherits_check(reference, "character")

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
    zt = zt,
    Shade = shade,
    Group = group,
    xunits = xunits,
    yunits = yunits,
    y2units = y2units,
    Description = description,
    Reference = reference
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
  cat("  .:", orange("xt", bold = TRUE), " timeseries object\n", sep = "")
  if (!is.null(x$Description)) {
    cat("  Description:", hilite(x$Description), "\n")
  }
  length_x <- length(x$x)
  cat(
    "  ", hilite(length_x),
    " x time ", ngettext(length_x, "vector", "vectors"), " of ",
    ngettext(length_x, "length ", "lengths "),
    hilite(paste(sapply(x$x, length), collapse = ", ")), "\n",
    sep = ""
  )

  length_x2 <- length(x$x2)
  if (length_x2 > 0) {
    cat(
      "  ", hilite(length_x2),
      " x2 time ", ngettext(length_x2, "vector", "vectors"), " of ",
      ngettext(length_x2, "length ", "lengths "),
      hilite(paste(sapply(x$x2, length), collapse = ", ")), "\n",
      sep = ""
    )
  }

  length_y <- length(x$y)
  cat(
    "  ", hilite(length_y), " y timeseries: ",
    paste(hilite(names(x$y)), collapse = ", "), "\n",
    sep = ""
  )

  if (!is.null(x$yunits)) {
    cat("    Units of y timeseries:", paste(hilite(x$yunits), collapse = ", "), "\n")
  }

  length_y2 <- length(x$y2)
  if (length_y2 > 0) {
    cat(
      "  ", hilite(length_y2), " y2 timeseries: ",
      paste(hilite(names(x$y2)), collapse = ", "), "\n",
      sep = ""
    )
  }

  if (!is.null(x$y2units)) {
    cat("    Units of y2 timeseries:", paste(hilite(x$y2units), collapse = ", "), "\n")
  }

  if (!is.null(x$Group)) {
    cat(
      "  ", hilite(length(x$Group)), ngettext(length(x$Group), " grouping", " groupings"), ": ",
      paste(hilite(names(x$Group)), collapse = ", "), "\n",
      sep = ""
    )
  }
  if (!is.null(x$Reference)) {
    cat("  Reference:", hilite(x$Reference), "\n")
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
  inherits_check(x, "list")
  as.xt.list(x)
} # /rtemisbio::as.xt


#' as.xt.list method
#'
#' @param x List: Named list with elements `x`, `y`, `y2`, `xunits`, `yunits`, `y2units`, 
#' `Description`, `Reference`.
#'
#' @author EDG
#' @return `xt` object.
#' @export

as.xt.list <- function(x) {
  # Check types
  inherits_check(x, "list")
  # inherits_check(x$x, "list")
  # inherits_check(x$y, "list")
  # inherits_check(x$y2, "list")
  inherits_check(x$xunits, "character")
  inherits_check(x$yunits, "character")
  inherits_check(x$y2units, "character")
  inherits_check(x$Description, "character")
  inherits_check(x$Reference, "character")

  # Create `xt` object
  xt <- toxt(
    x = x$x,
    y = x$y,
    y2 = x$y2,
    zt = x$zt,
    shade = x$Shade,
    group = x$Group,
    xunits = x$xunits,
    yunits = x$yunits,
    y2units = x$y2units,
    description = x$Description,
    reference = x$Reference
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

#' Aggregate method for `xt` object
#'
#' @param x `xt` object.
#' @param group Character: Grouping variable.
#' @param fn Function: Function to apply to each group.
#' @param backend Character: "base", "data.table", or "dplyr"; backend to use for aggregation.
#' @param ... Additional arguments passed to `fn`.
#'
#' @author EDG
#' @export
aggregate.xt <- function(x, groupname, fn = mean, backend = getOption("rt.backend", "base"), ...) {
  inherits_check(x, "xt")
  # Get name of fn
  fn_name <- deparse(substitute(fn))
  # Aggregate all y and y2 timeseries by grouping in `group`
  if (backend == "base") {
    # base
    y_agg <- lapply(seq_along(x$y), function(i) {
      out <- aggregate(list(y = x$y[[i]]), by = list(x$Group[[groupname]]), FUN = fn, ...)
      names(out) <- c(groupname, fn_name)
      out
    })

    if (!is.null(x$y2)) {
      y2_agg <- lapply(seq_along(x$y2), function(i) {
        out <- aggregate(list(y2 = x$y2[[i]]), by = list(x$Group[[groupname]]), FUN = fn, ...)
        names(out) <- c(groupname, paste0(fn_name))
        out
      })
    }
  } else if (backend == "data.table") {
    # data.table
    y_agg <- lapply(seq_along(x$y), function(i) {
      data.table::data.table(y = x$y[[i]])[, list(agg = fn(y)), by = x$Group[[groupname]]] |>
        data.table::setorder() |>
        data.table::setnames(c(groupname, fn_name))
    })
    if (!is.null(x$y2)) {
      y2_agg <- lapply(seq_along(x$y2), function(i) {
        data.table::data.table(y2 = x$y2[[i]])[, list(agg = fn(y2)), by = x$Group[[groupname]]] |>
          data.table::setorder() |>
          data.table::setnames(c(groupname, fn_name))
      })
    }
  } else if (backend == "dplyr") {
    # dplyr
    y_agg <- lapply(seq_along(x$y), function(i) {
      dplyr::tibble(y = x$y[[i]]) |>
        dplyr::group_by(Group = x$Group[[groupname]]) |>
        dplyr::summarize(!!fn_name := fn(y, ...))
    })
    if (!is.null(x$y2)) {
      y2_agg <- lapply(seq_along(x$y2), function(i) {
        dplyr::tibble(y2 = x$y2[[i]]) |>
          dplyr::group_by(Group = x$Group[[groupname]]) |>
          dplyr::summarize(!!fn_name := fn(y2, ...))
      })
    }
  }

  out <- list(y = y_agg)
  if (!is.null(x$y2)) {
    out$y2 <- y2_agg
  }
  out
} # /rtemisbio::aggregate.xt


#' Calculate light/dark ratio for `xt` object
#'
#' Calculates light/dark ratio for each `y` and `y2` timeseries in an `xt` object.
#'
#' @param x `xt` object.
#' @param fn Function: Function to apply to each group.
#' @param backend Character: "base", "data.table", or "dplyr"; backend to use for aggregation.
#' @param ... Additional arguments passed to `fn`.
#'
#' @author EDG
#' @export
#' @return data.frame with columns for group and summary statistic.
light_dark_ratio <- function(x, groupname = "Lights", fn = mean, backend = getOption("rt.backend", "data.table"), ...) {
  # Check types
  inherits_check(x, "xt")
  aggregate(x, groupname = groupname, fn = fn, backend = backend, ...)
} # /rtemisbio::light_dark_ratio
