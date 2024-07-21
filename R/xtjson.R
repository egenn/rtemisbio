# write.xtjson.R
# ::rtemisbio::
# 2024 EDG rtemis.org

#' Write `xt` object to JSON file
#'
#' @param x `xt` object, as created by [toxt] or [as.xt].
#' @param filepath Character: Path to save JSON file.
#'
#' @author EDG
#' @export

write.xtjson <- function(x, filepath, overwrite = FALSE) {
  # Check types ----
  inherits_test(x, "xt")
  inherits_test(filepath, "character")

  # Check dependencies ----
  dependency_check("jsonlite")

  # Normalize path ----
  filepath <- normalizePath(filepath, mustWork = FALSE)

  # Check file ----
  if (file.exists(filepath) && !overwrite) {
    stop(
      "File ", filepath, " exists.\033[0m",
      italic("\n  Set", hilite("`overwrite = TRUE`"), "if you wish to overwrite.")
    )
  }

  # Save to file
  jsonlite::write_json(
    x = as.list(x),
    path = filepath,
    simplifyVector = TRUE,
    simplifyMatrix = FALSE
  )

} # /rtemisbio::write.xtjson


# read.xtjson.R
# ::rtemisbio::
# 2024 EDG rtemis.org

#' Read `xt` object from JSON file
#'
#' @details Note that factors saved under `group` are written as character by [write.xtjson] and
#' when they are read back in, they are converted back to factors using [factor]. This means that
#' the levels will be set alphabetically. If needed, reorder them after reading in the JSON file
#' using [factor].
#' 
#' @param filepath Character: Path to JSON file.
#' @param verbosity Integer: if greater than 0, print messages.
#'
#' @author EDG
#' @return `xt` object.
#' @export

read.xtjson <- function(filepath, verbosity = 0L) {
  # Check types ----
  inherits_test(filepath, "character")

  # Check dependencies ----
  dependency_check("jsonlite")

  # Normalize path ----
  filepath <- normalizePath(filepath)

  # Check file ----
  if (!file.exists(filepath)) {
    stop("File", filepath, "does not exist.")
  }

  # Read from file
  xt <- jsonlite::read_json(
    path = filepath,
    simplifyVector = TRUE,
    simplifyMatrix = FALSE
  )
  # Convert empty lists to NULL
  emptylist.idi <- sapply(xt, is.list) & sapply(xt, length) == 0
  xt[emptylist.idi] <- NULL

  # Convert groups to factors
  if (!is.null(xt$group)) {
    xt$group <- lapply(xt$group, factor)
  }

  # Convert to `xt` object
  xt <- as.xt(xt)

  if (verbosity > 0) {
    cat("Read", filepath, ":\n", sep = "")
    print(xt)
  }

  return(xt)
} # /rtemisbio::read.xtjson
