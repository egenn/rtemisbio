# write.a3json.R
# ::rtemisbio::
# 2024 EDG rtemis.org

#' Write `a3` object to JSON file
#'
#' @param x `a3` object, as created by `as.a3()`.
#' @param filepath Character: Path to save JSON file.
#' 
#' @return Nothing. Writes JSON file.
#'
#' @author EDG
#' @export

write.a3json <- function(x, filepath, overwrite = FALSE) {
  # Check types ----
  check_inherits(x, "a3")
  check_inherits(filepath, "character")

  # Check dependencies ----
  check_dependencies("jsonlite")

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

} # /rtemisbio::write.a3json


# read.a3json.R
# ::rtemisbio::
# 2024 EDG rtemis.org

#' Read `a3` object from JSON file
#'
#' @param filepath Character: Path to JSON file.
#' @param verbosity Integer: if greater than 0, print messages.
#'
#' @return `a3` object.
#' 
#' @author EDG
#' @export

read.a3json <- function(filepath, verbosity = 0L) {
  # Check types ----
  check_inherits(filepath, "character")

  # Check dependencies ----
  check_dependencies("jsonlite")

  # Normalize path ----
  filepath <- normalizePath(filepath)

  # Check file ----
  if (!file.exists(filepath)) {
    stop("File", filepath, "does not exist.")
  }

  # Read from file
  a3 <- jsonlite::read_json(
    path = filepath,
    simplifyVector = TRUE,
    simplifyMatrix = FALSE
  ) |> as.a3()

  if (verbosity > 0) {
    cat("Read", filepath, ":\n", sep = "")
    print(a3)
  }
  a3
} # /rtemisbio::read.a3json
