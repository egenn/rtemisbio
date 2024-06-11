# read.a3json.R
# ::rtemisbio::
# 2024 EDG rtemis.org

#' Read `a3` object from JSON file
#'
#' @param filepath Character: Path to JSON file.
#'
#' @author EDG
#' @return `a3` object.
#' @export

read.a3json <- function(filepath) {
  # Check types ----
  inherits_test(filepath, "character")

  # Check dependencies ----
  dependency_check("jsonlite")

  # Normalize path ----
  filepath <- normalizePath(filepath, mustWork = TRUE)

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

  return(a3)
} # /rtemisbio::read.a3json
