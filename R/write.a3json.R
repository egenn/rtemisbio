# write.a3json.R
# ::rtemisbio::
# 2024 EDG rtemis.org

#' Write `a3` object to JSON file
#'
#' @param x `a3` object, as created by `as.a3()`.
#' @param filepath Character: Path to save JSON file.
#'
#' @author EDG
#' @export

write.a3json <- function(x, filepath, overwrite = FALSE) {
  # Check types ----
  inherits_test(x, "a3")
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

} # /rtemisbio::write.a3json
