# a3_utils.R
# ::rtemisbio::
# 2024 EDG rtemis.org

#' Get AlphaFold info for a given UniProt ID
#'
#' @param uniprotid Character: UniProt ID
#'
#' @return data frame with AlphaFold info
#'
#' @author EDG
#' @export
get_alphafold <- function(uniprotid) {
  url <- paste0("https://www.alphafold.ebi.ac.uk/api/prediction/", uniprotid)
  headers <- c(
    "accept" = "application/json"
  )
  response <- GET(url, add_headers(.headers = headers))
  content <- content(response, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(content)
} # /rtemisbio::get_alphafold

get_alphafold_pdb <- function(uniprotid) {
  get_alphafold(uniprotid)$pdb
} # /rtemisbio::get_alphafold_pdb
