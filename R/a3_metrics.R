# a3_metrics.R
# ::rtemisbio::
# 2024 EDG rtemis.org

#' Jaccard index
#'
#' @param x Character vector. Will be coerced to character.
#' @param y Character vector. Will be coerced to character.
#'
#' @return Numeric: Jaccard index.
#'
#' @author EDG
#' @export

jaccard_index <- function(x, y) {
  x <- unique(as.character(x))
  y <- unique(as.character(y))
  n_x_and_y <- length(intersect(x, y))
  n_x_or_y <- length(union(x, y))
  n_x_and_y / n_x_or_y
} # /rtemisbio::jaccard_index


#' Dice Coefficient
#'
#' @param x Character vector. Will be coerced to character.
#' @param y Character vector. Will be coerced to character.
#'
#' @return Numeric: Dice coefficient.
#'
#' @author EDG
#' @export

dice_coefficient <- function(x, y) {
  x <- unique(as.character(x))
  y <- unique(as.character(y))
  n_x <- length(x)
  n_y <- length(y)
  n_x_and_y <- length(intersect(x, y))
  2 * n_x_and_y / (n_x + n_y)
} # /rtemisbio::dice_coefficient


#' Overlap Coefficient
#'
#' Overlap Coefficient a.k.a. Szymkiewicz-Simpson coefficient.
#' 
#' @param x Character vector. Will be coerced to character.
#' @param y Character vector. Will be coerced to character.
#'
#' @return Numeric: Overlap coefficient.
#'
#' @author EDG
#' @export

overlap_coefficient <- function(x, y) {
  x <- unique(as.character(x))
  y <- unique(as.character(y))
  n_x_and_y <- length(intersect(x, y))
  n_x <- length(x)
  n_y <- length(y)
  n_x_and_y / min(n_x, n_y)
} # /rtemisbio::overlap_coefficient


#' Cosine similarity
#'
#' @param x Character vector. Will be coerced to character.
#' @param y Character vector. Will be coerced to character.
#'
#' @return Numeric: Cosine similarity.
#'
#' @author EDG
#' @export

cosine_similarity <- function(x, y, sequence) {
  xbin <- rep(0, length(sequence))
  xbin[x] <- 1
  ybin <- rep(0, length(sequence))
  ybin[y] <- 1
  sum(xbin * ybin) / sqrt(sum(xbin^2)) / sqrt(sum(ybin^2))
} # /rtemisbio::cosine_similarity


#' Pointwise Mutual Information
#'
#' @param x Character vector. Will be coerced to character.
#' @param y Character vector. Will be coerced to character.
#'
#' @return Numeric: Pointwise Mutual Information.
#'
#' @author EDG
#' @export

pmi <- function(x, y) {
  x <- unique(as.character(x))
  y <- unique(as.character(y))
  n_x_and_y <- length(intersect(x, y))
  n_x <- length(x)
  n_y <- length(y)
  n_x_and_y / (n_x * n_y)
} # /rtemisbio::pmi

pmi <- function(x, y, data) {
  px <- sum(x) / length(data)
  py <- sum(y) / length(data)
  pxy <- sum(x * y) / length(data)
  log2(pxy / (px * py))
}

#' Pairwise PTM similarity
#'
#' @param x List of PTM annotations
#' @param metric Character: "jaccard", "dice", "overlap", "cosine", "pmi"
#'
#' @return Matrix of pairwise similarity scores.
#'
#' @author EDG
#' @export

pairwise_similarity <- function(x, metric = "jaccard", sequence = NULL) {
  # Apply similarity metric on all pairs of elements in x
  n <- length(x)
  sim <- matrix(NA, nrow = n, ncol = n)
  for (i in seq_along(x)) {
    for (j in seq_along(x)) {
      sim[i, j] <- switch(metric,
        "jaccard" = jaccard_index(x[[i]], x[[j]]),
        "dice" = dice_coefficient(x[[i]], x[[j]]),
        "overlap" = overlap_coefficient(x[[i]], x[[j]]),
        "cosine" = cosine_similarity(x[[i]], x[[j]], sequence),
        "pmi" = pmi(x[[i]], x[[j]], x)
      )
    }
  }
  rownames(sim) <- colnames(sim) <- names(x)
  sim
} # /rtemisbio::pairwise_similarity
