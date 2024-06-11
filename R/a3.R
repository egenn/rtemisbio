# a3.R
# ::rtemisbio::
# 2024 EDG rtemis.org

#' Convert amino acid + annotations to JSON
#' 
#' @details 
#' We choose to keep NULL elements as empty lists in JSON, since we want users to be
#' able to easily add annotations, whether programmaticaly, using a web app, or 
#' manually.
#'
#' @param seq Character: Amino acid sequence.
#' @param site Named list of vectors of integer indices of sites, e.g.
#' `list("N-terminal repeat" = c(46, 47, 52), "Microtubule binding domain" = c(244, 245, 246))`
#' @param region Named list of character vectors with index range of regions in format
#' `start:end`, e.g. `list(Phosphodegron = c("46:51", "149:154"), KXGS = c("259:262", "290:293"))`
#' @param ptm Named list of vectors with indices of post-translational modifications, e.g.
#' `list("Phosphorylation" = c(17, 18, 29, 30), "Acetylation" = c(148, 150, 163))`
#' @param clv Named list of cleavage sites, e.g.
#' `list(CTSL = c(54, 244, 319), CTSD = c(340, 391, 426))`
#' @param variant List of lists with variant information. Each list must contain a
#' `Position` element
#'
#' @author EDG
#' @return `a3` object
#' @export

toa3 <- function(
    seq, site = NULL, region = NULL, ptm = NULL, clv = NULL,
    variant = NULL, uniprotid = NULL, reference = NULL) {
  # Check types
  inherits_test(seq, "character")
  inherits_test(site, "list")
  inherits_test(region, "list")
  inherits_test(ptm, "list")
  inherits_test(clv, "list")
  inherits_test(variant, "list")
  inherits_test(uniprotid, "character")
  inherits_test(reference, "character")

  # Convert to JSON
  a3 <- list(
    Sequence = seq,
    Annotations = list(
      Site = site,
      Region = region,
      PTM = ptm,
      Cleavage_site = clv,
      Variant = variant
    ),
    UniprotID = uniprotid,
    Reference = reference
  )
  class(a3) <- c("a3", "list")
  return(a3)
} # /rtemisbio::toa3


#' Print method for `a3` object
#'
#' @method print a3
#' @param x `a3` object.
#' @param ... Not used.
#'
#' @author EDG
#' @export

print.a3 <- function(x, head.n = 10, ...) {
  cat(".:", orange("a3", bold = TRUE), " object (Amino Acid Annotation)\n", sep = "")
  if (!is.null(x$UniprotID)) {
    cat("   Uniprot ID:", hilite(x$UniprotID), "\n")
  }
  site_annotations <- names(x$Annotations$Site)
  region_annotations <- names(x$Annotations$Region)
  ptm_annotations <- names(x$Annotations$PTM)
  n_clv_annotations <- length(x$Annotations$Cleavage_site)
  n_variant_annotations <- length(x$Annotations$Variant)
  # Head of sequence
  cat("     Sequence: ", bold(head(x$Sequence, head.n)), "...", " (length = ", length(x$Sequence), ")\n", sep = "")
  # Names of annotations
  cat("  Annotations:\n")
  if (is.null(site_annotations) && is.null(region_annotations) && is.null(ptm_annotations) && n_clv_annotations == 0 && n_variant_annotations == 0) {
    cat(italic("             None\n"))
  }
  if (length(site_annotations) > 0) {
    cat("          ", rtemis:::gray(italic("Site:")), paste(green(site_annotations), collapse = ", "), "\n")
  }
  if (length(region_annotations) > 0) {
    cat("        ", rtemis:::gray(italic("Region:")), paste(green(region_annotations), collapse = ", "), "\n")
  }
  if (length(ptm_annotations) > 0) {
    cat("           ", rtemis:::gray(italic("PTM:")), paste(green(ptm_annotations), collapse = ", "), "\n")
  }
  if (n_clv_annotations > 0) {
    cat(" ", rtemis:::gray(italic("Cleavage site:")), bold(n_clv_annotations), "annotations.\n")
  }
  if (n_variant_annotations > 0) {
    cat("      ", rtemis:::gray(italic("Variants:")), bold(n_variant_annotations), "variant annotations.\n")
  }
  if (!is.null(x$Reference)) {
    cat("    Reference:", hilite(x$Reference), "\n")
  }
} # /rtemisbio::print.a3


#' as.a3
#'
#' @param x Object to convert to `a3`.
#'
#' @author EDG
#' @return `a3` object.
#' @export

as.a3 <- function(x) {
  UseMethod("as.a3")
} # /rtemisbio::as.a3

#' as.a3
#'
#' @param x Object to convert to `a3`.
#' 
#' @author EDG
#' @return `a3` object.
#' @export

as.a3.default <- function(x) {
  inherits_test(x, "list")
  as.a3.list(x)
} # /rtemisbio::as.a3


#' as.a3.list method
#'
#' @param x List: Named list with elements `Sequence`, `Annotations`, `UniprotID`.
#' `Annotations` is a named list with possible elements `Site`, `Region`, `PTM`, 
#' `Cleavage_site`, `Variant`.
#'
#' @author EDG
#' @return `a3` object.
#' @export

as.a3.list <- function(x) {
  # Check types
  inherits_test(x, "list")
  inherits_test(x$Sequence, "character")
  inherits_test(x$Annotations, "list")
  inherits_test(x$Annotations$Site, "list")
  inherits_test(x$Annotations$Region, "list")
  inherits_test(x$Annotations$PTM, "list")
  inherits_test(x$Annotations$Cleavage_site, "list")
  inherits_test(x$Annotations$Variant, "list")
  inherits_test(x$UniprotID, "character")
  inherits_test(x$Reference, "character")

  # Convert to `a3` object
  a3 <- toa3(
    seq = x$Sequence,
    site = x$Annotations$Site,
    region = x$Annotations$Region,
    ptm = x$Annotations$PTM,
    clv = x$Annotations$Cleavage_site,
    variant = x$Annotations$Variant,
    uniprotid = x$UniprotID,
    reference = x$Reference
  )
  return(a3)
} # /rtemisbio::as.a3.list


#' Plot method for `a3` object
#'
#' @param x `a3` object.
#' @param ... Additional arguments passed to `rtemis::dplot3_protein`.
#'
#' @author EDG
#' @export

plot.a3 <- function(x, ...) {
  dplot3_protein(x, ...)
} # /rtemisbio::plot.a3
