# seq_ops.R
# ::rtemisbio::
# 2024 EDG rtemis.org

#' Get the sequence of a gene
#' 
#' @param gene Character: Gene name.
#' @param organism Character: Organism name.
#' @param biomart Character: Biomart name.
#' @param host Character: Host address.
#' 
#' @author EDG
#' @return data.frame with columns "gene", "ensembl_transcript_id" and "sequence".
#' @export

gene2sequence <- function(
    gene,
    organism = "hsapiens",
    biomart = "ensembl",
    host = "https://www.ensembl.org",
    seq_type = "coding",
    verbosity = 1) {
  
  # Check dependencies ----
  dependency_check("biomaRt")

  # Arguments ----
  stopifnot(is.character(gene))

  # Mart ----
  mart <- biomaRt::useMart(
    biomart = biomart,
    dataset = paste(organism, "_gene_ensembl", sep = ""),
    host = host
  )

  # Get transcript ID ----
  # Use gene name as filter and get transcript ID (ensembl_transcript_id)
  transcripts <- biomaRt::getBM(
    attributes = c("ensembl_gene_id", "ensembl_transcript_id"),
    filters = "hgnc_symbol",
    values = gene,
    mart = mart
  )

  if (verbosity > 0) {
    msg20(
      "Found ", bold(nrow(transcripts)), 
      " transcripts for gene ", hilite(gene), "."
    )
  }

  # Get sequence ----
  # Retrieve sequence(s) using transcript ID
  sequence <- biomaRt::getSequence(
    id = transcripts$ensembl_transcript_id,
    type = "ensembl_transcript_id",
    seqType = seq_type,
    mart = mart,
    verbose = verbosity > 1
  )

  if (verbosity > 0) {
    # Count number of sequences returned that are not "Sequence unavailable"
    nretrieved <- sum(sequence$coding != "Sequence unavailable")
    msg20(
      "Retrieved ", bold(nretrieved), "/", nrow(sequence), " sequences."
    )
  }

  seq <- data.frame(
    gene = gene,
    ensembl_transcript_id = sequence$ensembl_transcript_id,
    sequence = sequence$coding
  )

  return(seq)

} # rtemisbio::gene2sequence
