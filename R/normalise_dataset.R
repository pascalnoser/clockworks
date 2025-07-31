#' Turn RNA-seq counts to log2 CPM values
#'
#' @param counts A matrix with RNA-seq count data
#' @param group A vector or factor giving the experimental group or treatment
#'   condition for each sample
#' @param verbose Logical, whether a message should be printed
#'
#' @importFrom edgeR DGEList filterByExpr normLibSizes cpm
#'
#' @returns A matrix with log2 CPM values
#'
normalise_dataset <- function(counts, group = NULL, verbose = TRUE) {
  if (verbose == TRUE) {
    message("\nTransforming data to log2 CPM values")
  }

  dge <- edgeR::DGEList(counts = counts, group = group)
  keep = edgeR::filterByExpr(dge)
  dge = dge[keep, , keep.lib.sizes=FALSE]
  dge <- edgeR::normLibSizes(dge)
  logCPM <- edgeR::cpm(dge, log = TRUE)
  return(logCPM)
}
