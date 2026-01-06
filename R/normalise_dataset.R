#' Turn RNA-seq counts to log2 CPM values
#'
#' @param cd A `CircadianData` object with count data in the `results` slot
#' @param verbose Logical, whether a message should be printed
#'
#' @importFrom edgeR DGEList filterByExpr normLibSizes cpm
#'
#' @returns The updated `CircadianData` object with log2 CPM values in the
#'   `results` slot
#'
normalise_dataset <- function(cd, verbose = TRUE) {
  if (verbose == TRUE) {
    message("\nTransforming data to log2 CPM values")
  }

  # Get counts and group
  counts <- get_dataset(cd)
  group <- get_metadata(cd)$group

  # Filter, normalise, and turn to log CPM values
  dge <- edgeR::DGEList(counts = counts, group = group)
  keep = edgeR::filterByExpr(dge)
  dge = dge[keep, , keep.lib.sizes=FALSE]
  dge <- edgeR::normLibSizes(dge)
  logCPM <- edgeR::cpm(dge, log = TRUE)

  # Add back to cd object
  cd@dataset <- logCPM

  return(cd)
}
