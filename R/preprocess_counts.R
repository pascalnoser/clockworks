#' Preprocess count data by filtering lowly expressed features and normalising the data
#'
#' Uses edgeR's `filterByExpr` and `normLibSizes` functions to filter lowly expressed
#' features and normalise the data.
#'
#' @param cd A `CircadianData` object with count data in the `results` slot
#' @param verbose Logical, whether a message should be printed
#'
#' @importFrom edgeR DGEList filterByExpr normLibSizes
#'
#' @returns The updated `CircadianData` object with preprocessed data in the
#'   `results` slot
#'
preprocess_counts <- function(cd, verbose = TRUE) {
  # Get counts and group
  counts <- get_dataset(cd)
  group <- get_metadata(cd)$group

  # Filter lowly expressed features
  dge <- edgeR::DGEList(counts = counts, group = group)
  keep <- suppressWarnings(edgeR::filterByExpr(dge)) # Suppress warning if only one group
  removed <- sum(!keep)
  if (isTRUE(verbose)) {
    message(sprintf(
      "%d/%d genes removed due to low expression.",
      removed,
      nrow(counts)
    ))
  }
  dge <- dge[keep, , keep.lib.sizes = FALSE]

  # Normalise library sizes
  dge <- edgeR::normLibSizes(dge)

  # Add back to cd object
  cd@dataset <- dge$counts

  return(cd)
}
