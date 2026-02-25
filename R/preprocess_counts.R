#' Preprocess count data by filtering lowly expressed features and normalising the data
#'
#' Uses edgeR's `filterByExpr` and `normLibSizes` functions to filter lowly expressed
#' features and normalise the data.
#'
#' @param cd A `CircadianData` object with count data in the `results` slot.
#' @param filter_counts A logical value specifying whether lowly expressed
#'   features should be filtered out from count data.
#' @param verbose Logical, whether a message should be printed.
#'
#' @importFrom edgeR DGEList filterByExpr normLibSizes
#'
#' @returns The updated `CircadianData` object with preprocessed data in the
#'   `results` slot
#'
preprocess_counts <- function(cd, filter_counts = TRUE, verbose = TRUE) {
  # Get counts and group
  counts <- get_dataset(cd)
  meta <- get_metadata(cd)
  group <- meta$group

  dge <- edgeR::DGEList(counts = counts, group = group)

  # Filter lowly expressed features
  if (isTRUE(filter_counts)) {
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
  }

  # Normalise library sizes
  dge <- edgeR::normLibSizes(dge)

  # Add normalisation factors to metadata
  # TODO: Probably add this to a new slot in the CD object rather than the metadata
  meta$norm_factors <- dge$samples$norm.factors
  metadata(cd) <- meta

  # Add back to cd object
  dataset(cd) <- dge$counts

  return(cd)
}
