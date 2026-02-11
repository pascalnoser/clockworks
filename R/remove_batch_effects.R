#' Remove batch effect from repeated measures
#'
#' Removes subject-level batch effects from a CircadianData object using
#' limma's removeBatchEffect function. Handles both single and multiple group designs.
#'
#' @param cd A `CircadianData` object with repeated measures
#' @param verbose Logical, whether a message should be printed
#'
#' @importFrom limma removeBatchEffect
#'
#' @returns A `CircadianData` object with batch effects removed from the dataset
#'
remove_batch_effects <- function(cd, verbose = TRUE) {
  if (verbose == TRUE) {
    message("\nRemoving subject batch effects")
  }
  if (is.na(cd$n_groups) | cd$n_groups == 1) {
    # None or one group
    dataset_corrected <- limma::removeBatchEffect(
      x = get_dataset(cd),
      batch = get_metadata(cd)$subject_ID
    )
  } else {
    # Multiple groups
    dataset_corrected <- limma::removeBatchEffect(
      x = get_dataset(cd),
      batch = get_metadata(cd)$subject_ID,
      group = get_metadata(cd)$group
    )
  }
  cd@dataset <- dataset_corrected
  cd
}
