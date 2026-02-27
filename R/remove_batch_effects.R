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
  # Check if cd object contains repeated measures
  if (!experiment_info(cd)$repeated_measures) {
    stop(
      "Cannot remove batch effects because no repeated measures (subject_ID) found in metadata.",
      call. = FALSE
    )
  }

  # If there is only one subject, we cannot remove batch effects, so return the original object
  if (length(unique(get_metadata(cd)$subject_ID)) == 1) {
    warning(
      "Not removing batch effects because only one subject found in metadata."
    )
    return(cd)
  }

  if (verbose == TRUE) {
    message("\nRemoving subject batch effects")
  }
  if (is.na(cd$n_groups) | cd$n_groups == 1) {
    # None or one group
    # Note: Suppressing the message about the function assuming a one-group design
    dataset_corrected <- suppressMessages(limma::removeBatchEffect(
      x = get_dataset(cd),
      batch = get_metadata(cd)$subject_ID
    ))
  } else {
    # Multiple groups
    dataset_corrected <- limma::removeBatchEffect(
      x = get_dataset(cd),
      batch = get_metadata(cd)$subject_ID,
      group = get_metadata(cd)$group
    )
  }
  dataset(cd) <- dataset_corrected
  return(cd)
}
