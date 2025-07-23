#' GeneCycle CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with GeneCycle.
#'
#' @param cd A `CircadianData` object
#'
#' @returns A `CircadianData` object
check_genecycle <- function(cd) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # Add temporary group if there is no group column
  if (is.na(cd_local$n_groups)){
    df_meta_temp <- metadata(cd_local)
    df_meta_temp[["group"]] <- "tmp"
    metadata(cd_local) <- df_meta_temp
  }

  # Make sure samples are ordered by time and group (and subject ID if relevant)
  sort_cols <- intersect(c("time", "group", "subject_ID"), colnames(metadata(cd_local)))
  cd_local <- order_samples(cd_local, sort_cols)

  if (any(unlist(cd_local$n_replicates) > 1)) {
    message(
      "WARNING: 'GeneCycle' was selected, but replicates were detected at one or ",
      "more time points. Since GeneCycle doesn't support replicates, clockworks will ",
      "aggregate them by taking the median at each time point. To use a different ",
      "approach (e.g., concatenation), please preprocess the data manually or ",
      "choose a method that supports replicates."
    )
    # TODO: Print data frame showing time, number of replicates and group?
  }

  return(cd_local)
}
