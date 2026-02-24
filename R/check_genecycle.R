#' GeneCycle CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with GeneCycle.
#'
#' @param cd A `CircadianData` object
#'
#' @importFrom edgeR cpm
#'
#' @returns A `CircadianData` object
check_genecycle <- function(cd) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # Turn to logCPM values if we have count data
  if (cd_local$data_type == "count") {
    counts <- get_dataset(cd_local)
    logCPM <- edgeR::cpm(counts, log = TRUE)
    cd_local@dataset <- logCPM
  }

  # If we have repeated measures, remove subject batch effect
  if (cd_local$repeated_measures == TRUE) {
    cd_local <- remove_batch_effects(cd_local)
  }

  # Add temporary group if there is no group column
  if (is.na(cd_local$n_groups)) {
    df_meta_temp <- get_metadata(cd_local)
    df_meta_temp[["group"]] <- "tmp"
    metadata(cd_local) <- df_meta_temp
  }

  # Make sure samples are ordered by time and group (and subject ID if relevant)
  sort_cols <- intersect(
    c("time", "group", "subject_ID"),
    colnames(get_metadata(cd_local))
  )
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
