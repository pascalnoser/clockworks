#' dryR CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with dryR.
#'
#' @param cd A `CircadianData` object
#' @param remove_batch_effects Logical. If `TRUE` and the data contain repeated
#'   measures, subject batch effects are removed before the analysis.
#'
#' @returns A `CircadianData` object
check_dryr <- function(cd, remove_batch_effects = TRUE) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # If we have repeated measures, remove subject batch effect (unless turned
  # off by the user)
  # TODO: Figure out what to do if we have count data and removing batch
  # effects results in negative values
  if (cd_local$repeated_measures == TRUE && isTRUE(remove_batch_effects)) {
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

  # If data type is count, round the dataset to integers and make sure all values
  # are non-negative
  if (experiment_info(cd_local)$data_type == "count") {
    dataset(cd_local) <- round(dataset(cd_local))
    # dataset(cd_local)[dataset(cd_local) < 0] <- 0
  }

  # Remove potential results to allow for filtering of CD object later on
  results(cd_local) <- list()

  return(cd_local)
}
