#' LimoRhyde CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with LimoRhyde.
#'
#' @param cd A `CircadianData` object
#'
#' @returns A `CircadianData` object
check_limorhyde <- function(cd) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # Get sine and cosine component
  df_meta_temp <- get_metadata(cd_local)
  df_meta_temp$time_sin <- sin(
    2 * pi / mean(cd_local$period) * df_meta_temp$time
  )
  df_meta_temp$time_cos <- cos(
    2 * pi / mean(cd_local$period) * df_meta_temp$time
  )

  # Add to meta data
  metadata(cd_local) <- df_meta_temp

  # Make sure samples are ordered by time and group (and subject ID if relevant)
  sort_cols <- intersect(
    c("time", "group", "subject_ID"),
    colnames(get_metadata(cd_local))
  )
  cd_local <- order_samples(cd_local, sort_cols)

  # If there are repeated measures but no replicates, continue as if there were
  # no repeated measures to prevent error
  reps <- unlist(cd_local$n_replicates)
  if (
    is.na(cd_local$n_groups) &&
      cd_local$repeated_measures == TRUE &&
      all(reps == 1)
  ) {
    message(
      "The data contains repeated measures but no replicates. In order to ",
      "prevent errors, clockworks will treat the samples as independent instead."
    )
    cd_local$repeated_measures <- FALSE
    cd_local@metadata$subject_ID <- NULL
  }

  # Remove potential results to allow for filtering of CD object later on
  results(cd_local) <- list()

  return(cd_local)
}
