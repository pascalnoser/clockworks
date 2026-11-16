#' diffCircadian CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with diffCircadian.
#'
#' @param cd A `CircadianData` object
#' @param batch_correction Logical. If `TRUE` and the data contain repeated
#'   measures, subject batch effects are removed before the analysis.
#'
#' @returns A `CircadianData` object
check_diffcircadian <- function(cd, batch_correction = TRUE) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # diffCircadian can only test a single period, so use the mean if a range is given
  cd_local <- use_mean_period(cd_local, "diffCircadian")

  # Turn to logCPM values if we have count data
  if (cd_local$data_type == "count") {
    cd_local <- convert_to_cpm(cd_local, log = TRUE)
  }

  # If we have repeated measures, remove subject batch effect (unless turned
  # off by the user)
  if (cd_local$repeated_measures == TRUE && isTRUE(batch_correction)) {
    cd_local <- remove_batch_effects(cd_local)
  }

  # Make sure samples are ordered by time and group
  sort_cols <- intersect(c("time", "group"), colnames(get_metadata(cd_local)))
  cd_local <- order_samples(cd_local, sort_cols)

  # Get meta data
  df_meta_temp <- get_metadata(cd_local)

  # Add temporary group if there is no group column
  if (is.na(cd_local$n_groups)) {
    df_meta_temp[["group"]] <- "tmp"
  }

  # Add replicate column (each group separately)
  time_group <- paste0(df_meta_temp$time, df_meta_temp$group)
  df_meta_temp[["replicate"]] <- sequence(rle(time_group)$lengths)
  metadata(cd_local) <- df_meta_temp

  # Make sure samples are ordered by time and group (and subject ID if relevant)
  sort_cols <- c("time", "group", "replicate")
  cd_local <- order_samples(cd_local, sort_cols)

  # Remove potential results to allow for filtering of CD object later on
  cd_local@results <- list()

  return(cd_local)
}
