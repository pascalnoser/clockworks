#' TimeCycle CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with TimeCycle.
#'
#' @param cd A `CircadianData` object
#' @param batch_correction Logical. If `TRUE` and the data contain repeated
#'   measures, subject batch effects are removed before the analysis.
#'
#' @returns A `CircadianData` object
check_timecycle <- function(cd, batch_correction = TRUE) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # TimeCycle can only test a single period, so use the mean if a range is given
  cd_local <- use_mean_period(cd_local, "TimeCycle")

  # Turn to logCPM values if we have count data
  if (cd_local$data_type == "count") {
    cd_local <- convert_to_cpm(cd_local, log = TRUE)
  }

  # If we have repeated measures, remove subject batch effect (unless turned
  # off by the user)
  if (cd_local$repeated_measures == TRUE && isTRUE(batch_correction)) {
    cd_local <- remove_batch_effects(cd_local)
  }

  # Add temporary group if there is no group column
  if (is.na(cd_local$n_groups)) {
    df_meta_temp <- get_metadata(cd_local)
    df_meta_temp[["group"]] <- "tmp"
    metadata(cd_local) <- df_meta_temp
  }

  # Rename samples to follow TimeCycle conventions
  cd_local <- order_samples(cd_local, "time")
  df_meta_temp <- get_metadata(cd_local)
  rep_counts <- sequence(rle(df_meta_temp$time)$lengths)
  new_ids <- paste0("ZT_", df_meta_temp$time, "_R", rep_counts)
  colnames(cd_local) <- new_ids

  # Make sure samples are ordered by time and group (and subject ID if relevant)
  sort_cols <- intersect(
    c("time", "group", "subject_ID"),
    colnames(get_metadata(cd_local))
  )
  cd_local <- order_samples(cd_local, sort_cols)

  # Remove potential results to allow for filtering of CD object later on
  cd_local@results <- list()

  return(cd_local)
}
