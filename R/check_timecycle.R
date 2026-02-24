#' TimeCycle CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with TimeCycle.
#'
#' @param cd A `CircadianData` object
#'
#' @importFrom edgeR cpm
#'
#' @returns A `CircadianData` object
check_timecycle <- function(cd) {
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

  return(cd_local)
}
