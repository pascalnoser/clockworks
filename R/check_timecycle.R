#' TimeCycle CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with TimeCycle.
#'
#' @param cd A `CircadianData` object
#'
#' @returns A `CircadianData` object
check_timecycle <- function(cd) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # Normalise if count data
  if (cd_local$data_type == "count") {
    cd_local <- normalise_dataset(cd_local)
  }

  # Add temporary group if there is no group column
  if (is.na(cd_local$n_groups)){
    df_meta_temp <- metadata(cd_local)
    df_meta_temp[["group"]] <- "tmp"
    metadata(cd_local) <- df_meta_temp
  }

  # Rename samples to follow TimeCycle conventions
  cd_local <- order_samples(cd_local, "time")
  df_meta_temp <- metadata(cd_local)
  rep_counts <- sequence(rle(df_meta_temp$time)$lengths)
  new_ids <- paste0("ZT_", df_meta_temp$time, "_R", rep_counts)
  colnames(cd_local) <- new_ids

  # Make sure samples are ordered by time and group (and subject ID if relevant)
  sort_cols <- intersect(c("time", "group", "subject_ID"), colnames(metadata(cd_local)))
  cd_local <- order_samples(cd_local, sort_cols)

  return(cd_local)
}
