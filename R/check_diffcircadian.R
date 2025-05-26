#' diffCircadian CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with diffCircadian.
#'
#' @param cd A `CircadianData` object
#'
#' @returns A `CircadianData` object
check_diffcircadian <- function(cd) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # Make sure samples are ordered by time and group
  sort_cols <- intersect(c("time", "group"), colnames(metadata(cd_local)))
  cd_local <- order_samples(cd_local, sort_cols)

  # Get meta data
  df_meta_temp <- metadata(cd_local)

  # Add temporary group if there is no group column
  if (is.na(cd_local$n_groups)){
    df_meta_temp[["group"]] <- "tmp"
  }

  # Add replicate column (each group separately)
  time_group <- paste0(df_meta_temp$time, df_meta_temp$group)
  df_meta_temp[["replicate"]] <- sequence(rle(time_group)$lengths)
  metadata(cd_local) <- df_meta_temp

  # Make sure samples are ordered by time and group (and subject ID if relevant)
  sort_cols <- c("time", "group", "replicate")
  cd_local <- order_samples(cd_local, sort_cols)

  return(cd_local)
}
