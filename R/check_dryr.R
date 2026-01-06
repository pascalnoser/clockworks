#' dryR CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with dryR.
#'
#' @param cd A `CircadianData` object
#'
#' @returns A `CircadianData` object
check_dryr <- function(cd) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # Add temporary group if there is no group column
  if (is.na(cd_local$n_groups)){
    df_meta_temp <- get_metadata(cd_local)
    df_meta_temp[["group"]] <- "tmp"
    metadata(cd_local) <- df_meta_temp
  }

  # Make sure samples are ordered by time and group (and subject ID if relevant)
  sort_cols <- intersect(c("time", "group", "subject_ID"), colnames(get_metadata(cd_local)))
  cd_local <- order_samples(cd_local, sort_cols)

  return(cd_local)
}
