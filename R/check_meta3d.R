#' meta3d CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with meta3d.
#'
#' @param cd A `CircadianData` object
#'
#' @returns A `CircadianData` object
check_meta3d <- function(cd) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # Turn to logCPM values if we have count data
  if (cd_local$data_type == "count") {
    cd_local <- convert_to_cpm(cd_local, log = TRUE)
  }

  # Make sure samples are ordered by time and group (and subject ID if relevant)
  sort_cols <- intersect(
    c("time", "group", "subject_ID"),
    colnames(metadata(cd_local))
  )
  cd_local <- order_samples(cd_local, sort_cols)

  # Remove potential results to allow for filtering of CD object later on
  results(cd_local) <- list()

  return(cd_local)
}
