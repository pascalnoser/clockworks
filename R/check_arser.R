#' ARSER CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with ARSER.
#'
#' @param cd A `CircadianData` object
#'
#' @returns A `CircadianData` object
check_arser <- function(cd) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # Extract meta data to add necessary columns
  df_meta_temp <- metadata(cd_local)

  # Add temporary group if there is no group column
  if (is.na(cd_local$n_groups)){
    df_meta_temp[[".group"]] <- "tmp"
  }

  # Add meta data back to CD object
  metadata(cd_local) <- df_meta_temp

  # Make sure samples are ordered by time and group (and subject ID if relevant)
  if (cd_local$repeated_measures == TRUE) {
    cd_local <- order_samples(cd_local, c(".time", ".group", ".subject_ID"))
  } else {
    cd_local <- order_samples(cd_local, c(".time", ".group"))
  }

  # TODO: If there are replicates, print message warning the user that ARSER is
  # not designed for handling replicates and that we will take the median at
  # each time point. This also requires renaming the samples!
  # ...

  return(cd_local)
}
