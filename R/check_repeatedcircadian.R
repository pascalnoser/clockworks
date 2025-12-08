#' RepeatedCircadian CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with RepeatedCircadian.
#'
#' @param cd A `CircadianData` object
#'
#' @returns A `CircadianData` object
check_repeatedcircadian <- function(cd) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # If for some reason the user has picked this method but there are no repeated
  # measures, throw an error.
  if (cd_local$repeated_measures == FALSE) {
    stop(
      "The selected method 'RepeatedCircadian' is designed for data ",
      "with repeated measures, but no 'colname_subject' has been defined. ",
      "Please define a 'colname_subject' or select a different method.",
      call. = FALSE
    )
  }

  # If there is only one subject throw an error because RepeatedCircadian needs
  # multiple subjects.
  if (!nlevels(metadata(cd_local)$subject_ID) > 1) {
    stop(
      "The 'colname_subject' column has only 1 level. RepeatedCircadian ",
      "requires multiple subjects with repeated measures.",
      call. = FALSE
    )
  }

  # Normalise if count data
  if (cd_local$data_type == "count") {
    cd_local <- normalise_dataset(cd_local)
  }

  # Add temporary group if there is no group column
  if (is.na(cd_local$n_groups)){
    # Add column
    df_meta_temp <- metadata(cd_local)
    df_meta_temp[["group"]] <- "tmp"

    # Add back to CD object
    metadata(cd_local) <- df_meta_temp
  }

  # Make sure samples are ordered by time and subject
  cd_local <- order_samples(cd_local, c("time", "group", "subject_ID"))

  return(cd_local)
}
