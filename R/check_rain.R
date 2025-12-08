#' RAIN CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with RAIN.
#'
#' @param cd A `CircadianData` object
#'
#' @importFrom limma removeBatchEffect
#'
#' @returns A `CircadianData` object
check_rain <- function(cd) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # Normalise if count data
  if (cd_local$data_type == "count") {
    cd_local <- normalise_dataset(cd_local)
  }

  # If we have repeated measures, remove subject batch effect
  # TODO: Make this optional
  if (cd_local$repeated_measures == TRUE) {
    message("Removing subject batch effect before RAIN analysis")
    if (is.na(cd_local$n_groups) | cd_local$n_groups == 1) {
      # None or one group
      dataset_corrected <- limma::removeBatchEffect(
        x = dataset(cd_local),
        batch = metadata(cd_local)$subject_ID
      )
    } else {
      # Multiple groups
      dataset_corrected <- limma::removeBatchEffect(
        x = dataset(cd_local),
        batch = metadata(cd_local)$subject_ID,
        group = metadata(cd_local)$group
      )
    }
    cd_local@dataset = dataset_corrected
  }

  # Extract meta data to add necessary columns
  df_meta_temp <- metadata(cd_local)

  # Add temporary group if there is no group column
  if (is.na(cd_local$n_groups)){
    df_meta_temp[["group"]] <- "tmp"
  }

  # Add meta data back to CD object
  metadata(cd_local) <- df_meta_temp


  # Make sure samples are ordered by time and group (and subject ID if relevant)
  sort_cols <- intersect(c("time", "group", "subject_ID"), colnames(df_meta_temp))
  cd_local <- order_samples(cd_local, sort_cols)

  return(cd_local)
}
