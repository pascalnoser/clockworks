#' dryR CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with dryR.
#'
#' @param cd A `CircadianData` object
#' @param batch_correction Logical. If `TRUE` and the data contain repeated
#'   measures, subject batch effects are removed before the analysis. Skipped
#'   with a warning for count data, where removal can produce negative counts.
#'
#' @returns A `CircadianData` object
check_dryr <- function(cd, batch_correction = TRUE) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # dryR can only test a single period, so use the mean if a range is given
  cd_local <- use_mean_period(cd_local, "dryR")

  # If we have repeated measures, remove subject batch effect (unless turned
  # off by the user)
  if (cd_local$repeated_measures == TRUE && isTRUE(batch_correction)) {
    if (cd_local$data_type == "norm") {
      cd_local <- remove_batch_effects(cd_local)
    } else if (cd_local$data_type == "count") {
      warning(
        "The data contain repeated measures and are count data. ",
        "Removing subject batch effects would produce negative counts, ",
        "which dryR cannot handle, so batch correction is skipped. ",
        "Set `batch_correction = FALSE` to silence this warning, or consider ",
        "LimoRhyde, which models subjects directly.",
        call. = FALSE
      )
    }
  }

  # Add temporary group if there is no group column
  if (is.na(cd_local$n_groups)) {
    df_meta_temp <- get_metadata(cd_local)
    df_meta_temp[["group"]] <- "tmp"
    metadata(cd_local) <- df_meta_temp
  }

  # Make sure samples are ordered by time and group (and subject ID if relevant)
  sort_cols <- intersect(
    c("time", "group", "subject_ID"),
    colnames(get_metadata(cd_local))
  )
  cd_local <- order_samples(cd_local, sort_cols)

  # If data type is count, round the dataset to integers
  if (experiment_info(cd_local)$data_type == "count") {
    dataset(cd_local) <- round(dataset(cd_local))
  }

  # Remove potential results to allow for filtering of CD object later on
  cd_local@results <- list()

  return(cd_local)
}
