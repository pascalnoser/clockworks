#' GeneCycle CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with GeneCycle.
#'
#' @param cd A `CircadianData` object
#' @param batch_correction Logical. If `TRUE` and the data contain repeated
#'   measures, subject batch effects are removed before the analysis.
#' @param method_args The `method_args` passed to `clockworks()`. Used to check
#'   if GeneCycle is run without a period (no warning about a period range).
#'
#' @returns A `CircadianData` object
check_genecycle <- function(
  cd,
  batch_correction = TRUE,
  method_args = list()
) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # GeneCycle tests a single period, unless the user runs it without a period
  # ('index = NA' for the rank-based or 'periodicity.time = FALSE' for the
  # regression-based algorithm). Use the mean (with a warning) if a range is
  # given, unless the period is not used anyway.
  period_free <- (
    "index" %in% names(method_args) && isTRUE(is.na(method_args$index))
  ) ||
    isFALSE(method_args$periodicity.time)
  if (!period_free) {
    cd_local <- use_mean_period(cd_local, "GeneCycle")
  }

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

  # Make sure samples are ordered by time and group (and subject ID if relevant)
  sort_cols <- intersect(
    c("time", "group", "subject_ID"),
    colnames(get_metadata(cd_local))
  )
  cd_local <- order_samples(cd_local, sort_cols)

  if (any(unlist(cd_local$n_replicates) > 1)) {
    message(
      "WARNING: 'GeneCycle' was selected, but replicates were detected at one or ",
      "more time points. Since GeneCycle doesn't support replicates, clockworks will ",
      "aggregate them by taking the median at each time point. To use a different ",
      "approach (e.g., concatenation), please preprocess the data manually or ",
      "choose a method that supports replicates."
    )
  }

  # Remove potential results to allow for filtering of CD object later on
  cd_local@results <- list()

  return(cd_local)
}
