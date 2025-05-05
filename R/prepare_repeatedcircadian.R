#' RepeatedCircadian prep
#'
#' This function is used to create the input required to run rhythmicity
#' detection with RepeatedCircadian.
#'
#' @param cd A `CircadianData` object
#' @param grp A string specifying a value in the "group" column of the metadata
#'   slot of `cd` which is used for filtering.
#'
#' @returns A list with inputs for `execute_repeatedcircadian()`
prepare_repeatedcircadian <- function(cd, grp) {
  # Filter CD object by group
  cd_filt <- filter_samples(cd, col = "group", value = grp)

  # Get relevant parameters
  n_features <- nrow(cd_filt)
  tt <- metadata(cd_filt)[["time"]]
  subj_id <- metadata(cd_filt)[["subject_ID"]]
  period <- cd_filt$period

  # Create named list to return
  ls_inputs <- list(
    n_features = n_features,
    tt = tt,
    subj_id = subj_id,
    data_filt = dataset(cd_filt),
    period = period,
    group = grp
  )

  return(ls_inputs)
}
