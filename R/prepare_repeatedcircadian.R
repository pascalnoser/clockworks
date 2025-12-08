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
  cd_filt <- filter_samples(cd, group == grp)

  # Create named list to return
  inputs <- list(
    dat = dataset(cd_filt),
    tt = metadata(cd_filt)[["time"]],
    id = metadata(cd_filt)[["subject_ID"]],
    period = mean(cd_filt$period)
  )

  return(inputs)
}
