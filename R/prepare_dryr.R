#' dryR prep
#'
#' This function is used to create the input required to run rhythmicity
#' detection with dryR.
#'
#' @param cd A `CircadianData` object
#' @param grp A string specifying a value in the "group" column of the metadata
#'   slot of `cd` which is used for filtering.
#'
#' @returns A list with inputs for `execute_dryr()`
prepare_dryr <- function(cd, grp) {
  # Filter CD object by group
  cd_filt <- filter_samples(cd, group == grp)

  if (cd_filt$data_type == "norm") {
    # Data will be analysed by `f_24()`
    inputs <- list(
      data = get_dataset(cd_filt),
      time = get_metadata(cd_filt)$time,
      period = mean(cd_filt$period),
      method = "f_24"  # will be removed later
    )
  } else if (cd_local$data_type == "count") {
    # Data will be analysed by `dryseq_single()`
    # Note: `group` and `single` are useful if you don't pre-filter your data
    # because they allow you to provide a vector with the group of each sample
    # (`group`) as well as which single group you want to analyse (`single`).
    # Because there is no default we need to define them, even if we have
    # filtered the data already.
    inputs <- list(
      countData = get_dataset(cd_filt),
      time = get_metadata(cd_filt)$time,
      period = mean(cd_filt$period),
      group = get_metadata(cd_filt)$group,
      single = grp,
      method = "dryseq_single"  # will be removed later
    )
  } else {
    stop("`data_type` of CircadianData object must be 'count' or 'norm'.")
  }

  return(inputs)
}
