#' TimeCycle prep
#'
#' This function is used to create the input required to run rhythmicity
#' detection with TimeCycle.
#'
#' @param cd A `CircadianData` object
#' @param grp A string specifying a value in the "group" column of the metadata
#'   slot of `cd` which is used for filtering.
#'
#' @returns A list with inputs for `execute_timecycle()`
prepare_timecycle <- function(cd, grp) {
  # TODO: Figure out what to do with `cores` argument of TimeCycle

  # Filter CD object by group
  cd_filt <- filter_samples(cd, group == grp)

  # Define number of replicates at each time point. Do NOT include time points
  # with 0 replicates, so don't use cd_filt$n_replicates which includes these if
  # present
  repLabel <- table(get_metadata(cd_filt)$time)

  # Change maxLag if time-series is under 48 hours in length (under 2 cycles) or
  # if sampling interval is higher than 2 hours
  if (is.na(cd_filt$delta_t)) {
    # Leave at default if delta_t is NA
    maxLag <- 5
  } else {
    if (cd_filt$delta_t > 2 || cd_filt$n_cycles[[grp]] < 2) {
      maxLag <- 3
    } else {
      # Default value is 5
      maxLag <- 5
    }
  }

  # Create list with inputs for run
  inputs <- list(
    data = get_dataset(cd_filt),
    repLabel = repLabel,
    period = mean(cd_filt$period),
    # cores = 1, # TODO: Figure out what do to with this
    maxLag = maxLag
  )

  return(inputs)
}
