#' RAIN prep
#'
#' This function is used to create the input required to run rhythmicity
#' detection with RAIN.
#'
#' @param cd A `CircadianData` object
#' @param grp A string specifying a value in the "group" column of the metadata
#'   slot of `cd` which is used for filtering.
#'
#' @returns A list with inputs for `execute_rain()`
prepare_rain <- function(cd, grp) {
  # Filter CD object by group
  cd_filt <- filter_samples(cd, col = "group", value = grp)

  # Create list with inputs for execute method
  ls_inputs <- list(
    x = t(dataset(cd_filt)),
    deltat = cd_filt$delta_t,
    period = mean(cd_filt$period),
    measure.sequence = cd_filt$n_replicates[[grp]],
    method = ifelse(cd_filt$repeated_measures == TRUE, "longitudinal", "independent")
  )

  return(ls_inputs)
}
