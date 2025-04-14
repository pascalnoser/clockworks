#' Add experiment information to CircadianData object
#'
#' @param cd A `CircadianData` object.
#' @param period A number defining the period length of the circadian data.
#'
#' @returns The `CircadianData` object with added experiment info
#'
add_CD_info <- function(cd, period) {
  # Get meta data column names
  meta_cnames = colnames(metadata(cd))

  # Add period
  cd$period = period

  # Add group info
  n_groups <- length(unique(metadata(cd)[[".group"]]))

  if (".group" %in% meta_cnames) {
    cd$n_groups = n_groups
  } else {
    cd$n_groups = NA
  }

  # Add repeated measures info
  if (".subject_ID" %in% meta_cnames) {
    cd$repeated_measures = TRUE
  } else {
    cd$repeated_measures = FALSE
  }

  return(cd)
}
