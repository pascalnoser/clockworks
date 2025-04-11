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
  if ("_group" %in% meta_cnames && length(unique(metadata(cd)[["_group"]])) > 1) {
    cd$group_info = TRUE
  } else {
    cd$group_info = FALSE
  }

  # Add repeated measures info
  if ("_subject_ID" %in% meta_cnames) {
    cd$repeated_measures = TRUE
  } else {
    cd$repeated_measures = FALSE
  }

  return(cd)
}
