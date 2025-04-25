#' Add experiment information to CircadianData object
#'
#' @param cd A `CircadianData` object.
#' @param period A number defining the period length of the circadian data.
#'
#' @returns The `CircadianData` object with added experiment info
add_experiment_info <- function(cd, period) {
  # Get meta data column names
  meta_cnames <- colnames(metadata(cd))

  # Add period ----
  cd$period <- period


  # Add group info ----
  # TODO: Remove "n_groups" and replace with "groups" ?
  groups <- unique(metadata(cd)[[".group"]])
  n_groups <- length(groups)

  if (".group" %in% meta_cnames) {
    cd$n_groups <- n_groups
    cd$groups <- groups
  } else {
    cd$n_groups <- NA
    cd$groups <- NA
  }


  # Add replicate info ----
  # Make sure to show all time points in every group
  t_unique <- sort(unique(metadata(cd)[[".time"]]))

  if (all(is.na(cd$groups))) {
    cd$replicates <- table(factor(metadata(cd)[[".time"]], levels = t_unique))
  } else {
    grp_split <- split(metadata(cd), metadata(cd)[[".group"]])
    cd$replicates <- lapply(grp_split, function(x) {
      table(factor(x[[".time"]], levels = t_unique))
    })
  }


  # Add repeated measures info ----
  if (".subject_ID" %in% meta_cnames) {
    cd$repeated_measures <- TRUE
  } else {
    cd$repeated_measures <- FALSE
  }

  return(cd)
}
