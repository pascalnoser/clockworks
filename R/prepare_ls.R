#' Lomb-Scargle prep
#'
#' This function is used to create the input required to run rhythmicity
#' detection with Lomb-Scargle.
#'
#' @param cd A `CircadianData` object
#' @param grp A string specifying a value in the ".group" column of the metadata
#'   slot of `cd` which is used for filtering.
#'
#' @returns A list with inputs for `execute_ls()`
prepare_ls <- function(cd, grp) {
  # Filter CD object by group
  cd_filt <- filter_samples(cd, col = ".group", value = grp)

  # Prepare data
  df_input <- data.frame(feature = rownames(dataset(cd_filt)), dataset(cd_filt))

  # Create list with inputs for run
  ls_inputs <- list(
    inDF = df_input,
    timepoints = metadata(cd_filt)[[".time"]],
    minper = min(cd_filt$period),
    maxper = max(cd_filt$period),
    group = grp
  )

  return(ls_inputs)
}
