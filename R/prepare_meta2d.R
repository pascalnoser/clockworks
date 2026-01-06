#' meta2d prep
#'
#' This function is used to create the input required to run rhythmicity
#' detection with meta2d.
#'
#' @param cd A `CircadianData` object
#' @param grp A string specifying a value in the "group" column of the metadata
#'   slot of `cd` which is used for filtering.
#'
#' @returns A list with inputs for `execute_meta2d()`
prepare_meta2d <- function(cd, grp) {
  # Filter CD object by group
  cd_filt <- filter_samples(cd, group == grp)

  # Prepare data
  df_input <- data.frame(feature = rownames(cd_filt), get_dataset(cd_filt))

  # Create list with default inputs for run
  inputs <- list(
    inDF = df_input,
    infile = paste("Group", grp), # Using inDF, but can't be empty
    filestyle = "csv", # Irrelevant, but needs to be either "csv" or "txt"
    timepoints = get_metadata(cd_filt)[["time"]],
    minper = min(cd_filt$period),
    maxper = max(cd_filt$period),
    # ARSdefaultPer = mean(cd_filt$period), # Must be minper < ARSdefaultPer > maxper
    parallelize = FALSE,
    nCores = 1,
    outputFile = FALSE,
    releaseNote = FALSE
  )

  return(inputs)
}
