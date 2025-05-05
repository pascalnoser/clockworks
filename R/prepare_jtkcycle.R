#' JTK_CYCLE prep
#'
#' This function is used to create the input required to run rhythmicity
#' detection with JTK_CYCLE.
#'
#' @param cd A `CircadianData` object
#' @param grp A string specifying a value in the "group" column of the metadata
#'   slot of `cd` which is used for filtering.
#'
#' @returns A list with inputs for `execute_jtkcycle()`
prepare_jtkcycle <- function(cd, grp) {
  # TODO: Figure out what to do about `parallelize` and `nCores`

  # Filter CD object by group
  cd_filt <- filter_samples(cd, col = "group", value = grp)

  # Prepare data
  df_input <- data.frame(feature = rownames(dataset(cd_filt)), dataset(cd_filt))

  # Create list with inputs for run
  ls_inputs <- list(
    inDF = df_input,
    infile = paste("Group", grp), # Using inDF, but can't be empty
    filestyle = "csv", # Irrelevant, but needs to be either "csv" or "txt"
    timepoints = metadata(cd_filt)[["time"]],
    minper = min(cd_filt$period),
    maxper = max(cd_filt$period),
    cycMethod = "JTK",
    parallelize = FALSE,
    nCores = 1,
    outputFile = FALSE,
    releaseNote = TRUE # Probably set to FALSE?
  )

  return(ls_inputs)
}
