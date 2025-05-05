#' ARSER prep
#'
#' This function is used to create the input required to run rhythmicity
#' detection with ARSER.
#'
#' @param cd A `CircadianData` object
#' @param grp A string specifying a value in the "group" column of the metadata
#'   slot of `cd` which is used for filtering.
#' @param added_group Logical, whether the `check_arser()` method added the
#'   "group" column
#'
#' @returns A list with inputs for `execute_arser()`
prepare_arser <- function(cd, grp, added_group) {
  # Check if there are replicates in any group
  # Note: Check all groups, not just this, to make sure we rename the samples in
  # all groups so they are consistent
  replicates <- ifelse(any(unlist(cd$n_replicates) > 1), TRUE, FALSE)

  # Filter CD object by group
  cd_filt <- filter_samples(cd, col = "group", value = grp)

  # If there are replicates, take median
  if (replicates == TRUE) {
    t_split <- split(metadata(cd_filt), metadata(cd_filt)[["time"]])
    timepoints <- as.numeric(names(t_split))

    names(t_split) <- paste0("CT_", timepoints)

    ls_meds <- lapply(t_split, function(df) {
      sample_IDs <- rownames(df)
      df_vals <- dataset(cd_filt)[, sample_IDs]
      apply(df_vals, 1, median)
    })

    # Prepare data
    df_input <- data.frame(feature = rownames(dataset(cd_filt)), ls_meds)
  } else {
    df_input <- data.frame(feature = rownames(dataset(cd_filt)), dataset(cd_filt))
    timepoints <- metadata(cd_filt)[["time"]]
  }

  # Create list with default inputs for run
  ls_inputs <- list(
    inDF = df_input,
    infile = paste("Group", grp), # Using inDF, but can't be empty
    filestyle = "csv", # Irrelevant, but needs to be either "csv" or "txt"
    timepoints = timepoints,
    minper = min(cd_filt$period),
    maxper = max(cd_filt$period),
    ARSdefaultPer = mean(cd_filt$period), # Must be minper < ARSdefaultPer > maxper
    cycMethod = "ARS",
    parallelize = FALSE,
    nCores = 1,
    outputFile = FALSE,
    releaseNote = TRUE
  )

  return(ls_inputs)
}
