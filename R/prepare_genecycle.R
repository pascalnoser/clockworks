#' GeneCycle prep
#'
#' This function is used to create the input required to run rhythmicity
#' detection with GeneCycle.
#'
#' @param cd A `CircadianData` object
#' @param grp A string specifying a value in the "group" column of the metadata
#'   slot of `cd` which is used for filtering.
#'
#' @returns A list with inputs for `execute_genecycle()`
prepare_genecycle <- function(cd, grp) {
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
    df_dat <- t(data.frame(ls_meds))
  } else {
    df_dat <- t(dataset(cd_filt))
    timepoints <- metadata(cd_filt)[["time"]]
  }

  # Create list with inputs for run
  # Add default values for everything
  inputs <- list(
    robust_spectrum = list(
      x = df_dat,
      algorithm = "rank",
      t = timepoints,
      periodicity.time = FALSE,
      noOfPermutations = 300
    ),
    robust_g_test = list(
      y = NULL,  # Will be output of `robust.spectrum()`
      index = NA,  # TODO: Add this
      perm = FALSE,
      x = NULL,
      noOfPermutations = 300,
      algorithm = "rank",
      t = timepoints  # Only relevant for `algorithm = "regression"` but doesn't change anything for "rank"
    )
  )

  return(inputs)
}
