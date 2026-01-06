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
  cd_filt <- filter_samples(cd, group == grp)

  # If there are replicates, take median
  if (replicates == TRUE) {
    t_split <- split(get_metadata(cd_filt), get_metadata(cd_filt)[["time"]])
    timepoints <- as.numeric(names(t_split))

    names(t_split) <- paste0("CT_", timepoints)

    ls_meds <- lapply(t_split, function(df) {
      sample_IDs <- rownames(df)
      df_vals <- get_dataset(cd_filt)[, sample_IDs]
      apply(df_vals, 1, median)
    })

    # Prepare data
    df_dat <- t(data.frame(ls_meds))
  } else {
    df_dat <- t(get_dataset(cd_filt))
    timepoints <- get_metadata(cd_filt)[["time"]]
  }

  # Create list with inputs for run
  inputs <- list(
    spectrum = list(
      x = df_dat,
      algorithm = "rank",
      t = timepoints, # Only relevant for "regression" but doesn't change anything for "rank"
      # noOfPermutations = 300,
      periodicity.time = mean(cd_filt$period)
    ),
    gtest = list(
      y = NULL,  # Will be output of `robust.spectrum()`
      perm = FALSE,
      # noOfPermutations = 300,
      algorithm = "rank",
      t = timepoints  # Only relevant for "regression" but doesn't change anything for "rank"
    )
  )

  # If possible, add index value
  n_cycles <- unlist(cd_filt$n_cycles)
  if (!is.na(n_cycles)) {
    # Note: Round because otherwise e.g. 1.9 will result in
    # GeneCycle::g.statistic() (which is called by GeneCycle::robust.g.test())
    # extracting index 1 on line 19, although index 2 would be much closer to
    # 1.9. It's just a quirk of indexing in R that vec[1.9] return the first
    # element of vec rather than an error.
    inputs$gtest$index <- round(n_cycles*2 + 1)
  }

  return(inputs)
}
