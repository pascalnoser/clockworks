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
  cd_filt <- filter_samples(cd, group == .env$grp, renormalise = FALSE)

  # If there are replicates, take median
  if (replicates == TRUE) {
    t_split <- split(get_metadata(cd_filt), get_metadata(cd_filt)[["time"]])
    timepoints <- as.numeric(names(t_split))

    names(t_split) <- paste0("CT_", timepoints)

    ls_meds <- lapply(t_split, function(df) {
      sample_IDs <- rownames(df)
      df_vals <- get_dataset(cd_filt)[, sample_IDs, drop = FALSE]
      apply(df_vals, 1, median, na.rm = TRUE)
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
      # FALSE runs the regression-based algorithm without a period
      periodicity.time = if (is_no_period(cd_filt$period)) FALSE else cd_filt$period
    ),
    gtest = list(
      y = NULL, # Will be output of `robust.spectrum()`
      perm = FALSE,
      # noOfPermutations = 300,
      algorithm = "rank",
      t = timepoints # Only relevant for "regression" but doesn't change anything for "rank"
    )
  )

  # Sampling interval, used by execute_genecycle() to convert spectrum indices
  # into periods
  delta_t <- cd_filt$delta_t
  inputs$delta_t <- if (is.null(delta_t)) NA else delta_t

  # GeneCycle treats the rows as equally spaced, so time points that are
  # missing entirely shift the frequencies of the spectrum
  if (
    !is.na(inputs$delta_t) &&
      any(abs(diff(sort(unique(timepoints))) - inputs$delta_t) > 1e-6)
  ) {
    warning(
      "GeneCycle assumes equally spaced time points, but some time points ",
      "are missing entirely. The p-values and period estimates of GeneCycle ",
      "may be unreliable.",
      call. = FALSE
    )
  }

  # If possible, add index value
  # Note: robust.spectrum() zero-pads the autocorrelation to twice the length of
  # the time series, so index k corresponds to the period
  # 2 * n * delta_t / (k - 1), with n the number of (equally spaced) rows that
  # GeneCycle sees. The number of rows is used instead of the time range in
  # `n_cycles`, because GeneCycle assumes each row is one sampling step.
  if (!is_no_period(cd_filt$period) && !is.na(inputs$delta_t)) {
    n_cycles <- nrow(df_dat) * inputs$delta_t / cd_filt$period
    # Note: Round because otherwise e.g. 1.9 will result in
    # GeneCycle::g.statistic() (which is called by GeneCycle::robust.g.test())
    # extracting index 1 on line 19, although index 2 would be much closer to
    # 1.9. It's just a quirk of indexing in R that vec[1.9] return the first
    # element of vec rather than an error.
    inputs$gtest$index <- round(n_cycles * 2 + 1)
  }

  return(inputs)
}
