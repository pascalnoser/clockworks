#' ARSER prep
#'
#' This function is used to create the input required to run rhythmicity
#' detection with ARSER.
#'
#' @param cd A `CircadianData` object
#' @param grp A string specifying a value in the ".group" column of the metadata
#'   slot of `cd` which is used for filtering.
#'
#' @returns A list with inputs for `execute_arser()`
prepare_arser <- function(cd, grp, added_group) {
  # Filter CD object by group
  cd_filt <- filter_samples(cd, col = ".group", value = grp)

  # Get table of replicates
  if (added_group == TRUE) {
    rep_table <- cd_filt$replicates
  } else {
    rep_table <- cd_filt$replicates[[grp]]
  }

  # If there are replicates, take median
  if (any(rep_table > 1)) {
    t_split <- split(metadata(cd_filt), metadata(cd_filt)$.time)
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
    timepoins <- metadata(cd_filt)[[".time"]]
  }



  # Create list with inputs for run
  ls_inputs <- list(
    inDF = df_input,
    timepoints = timepoints,
    minper = min(cd_filt$period),
    maxper = max(cd_filt$period),
    group = grp
  )

  return(ls_inputs)
}
