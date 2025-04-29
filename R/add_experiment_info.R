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
  # TODO: Remove "n_groups" and replace with "groups"?
  groups <- unique(metadata(cd)[[".group"]])
  n_groups <- length(groups)

  if (".group" %in% meta_cnames) {
    cd$n_groups <- n_groups
    # cd$groups <- groups
  } else {
    cd$n_groups <- NA
    # cd$groups <- NA
  }


  # Add replicate info ----
  # Make sure to show all time points in every group
  # Option 1: Table ----
  t_unique <- sort(unique(metadata(cd)[[".time"]]))

  if (all(is.na(cd$n_groups))) {
    cd$n_replicates <- table(factor(metadata(cd)[[".time"]], levels = t_unique))
  } else {
    grp_split <- split(metadata(cd), metadata(cd)[[".group"]])
    cd$n_replicates <- lapply(grp_split, function(x) {
      table(factor(x[[".time"]], levels = t_unique))
    })
  }

  # cd$replicates <- ifelse(any(unlist(cd$n_replicates) > 1), TRUE, FALSE)


  # ## Option 2: data frame ----
  # t_unique <- sort(unique(metadata(cd)[[".time"]]))
  #
  # if (all(is.na(cd$groups))) {
  #   rep_table <- table(factor(metadata(cd)[[".time"]], levels = t_unique))
  #   rep_df <- as.data.frame(rep_table)
  #   colnames(rep_df) <- c("time", "n_replicates")
  #   cd$n_replicates <- rep_df
  # } else {
  #   grp_split <- split(metadata(cd), metadata(cd)[[".group"]])
  #   cd$n_replicates <- lapply(grp_split, function(x) {
  #     rep_table <- table(factor(x[[".time"]], levels = t_unique))
  #     rep_df <- as.data.frame(rep_table)
  #     colnames(rep_df) <- c("time", "n_replicates")
  #     rep_df
  #   })
  # }


  # Add repeated measures info ----
  if (".subject_ID" %in% meta_cnames) {
    cd$repeated_measures <- TRUE
  } else {
    cd$repeated_measures <- FALSE
  }

  return(cd)
}
