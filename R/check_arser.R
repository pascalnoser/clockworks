#' ARSER CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with ARSER.
#'
#' @param cd A `CircadianData` object
#'
#' @returns A `CircadianData` object
check_arser <- function(cd) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # Extract meta data to add necessary columns
  df_meta_temp <- metadata(cd_local)

  # Add temporary group if there is no group column
  if (is.na(cd_local$n_groups)){
    df_meta_temp[[".group"]] <- "tmp"
  }

  # Add meta data back to CD object
  metadata(cd_local) <- df_meta_temp

  # Make sure samples are ordered by time and group (and subject ID if relevant)
  sort_cols <- intersect(c(".time", ".group", ".subject_ID"), colnames(df_meta_temp))
  cd_local <- order_samples(cd_local, sort_cols)

  # If there are replicates, print message warning the user that ARSER is
  # not designed for handling replicates, that we will take the median at
  # each time point and rename the samples.
  # Calculation of medians will be handled by `prepare_arser()`
  if (any(unlist(cd_local$n_replicates) > 1)) {
    message(
      "WARNING: The 'ARSER' method was selected for analysis; however, clockworks ",
      "detected replicates at one or more time points within at least one ",
      "group (if applicable). Since ARSER does not support replicate handling, ",
      "clockworks will aggregate replicates by computing the median at each ",
      "time point. These median values will be used in the analysis and ",
      "labeled as 'CT_<timepoint>'. If you prefer an alternative approach ",
      "(e.g. concatenating replicates), please preprocess the data manually ",
      "and rerun clockworks. Alternatively, consider using a different method ",
      "that supports replicates."

    )
    # TODO: Print data frame showing time, number of replicates and group?

    # if (is.table(cd_local$n_replicates)) {
    #   df_rpl <- as.data.frame(cd_local$n_replicates)
    #   colnames(df_rpl) <- c("time", "n_replicates")
    #   print(df_rpl)
    # } else {
    #   ls_repl <- cd_local$n_replicates
    #   ls_repl <- lapply(ls_repl, function(grp) {
    #     df_grp <- as.data.frame(grp)
    #     colnames(df_grp) <- c("time", "n_replicates")
    #     return(df_grp)
    #   })
    #   print(ls_repl)
    # }

  }

  return(cd_local)
}
