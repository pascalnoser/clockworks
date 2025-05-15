#' Detect rhythmicity with RepeatedCircadian
#'
#' This function runs rhythmicity detection using RepeatedCircadian
#'
#' @param cd A `CircadianData` object.
#' @param method_args Additional parameters passed to
#'   `RepeatedCircadian::rpt_rhythmicity()`
#'
#'
#' @returns A list containing the original as and cleaned results of the
#'   RepeatedCircadian analysis.
#' @examples
#' data(cw_data)
#' data(cw_metadata)
#' cw_metadata <- clockworks::check_metadata(
#'   cw_metadata,
#'   colname_sample = "Sample_ID",
#'   colname_time = "Time",
#'   colname_group = "Group",
#'   colname_subject = "Subject_ID"
#' )
#' cd <- CircadianData(cw_data, cw_metadata)
#' cd <- clockworks:::add_experiment_info(cd, period = 24)
#' results <- clockworks:::analyze_repeatedcircadian(cd)
#' head(results$res_original)
analyze_repeatedcircadian <- function(cd, method_args = list()) {
  # TODO: DOUBLE CHECK WHAT THE OUTPUT COLUMNS ARE EXACTLY
  # TODO: DOUBLE CHECK WHAT THE OUTPUT COLUMNS ARE EXACTLY
  # TODO: DOUBLE CHECK WHAT THE OUTPUT COLUMNS ARE EXACTLY
  # TODO: DOUBLE CHECK WHAT THE OUTPUT COLUMNS ARE EXACTLY
  # TODO: DOUBLE CHECK WHAT THE OUTPUT COLUMNS ARE EXACTLY
  # TODO: DOUBLE CHECK WHAT THE OUTPUT COLUMNS ARE EXACTLY
  # TODO: DOUBLE CHECK WHAT THE OUTPUT COLUMNS ARE EXACTLY

  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_repeatedcircadian(cd)

  # Remove group column later if added temporarily by check
  added_group <- ifelse(is.na(cd_local$n_groups), TRUE, FALSE)

  ### ----- ###
  # # Prepare data
  # df_prep <- prepare_repeatedcircadian_long(cd_local)
  # df_prep[1:8, 1:8]
  #
  # # Now what?
  ### ----- ###

  # Create empty list for results
  ls_res_groups = list()

  # Run rhythmicity detection for each group separately
  groups <- unique(metadata(cd_local)[["group"]])
  for (grp in groups) {
    # TODO: Don't loop over groups
    # Prepare inputs
    ls_inputs <- prepare_repeatedcircadian(cd_local, grp)

    # Run rhythmicity analysis
    df_res_grp <- execute_repeatedcircadian(ls_inputs, grp, method_args)

    # Add to list
    ls_res_groups[[grp]] <- df_res_grp
  }

  # Postprocessing
  ls_res <- format_repeatedcircadian(ls_res_groups, added_group)

  return(ls_res)
}
