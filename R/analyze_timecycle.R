#' Detect rhythmicity with TimeCycle
#'
#' This function runs rhythmicity detection with TimeCycle
#'
#' @param cd A `CircadianData` object.
#' @param method_args Additional parameters passed to `<TimeCycle::TimeCycle()>`
#'
#' @returns A list with the original and formatted results of the TimeCycle
#'   analysis.
analyze_timecycle <- function(cd, method_args = list()) {
  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_timecycle(cd)

  # Remove group column later if added temporarily by check
  added_group <- ifelse(is.na(cd_local$n_groups), TRUE, FALSE)

  # Create empty list for results
  ls_res_groups <- list()

  # Run rhythmicity detection for each group separately
  groups <- unique(get_metadata(cd_local)[["group"]])
  for (grp in groups) {
    # Prepare inputs
    inputs <- prepare_timecycle(cd_local, grp)

    # Run rhythmicity analysis
    df_res_grp <- execute_timecycle(inputs, grp, method_args)

    # Add to list
    ls_res_groups[[grp]] <- df_res_grp
  }

  # Pass number of resamplings to formatting function to correct p-values (10000 is default)
  resamplings <- ifelse("resamplings" %in% names(method_args), method_args[["resamplings"]], 10000)

  # Postprocessing
  ls_res <- format_timecycle(
    ls_res_groups = ls_res_groups,
    w_params = get_wave_params(cd_local),
    resamplings = resamplings,
    added_group = added_group
  )

  return(ls_res)
}
