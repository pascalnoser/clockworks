#' Detect rhythmicity with RAIN
#'
#' This function runs rhythmicity detection with RAIN
#'
#' @param cd A `CircadianData` object.
#' @param method_args Additional parameters passed to `<method_function>`
#'
#' @returns A list with the original and formatted results of the RAIN analysis.
analyze_rain <- function(cd, method_args = list()) {
  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_rain(cd)

  # Remove group column later if added temporarily by check
  added_group <- ifelse(is.na(cd_local$n_groups), TRUE, FALSE)

  # Create empty list for results
  ls_res_groups <- list()

  # Run rhythmicity detection for each group separately
  groups <- unique(get_metadata(cd_local)[["group"]])
  for (grp in groups) {
    # Prepare inputs
    inputs <- prepare_rain(cd_local, grp)

    # Run rhythmicity analysis
    df_res_grp <- execute_rain(inputs, grp, method_args)

    # Add to list
    ls_res_groups[[grp]] <- df_res_grp
  }

  # Postprocessing
  ls_res <- format_rain(
    ls_res_groups = ls_res_groups,
    w_params = get_wave_params(cd_local),
    t_min = min(get_metadata(cd_local)[["time"]]),
    deltat = cd_local$delta_t,
    period = cd_local$period,
    added_group = added_group
  )

  return(ls_res)
}
