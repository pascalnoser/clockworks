#' Detect rhythmicity with GeneCycle
#'
#' This function runs rhythmicity detection with GeneCycle
#'
#' @param cd A `CircadianData` object.
#' @param method_args Additional parameters passed to `GeneCycle:robust.g.test()`
#'
#' @returns A list with the original and formatted results of the GeneCycle analysis.
analyze_genecycle <- function(cd, method_args = list()) {
  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_genecycle(cd)

  # Remove group column later if added temporarily by check
  added_group <- ifelse(is.na(cd_local$n_groups), TRUE, FALSE)

  # Create empty list for results
  ls_res_groups <- list()

  # Run rhythmicity detection for each group separately
  groups <- unique(get_metadata(cd_local)[["group"]])
  for (grp in groups) {
    # Prepare inputs
    inputs <- prepare_genecycle(cd_local, grp)

    # Run rhythmicity analysis
    df_res_grp <- execute_genecycle(inputs, grp, method_args)

    # Add to list
    ls_res_groups[[grp]] <- df_res_grp
  }

  # Postprocessing
  ls_res <- format_genecycle(
    ls_res_groups = ls_res_groups,
    w_params = get_wave_params(cd_local),
    added_group = added_group
  )

  return(ls_res)
}
