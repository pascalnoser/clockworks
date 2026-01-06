#' Detect rhythmicity with meta2d
#'
#' This function runs rhythmicity detection with meta2d
#'
#' @param cd A `CircadianData` object.
#' @param method_args Additional parameters passed to `MetaCycle::meta2d()`
#'
#' @returns A data frame with the results of the meta2d analysis.
analyze_meta2d <- function(cd, method_args = list()) {
  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_meta2d(cd)

  # Remove group column later if added temporarily by check
  added_group <- ifelse(is.na(cd_local$n_groups), TRUE, FALSE)

  # Create empty list for results
  ls_res_groups <- list()

  # Run rhythmicity detection for each group separately
  groups <- unique(get_metadata(cd_local)[["group"]])
  for (grp in groups) {
    # Prepare inputs
    inputs <- prepare_meta2d(cd_local, grp)

    # Run rhythmicity analysis
    ls_res_grp <- execute_meta2d(inputs, grp, method_args)

    # Add to list
    ls_res_groups[[grp]] <- ls_res_grp
  }

  # Postprocessing
  ls_res <- format_meta2d(
    ls_res_groups = ls_res_groups,
    w_params = get_wave_params(cd_local),
    added_group = added_group,
    log_transformed = cd_local$log_transformed,
    log_base = cd_local$log_base
  )

  return(ls_res)
}
