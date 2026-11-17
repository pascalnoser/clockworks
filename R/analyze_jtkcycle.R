#' Detect rhythmicity with JTK_CYCLE
#'
#' This function runs rhythmicity detection JTK_CYCLE
#'
#' @param cd A `CircadianData` object.
#' @param method_args Additional parameters passed to `<method_function>`
#' @param batch_correction Logical. If `TRUE` and the data contain repeated
#'   measures, subject batch effects are removed before the analysis.
#'
#' @returns A data frame with the results of the JTK_CYCLE analysis.
analyze_jtkcycle <- function(cd, method_args = list(),
                             batch_correction = TRUE) {
  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_jtkcycle(cd, batch_correction)

  # Remove group column later if added temporarily by check
  added_group <- ifelse(is.na(cd_local$n_groups), TRUE, FALSE)

  # Create empty list for results
  ls_res_groups <- list()

  # Run rhythmicity detection for each group separately
  groups <- unique(get_metadata(cd_local)[["group"]])
  # Note: warn_once() shows each warning only once, because groups with the
  # same design would otherwise repeat them (e.g. about the tested periods)
  warn_once(
    for (grp in groups) {
      # Prepare inputs
      inputs <- prepare_jtkcycle(cd_local, grp)

      # Run rhythmicity analysis
      ls_res_grp <- execute_jtkcycle(inputs, grp, method_args)

      # Add to list
      ls_res_groups[[grp]] <- ls_res_grp
    }
  )

  # Postprocessing
  t_min <- min(get_metadata(cd_local)$time)
  ls_res <- format_jtkcycle(
    ls_res_groups = ls_res_groups,
    w_params = get_wave_params(cd_local),
    added_group = added_group,
    t_min = t_min,
    log_transformed = cd_local$log_transformed,
    log_base = cd_local$log_base
  )

  # Remove potential results to allow for filtering of CD object later on
  cd_local@results <- list()

  return(ls_res)
}
