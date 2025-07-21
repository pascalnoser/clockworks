#' Detect rhythmicity with meta2d
#'
#' This function runs rhythmicity detection with meta2d
#'
#' @param cd A `CircadianData` object.
#' @param method_args Additional parameters passed to `MetaCycle::meta2d()`
#'
#' @returns A data frame with the results of the meta2d analysis.
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
#' cd <- clockworks:::add_experiment_info(cd, period = 24, data_type = "norm")
#' results <- clockworks:::analyze_meta2d(cd)
#' head(results)
analyze_meta2d <- function(cd, method_args = list()) {
  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_meta2d(cd)

  # Remove group column later if added temporarily by check
  added_group <- ifelse(is.na(cd_local$n_groups), TRUE, FALSE)

  # Create empty list for results
  ls_res_groups = list()
  ls_harm_groups = list()

  # Run rhythmicity detection for each group separately
  groups <- unique(metadata(cd_local)[["group"]])
  for (grp in groups) {
    # Prepare inputs
    inputs <- prepare_meta2d(cd_local, grp)

    # Run rhythmicity analysis
    ls_res_grp <- execute_meta2d(inputs, grp, method_args)

    # Run harmonic regression
    df_harm_grp <- estimate_wave_params(cd_local, grp)

    # Add to list
    ls_res_groups[[grp]] <- ls_res_grp
    ls_harm_groups[[grp]] <- df_harm_grp
  }

  # Postprocessing
  ls_res <- format_meta2d(ls_res_groups = ls_res_groups,
                          ls_harm_groups = ls_harm_groups,
                          added_group = added_group,
                          log_transformed = cd_local$log_transformed,
                          log_base = cd_local$log_base)

  return(ls_res)
}
