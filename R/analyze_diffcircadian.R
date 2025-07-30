#' Detect rhythmicity with diffCircadian
#'
#' This function runs rhythmicity detection with diffCircadian
#'
#' @param cd A `CircadianData` object.
#' @param method_args Additional parameters passed to `<method_function>`
#'
#' @returns A list with the original and formatted results of the diffCircadian analysis.
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
#' results <- clockworks:::analyze_diffcircadian(cd)
#' head(results)
analyze_diffcircadian <- function(cd, method_args = list()) {
  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_diffcircadian(cd)

  # Remove group column later if added temporarily by check
  added_group <- ifelse(is.na(cd_local$n_groups), TRUE, FALSE)

  # Get reshaped values and metadata
  inputs <- prepare_diffcircadian(cd_local)

  # Run rhythmicity analysis
  df_res <- execute_diffcircadian(inputs, method_args)

  # Postprocessing
  ls_res <- format_diffcircadian(
    res_original = df_res,
    w_params = wave_params(cd_local),
    period = mean(cd_local$period),
    added_group = added_group,
    log_transformed = cd_local$log_transformed,
    log_base = cd_local$log_base
  )

  return(ls_res)
}
