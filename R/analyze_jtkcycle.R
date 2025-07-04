#' Detect rhythmicity with JTK_CYCLE
#'
#' This function runs rhythmicity detection JTK_CYCLE
#'
#' @param cd A `CircadianData` object.
#' @param method_args Additional parameters passed to `<method_function>`
#'
#' @returns A data frame with the results of the JTK_CYCLE analysis.
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
#' results <- clockworks:::analyze_jtkcycle(cd)
#' head(results)
analyze_jtkcycle <- function(cd, method_args = list()) {
  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_jtkcycle(cd)

  # Remove group column later if added temporarily by check
  added_group <- ifelse(is.na(cd_local$n_groups), TRUE, FALSE)

  # Create empty list for results
  ls_res_groups = list()

  # Run rhythmicity detection for each group separately
  groups <- unique(metadata(cd_local)[["group"]])
  for (grp in groups) {
    # Prepare inputs
    inputs <- prepare_jtkcycle(cd_local, grp)

    # Run rhythmicity analysis
    ls_res_grp <- execute_jtkcycle(inputs, grp, method_args)

    # Add to list
    ls_res_groups[[grp]] <- ls_res_grp
  }

  # Postprocessing
  ls_res <- format_jtkcycle(ls_res_groups, added_group, cd_local$log_transformed, cd_local$log_base)
  ls_res <- format_jtkcycle(ls_res_groups, added_group, FALSE, cd_local$log_base)

  return(ls_res)
}
