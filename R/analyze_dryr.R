#' Detect rhythmicity with dryR
#'
#' This function runs rhythmicity detection with dryR
#'
#' @param cd A `CircadianData` object.
#' @param method_args Additional parameters passed to `dryR::f_24()` or
#'   `dryR::dryseq_single()`
#'
#' @returns A list with the original and formatted results of the dryR analysis.
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
#' results <- clockworks:::analyze_dryr(cd)
#' head(results)
analyze_dryr <- function(cd, method_args = list()) {
  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_dryr(cd)

  # Remove group column later if added temporarily by check
  added_group <- ifelse(is.na(cd_local$n_groups), TRUE, FALSE)

  # Create empty list for results
  ls_res_groups = list()

  # Run rhythmicity detection for each group separately
  groups <- unique(metadata(cd_local)[["group"]])
  for (grp in groups) {
    # Prepare inputs
    inputs <- prepare_dryr(cd_local, grp)

    # Run rhythmicity analysis
    df_res_grp <- execute_dryr(inputs, grp, method_args)

    # Add to list
    ls_res_groups[[grp]] <- df_res_grp
  }

  # Postprocessing
  ls_res <- format_dryr(
    ls_res_groups = ls_res_groups,
    w_params = wave_params(cd_local),
    method = inputs$method,
    added_group = added_group
  )

  return(ls_res)
}
