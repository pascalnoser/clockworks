#' Detect rhythmicity with LimoRhyde
#'
#' This function runs rhythmicity detection with LimoRhyde
#'
#' @param cd A `CircadianData` object.
#' @param method_args Additional parameters passed to `<method_function>`
#'
#' @returns A list with the original and formatted results of the LimoRhyde analysis.
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
#' results <- clockworks:::analyze_limorhyde(cd)
#' head(results)
analyze_limorhyde <- function(cd, method_args = list()) {
  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_limorhyde(cd)

  # # Remove group column later if added temporarily by check
  # added_group <- ifelse(is.na(cd_local$n_groups), TRUE, FALSE)

  # Prepare inputs
  ls_inputs <- prepare_limorhyde(cd_local)

  # Run rhythmicity analysis
  df_res <- execute_limorhyde(ls_inputs, cd_local$type, method_args)

  #########

  # Create empty list for results
  ls_res_groups = list()

  # Run rhythmicity detection for each group separately
  groups <- unique(metadata(cd_local)[["group"]])
  for (grp in groups) {
    # Prepare inputs
    ls_inputs <- prepare_limorhyde(cd_local, grp)

    # Run rhythmicity analysis
    df_res_grp <- execute_limorhyde(ls_inputs, grp, method_args)

    # Add to list
    ls_res_groups[[grp]] <- df_res_grp
  }

  # Postprocessing
  ls_res <- format_limorhyde(ls_res_groups, added_group)

  return(ls_res)
}
