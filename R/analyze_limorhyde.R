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
#' cd <- clockworks:::add_experiment_info(cd, period = 24, data_type = "norm")
#' results <- clockworks:::analyze_limorhyde(cd)
#' head(results)
analyze_limorhyde <- function(cd, method_args = list()) {
  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_limorhyde(cd)

  # Prepare inputs
  inputs <- prepare_limorhyde(cd_local)

  # Run rhythmicity analysis
  groups <- unique(metadata(cd_local)[["group"]])
  df_res <- execute_limorhyde(inputs, groups, method_args)

  # Postprocessing
  ls_res <- format_limorhyde(df_res, mean(cd_local$period))

  return(ls_res)
}
