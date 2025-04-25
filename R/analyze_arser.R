#' Detect rhythmicity with ARSER
#'
#' This function runs rhythmicity detection ARSER
#'
#' @param cd A `CircadianData` object.
#' @param ... Additional parameters passed to `<method_function>`
#'
#' @returns A data frame with the results of the ARSER analysis.
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
#' cd <- add_experiment_info(cd, period = 24)
#' results <- clockworks:::analyze_arser(cd)
#' head(results)
analyze_arser <- function(cd, ...) {
  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_arser(cd)
  # Remove group column later if added temporarily by check
  added_group <- ifelse(is.na(cd_local$n_groups), TRUE, FALSE)

  # Create empty list for results
  ls_res_groups = list()

  # Run rhythmicity detection for each group separately
  groups <- unique(metadata(cd_local)[[".group"]])
  for (grp in groups) {
    # Prepare inputs
    ls_inputs <- prepare_arser(cd_local, grp, added_group)

    # Run rhythmicity analysis
    ls_res_grp <- execute_arser(ls_inputs)
    # ls_res_grp <- execute_arser(ls_inputs, ...)

    # Add to list
    ls_res_groups[[grp]] <- ls_res_grp
  }

  # Postprocessing
  ls_res <- format_arser(ls_res_groups, added_group)

  return(ls_res)
}
