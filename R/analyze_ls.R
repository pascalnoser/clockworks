#' Detect rhythmicity with Lomb-Scargle
#'
#' This function runs rhythmicity detection Lomb-Scargle
#'
#' @param cd A `CircadianData` object.
#' @param ... Additional parameters passed to `<method_function>`
#'
#' @returns A data frame with the results of the Lomb-Scargle analysis.
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
#' results <- clockworks:::analyze_ls(cd)
#' head(results)
analyze_ls <- function(cd, ...) {
  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_ls(cd)
  # Remove group column later if added temporarily by check
  added_group <- ifelse(is.na(cd_local$n_groups), TRUE, FALSE)

  # Create empty list for results
  ls_res_groups = list()

  # Run rhythmicity detection for each group separately
  groups <- unique(metadata(cd_local)[[".group"]])
  for (grp in groups) {
    # Prepare inputs
    ls_inputs <- prepare_ls(cd_local, grp)

    # Run rhythmicity analysis
    ls_res_grp <- execute_ls(ls_inputs, ...)

    # Add to list
    ls_res_groups[[grp]] <- ls_res_grp
  }

  # Postprocessing
  ls_res <- format_ls(ls_res_groups, added_group)

  return(ls_res)
}
