#' Detect rhythmicity with CircaN
#'
#' This function runs rhythmicity detection using CircaN.
#'
#' @param cd A `CircadianData` object.
#' @param ... Additional parameters passed to `CircaN::circan()`
#'
#' @import CircaN
#' @import nlme
#'
#' @returns A data frame with the results of the CircaN analysis.
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
#' cd <- CircadianData(
#'   cw_data,
#'   cw_metadata,
#'   experiment_info = list(
#'     period = 24,
#'     repeated_measures = TRUE,
#'     n_groups = 2
#'   )
#' )
#' results <- clockworks:::analyze_circan(cd)
#' head(results$res_original)
analyze_circan <- function(cd, ...) {
  # TODO: REMOVE IMPORTS AND MOVE TO RUN FUNCTION

  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_circan(cd)
  # Remove group column later if added temporarily by check
  remove_group <- ifelse(is.na(cd_local$n_groups), TRUE, FALSE)

  # Create empty list for results
  ls_res_groups = list()

  # Run rhythmicity detection for each group separately
  groups <- unique(metadata(cd_local)[[".group"]])
  for (grp in groups) {
    # Prepare inputs
    ls_inputs <- prepare_circan(cd_local, grp)

    # Run rhythmicity analysis
    # TODO
    df_res_grp <- run_circan(ls_inputs, ...)

    # Add to list
    ls_res_groups[[grp]] <- df_res_grp
  }

  # Postprocessing
  # TODO
  ls_res <- format_circan(ls_res_groups, remove_group)

  return(ls_res)
}
