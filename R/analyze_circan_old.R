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
analyze_circan_old <- function(cd, ...) {
  # TODO: Add info about required parameters in experiment_info slot (group_info,
  # repeated_measures, others?)

  # TODO: ADD WAY TO HANDLE DIFFERENT GROUPS

  # Prevent "object 'vec' not found" error if one curve type can't be fit
  # TODO: Figure out if this is necessary (was in the benchmark) or of there is
  # a better way to do this
  vec <<- rep(NA, times = 12)
  akaike <<- c(NA, NA)
  r <<- NA

  # Prepare data and metadata
  ls_prep = prepare_circan(cd)
  df_data = ls_prep$df_data
  df_metadata = ls_prep$df_metadata

  # Run rhythmicity analysis
  df_results_original = suppressWarnings(CircaN::circan(
    data = df_data,
    meta = df_metadata,
    min_per = min(cd$period),
    max_per = max(cd$period),
    ...
  ))

  # Postprocessing
  df_results_modified <- format_circan(df_results_original)


  # Remove global variables
  rm(vec)
  rm(akaike)
  rm(r)

  return(list(res_original = df_results_original, res_formatted = df_results_modified))
}
