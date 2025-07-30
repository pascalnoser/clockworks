#' Detect rhythmicity with CircaN
#'
#' This function runs rhythmicity detection using CircaN.
#'
#' @param cd A `CircadianData` object.
#' @param method_args Additional parameters passed to `CircaN::circan()`
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
#' cd <- CircadianData(cw_data, cw_metadata)
#' cd <- clockworks:::add_experiment_info(cd, period = 24, data_type = "norm")
#' results <- clockworks:::analyze_circan(cd)
#' head(results$res_original)
analyze_circan <- function(cd, method_args = list()) {
  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_circan(cd)

  # Remove group column later if added temporarily by check
  added_group <- ifelse(is.na(cd_local$n_groups), TRUE, FALSE)

  # Create empty list for results
  ls_res_groups = list()

  # Run rhythmicity detection for each group separately
  # TODO: Use long format prepare function from repeatedcircadian and then use
  # `split()` to split by group and feauture to parallelise
  groups <- unique(metadata(cd_local)[["group"]])
  for (grp in groups) {
    # Prepare inputs
    inputs <- prepare_circan(cd_local, grp)

    # Run rhythmicity analysis
    df_res_grp <- execute_circan(inputs, grp, method_args)

    # Add to list
    ls_res_groups[[grp]] <- df_res_grp
  }

  # Remove global variables
  # TODO: Figure out how to do this properly (see TODO in `run_circan()`)
  rm(vec)
  rm(akaike)
  rm(r)

  # Postprocessing
  ls_res <- format_circan(
    ls_res_groups = ls_res_groups,
    w_params = wave_params(cd_local),
    added_group = added_group
  )

  return(ls_res)
}
