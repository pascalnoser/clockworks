#' Detect rhythmicity with CircaN
#'
#' This function runs rhythmicity detection using CircaN.
#'
#' @param cd A `CircadianData` object.
#' @param method_args Additional parameters passed to `CircaN::circan()`
#'
#' @returns A data frame with the results of the CircaN analysis.
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
  groups <- unique(get_metadata(cd_local)[["group"]])
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
    w_params = get_wave_params(cd_local),
    added_group = added_group
  )

  return(ls_res)
}
