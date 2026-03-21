#' Detect rhythmicity with MetaCycle
#'
#' This function calls either `MetaCycle::meta2d()` or `MetaCycle::meta3d()` to
#' run rhythmicity detection, depending on whether the data includes repeated
#' measures or not.
#'
#' @param cd A `CircadianData` object.
#' @param method_args Additional parameters passed to `MetaCycle::meta3d()`
#'
#' @returns A list with the results of the MetaCycle analysis
analyze_metacycle <- function(cd, method_args = list()) {
  # Check if cd object contains repeated measures and call appropriate function
  if (experiment_info(cd)$repeated_measures) {
    cat("\nRunning MetaCycle meta3d analysis...\n")
    ls_res <- analyze_meta3d(cd, method_args)
  } else {
    cat("\nRunning MetaCycle meta2d analysis...\n")
    ls_res <- analyze_meta2d(cd, method_args)
  }

  return(ls_res)
}
