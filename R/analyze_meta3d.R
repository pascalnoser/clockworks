#' Detect rhythmicity with meta3d
#'
#' This function runs rhythmicity detection with meta3d
#'
#' @param cd A `CircadianData` object.
#' @param method_args Additional parameters passed to `MetaCycle::meta3d()`
#'
#' @returns A data frame with the results of the meta3d analysis.
analyze_meta3d <- function(cd, method_args = list()) {
  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_meta3d(cd)

  # Prepare inputs
  inputs <- prepare_meta3d(
    cd_local,
    cycMethodOne = method_args$cycMethodOne %||% "JTK"
  )

  # Run rhythmicity analysis
  ls_res <- execute_meta3d(inputs, method_args)

  # Postprocessing
  ls_res <- format_meta3d(
    ls_res = ls_res,
    w_params = get_wave_params(cd_local),
    meta3d_method = inputs$cycMethodOne
  )

  return(ls_res)
}
