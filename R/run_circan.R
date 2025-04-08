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
#' data(dataset)
#' data(meta)
#' cd <- CircadianData(dataset, meta, experimentInfo = list(period = 24, repeated_measures = TRUE))
#' results <- suppressWarnings(clockworks:::run_circan(cd))
#' head(results)
run_circan <- function(cd, ...) {
  # TODO: Add info about required parameters in experimentInfo slot (group_info,
  # repeated_measures, others?)

  # Prevent "object 'vec' not found" error if one curve type can't be fit
  # TODO: Figure out if this is necessary (was in the benchmark) or of there is
  # a better way to do this
  vec <<- rep(NA, times = 12)
  akaike <<- c(NA, NA)
  r <<- NA

  # Prepare data and metadata
  df = prep_circan(cd)
  df_data = df$df_data
  df_metadata = df$df_metadata

  # Run rhythmicity analysis
  df_results_original = suppressWarnings(CircaN::circan(
    data = df_data,
    meta = df_metadata,
    min_per = min(cd$period),
    max_per = max(cd$period),
    ...
  ))

  # Postprocessing
  df_results_modified <- postprocess_circan(df_results_original)


  # Remove global variables
  rm(vec)
  rm(akaike)
  rm(r)

  return(list(df_results_original = df_results_original, df_results_modified = df_results_modified))
}
