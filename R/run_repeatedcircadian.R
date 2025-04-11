#' Detect rhythmicity with RepeatedCircadian
#'
#' This function runs rhythmicity detection using RepeatedCircadian
#'
#' @param cd A `CircadianData` object.
#' @param ... Additional parameters passed to `RepeatedCircadian::rpt_rhythmicity()`
#'
#' @import RepeatedCircadian
#' @importFrom parallel mclapply
#'
#' @returns A data frame with the results of the RepeatedCircadian analysis.
#' @examples
#' data(dataset)
#' data(meta)
#' cd <- CircadianData(dataset,
#'                     meta,
#'                     experimentInfo = list(
#'                       period = 24,
#'                       repeated_measures = TRUE,
#'                       group_info = TRUE
#'                     ))
#' results <- clockworks:::run_repeatedcircadian(cd)
#' head(results)
run_repeatedcircadian <- function(cd, ...) {
  # Get metadata
  df_metadata = metadata(cd)

  # Temporarily add all samples to same group if data contains no groups
  # TODO: probably not necessary?
  if (cd$group_info == FALSE) {
    df_metadata[["_group"]] <- "tmp"
  }

  # Prepare data
  df_prep <- prep_repeatedcircadian(cd)




  ##########

  # Get groups
  groups <- unique(df_metadata[["_group"]])

  # Loop over groups
  for (grp in groups) {
    # ...
  }

  # Remove 'group' column if only added temporarily at the start
  if (cd$group_info == FALSE) {
    df_results_original$group = NULL
  }

  # Postprocessing
  df_results_modified <- postprocess_repeatedcircadian(df_results_original)


  return(list(df_results_original = df_results_original, df_results_modified = df_results_modified))
}
