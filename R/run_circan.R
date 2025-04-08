#' Detect rhythmicity with CircaN
#'
#' This function runs rhythmicity detection using CircaN.
#'
#' @param cd A `CircadianData` object.
#' @param ... Additional parameters passed to `CircaN::circan()`
#'
#' @importFrom CircaN circan
#'
#' @returns A data frame with the results of the CircaN analysis.
run_circan <- function(cd, ...) {
  # TODO: Add info about required parameters in experimentInfo slot (group_info,
  # repeated_measures, others?)
  # TODO: Handle the "..."

  # Prevent "object 'vec' not found" error if one curve type can't be fit
  # TODO: Figure out if this is necessary (was in the benchmark) or of there is
  # a better way to do this
  vec = rep(NA, times = 12)
  akaike = c(NA, NA)
  r = NA

  # Prepare meta data
  df_metadata = data.frame(
    time = metadata(cd)$Time,
    sample = metadata(cd)$Sample_ID
  )
  if (cd$repeated_measures == TRUE) df_metadata$ind = metadata(cd)$Subject_ID

  # Prepare data (must be a data frame with features as first column)
  df_data = data.frame(feature = rownames(dataset(cd)), dataset(cd))

  # Run rhythmicity analysis
  df_results = CircaN::circan(
    data = df_data,
    meta = df_metadata,
    mode = "default", # Figure out how this could be controlled by user using "..."
    min_per = min(cd$per),
    max_per = max(cd$per)
  )

}
