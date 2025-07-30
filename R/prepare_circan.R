#' CircaN prep
#'
#' This function is used to create the input required to run rhythmicity
#' detection with CircaN.
#'
#' @param cd A `CircadianData` object
#' @param grp A string specifying a value in the "group" column of the metadata
#'   slot of `cd` which is used for filtering.
#'
#' @returns A list with inputs for `execute_circan()`
prepare_circan <- function(cd, grp) {
  # Filter CD object by group
  cd_filt <- filter_samples(cd, col = "group", value = grp)

  # Prepare data (must be a data frame with features as first column)
  df_data <- data.frame(feature = rownames(dataset(cd_filt)), dataset(cd_filt), check.names = FALSE)

  # Create list with inputs
  inputs <- list(
    data = df_data,
    meta = metadata(cd_filt),
    mode = "default",
    init_value = mean(cd$period),
    min_per = min(cd$period),
    max_per = max(cd$period)
  )

  return(inputs)
}
