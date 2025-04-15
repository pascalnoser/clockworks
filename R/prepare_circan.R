#' Title
#'
#' @param cd
#'
#' @returns something
prepare_circan <- function(cd) {
  # Prepare meta data
  df_metadata <- data.frame(
    sample = rownames(metadata(cd)),
    time = metadata(cd)$Time
  )
  if (cd$repeated_measures == TRUE) {
    df_metadata$ind <- metadata(cd)$Subject_ID
  } else {
    df_metadata$ind <- as.factor(1)
  }

  # Prepare data (must be a data frame with features as first column)
  df_data <- data.frame(feature = rownames(dataset(cd)), dataset(cd))

  return(list(df_data = df_data, df_metadata = df_metadata))
}
