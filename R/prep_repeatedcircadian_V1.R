#' Prepare data for analysis with RepeatedCircadian
#'
#' Merge dataset and metadata of a `CircadianData` object.
#'
#' @param cd A `CircadianData` object.
#'
#' @returns A long format data frame
prep_repeatedcircadian_V1 <- function(cd) {
  # Get dataset
  df_data <- dataset(cd)

  # Get feature IDs, repeated for each sample
  feature_IDs <- rownames(df_data)[row(df_data)]

  # Get sample IDs, repeated for each feature
  sample_IDs <- colnames(df_data)[col(df_data)]

  # Get values as a vector (column by column)
  vals <- c(df_data)

  # Create long-format data frame for data
  data_long <- data.frame(
    "_feature" = feature_IDs,
    "_sample" = sample_IDs,
    "_value" = vals,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Add sample IDs as column to metadata
  df_meta <- metadata(cd)
  df_meta[["_sample"]] <- rownames(df_meta)

  # Merge
  df_long_merged <- merge(data_long, df_meta, by = "_sample")

  # Reorder
  col_order = c("_feature", "_sample", "_subject_ID", "_group", "_time", "_value")
  df_long_final <- df_long_merged[, col_order]

  return(df_long_final)
}
