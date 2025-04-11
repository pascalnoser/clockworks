#' Prepare data for analysis with RepeatedCircadian
#'
#' Merge dataset and metadata of a `CircadianData` object.
#'
#' @param cd A `CircadianData` object.
#'
#' @returns A long format data frame
prep_repeatedcircadian <- function(cd) {
  # Make sure samples are ordered by time and subject


  # Get dataset
  df_data <- dataset(cd)

  # Get feature- and sample IDs
  feature_IDs <- rownames(df_data)
  sample_IDs <- colnames(df_data)

  # Convert into long format
  data_long <- data.frame(
    "_feature" = rep(feature_IDs, times = length(sample_IDs)),
    "_sample" = rep(sample_IDs, each = length(feature_IDs)),
    "_value" = as.vector(as.matrix(df_data)),
    check.names = FALSE  # Don't add "X" before leading _
  )

  # Add sample IDs as column to metadata
  df_meta <- metadata(cd)
  df_meta[["_sample"]] <- rownames(df_meta)

  # Only keep relevant columns of meta data
  df_meta = df_meta[, c("_sample", "_subject_ID", "_group", "_time")]

  # Merge
  df_merged <- merge(data_long, df_meta, by = "_sample")

  # Remove the now redundant sample ID column to prevent warning when reshaping
  df_merged[["_sample"]] <- NULL

  # Reshape to wider format
  df_wide <- reshape(
    df_merged,
    idvar = c("_feature", "_subject_ID", "_group"),
    timevar = "_time",
    v.names = "_value",
    direction = "wide"
  )

  # Clean up column names
  colnames(df_wide) <- gsub("_value\\.", "T", colnames(df_wide))


  return(df_long_final)
}
