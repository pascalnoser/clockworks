#' Prepare data for analysis with RepeatedCircadian
#'
#' Merge dataset and metadata of a `CircadianData` object.
#'
#' @param cd A `CircadianData` object.
#'
#' @returns A data frame.
prep_repeatedcircadian_long <- function(cd) {
  # Get dataset
  df_data <- dataset(cd)

  # Get feature- and sample IDs
  feature_IDs <- rownames(df_data)
  sample_IDs <- colnames(df_data)

  # Convert into long format
  data_long <- data.frame(
    ".feature" = rep(feature_IDs, times = length(sample_IDs)),
    ".sample" = rep(sample_IDs, each = length(feature_IDs)),
    ".value" = as.vector(as.matrix(df_data)),
    check.names = FALSE  # Don't add "X" before leading _
  )

  # Add sample IDs as column to metadata
  df_meta <- metadata(cd_sorted)
  df_meta[[".sample"]] <- rownames(df_meta)

  # Only keep relevant columns of meta data
  if (!is.na(cd$n_groups)) {
    relevant_cols <- c(".sample", ".subject_ID", ".time", ".group")
    id_cols <- c(".feature", ".subject_ID", ".group")
  } else {
    relevant_cols <- c(".sample", ".subject_ID", ".time")
    id_cols <- c(".feature", ".subject_ID")
  }
  df_meta = df_meta[, relevant_cols]

  # Merge
  df_merged <- merge(data_long, df_meta, by = ".sample")

  # Remove the now redundant sample ID column to prevent warning when reshaping
  df_merged[[".sample"]] <- NULL

  # Reshape to wider format
  df_reshaped <- reshape(
    df_merged,
    idvar = id_cols,
    timevar = ".time",
    v.names = ".value",
    direction = "wide"
  )

  # Clean up column names
  colnames(df_reshaped) <- gsub(".value\\.", "T", colnames(df_reshaped))


  return(df_reshaped)
}
