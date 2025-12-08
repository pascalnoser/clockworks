#' Prepare data for analysis with diffCircadian
#'
#' Merge dataset and metadata of a `CircadianData` object.
#'
#' @param cd A `CircadianData` object.
#'
#' @returns A data frame.
prepare_diffcircadian <- function(cd) {
  # Get feature- and sample IDs
  feature_IDs <- rownames(cd)
  sample_IDs <- colnames(cd)

  # Convert into long format
  data_long <- data.frame(
    "feature" = rep(feature_IDs, times = length(sample_IDs)),
    "sample" = rep(sample_IDs, each = length(feature_IDs)),
    "value" = as.vector(as.matrix(dataset(cd)))
  )

  # Add sample IDs as column to metadata
  df_meta <- metadata(cd)
  df_meta[["sample"]] <- rownames(df_meta)
  df_meta[["time_replicate"]] <- paste0("T", df_meta$time, "_R", df_meta$replicate)

  # Only keep relevant columns of meta data
  relevant_cols <- c("sample", "time_replicate", "group")
  id_cols <- c("feature", "group")

  df_meta = df_meta[, relevant_cols]

  # Merge
  df_merged <- merge(data_long, df_meta, by = "sample")

  # Remove the now redundant sample ID column to prevent warning when reshaping
  df_merged[["sample"]] <- NULL

  # Reshape to wider format
  df_reshaped <- reshape(
    df_merged,
    idvar = id_cols,
    timevar = "time_replicate",
    v.names = "value",
    direction = "wide"
  )

  # Clean up column names
  colnames(df_reshaped) <- gsub("value\\.", "", colnames(df_reshaped))

  # Create named list to return
  inputs <- list(
    dat = df_reshaped,
    period = mean(cd$period),
    method = "LR",
    FN = TRUE
  )

  return(inputs)
}
