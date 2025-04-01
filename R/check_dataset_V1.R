# Note: This old version performed more checks because it allowed for more
# flexibility. We made the decision to simplify this to minimise the risk of
# bugs or confusion for the user.
check_dataset_V1 <- function(dataset,
                          metadata,
                          colname_sample,
                          colname_features = NULL) {
  # 1. Check if dataset is a data.frame or matrix
  if (!inherits(dataset, c("data.frame", "matrix"))) {
    stop("`dataset` must be a data frame or matrix.", call. = FALSE)
  }

  # 2. Check if colname_features is NULL and the row names of dataset are just
  # numbers from 1 to nrow(dataset)
  if (is.null(colname_features)) {
    if (identical(rownames(dataset), as.character(1:nrow(dataset)))) {
      warning("The dataset does not seem to contain any feature IDs. Using row names as feature IDs (e.g. gene names). To avoid this, add feature IDs to `dataset` as row names or alternatively as a column that then needs to be specified using `colname_features`.", call. = FALSE)
      rownames(dataset) <- as.character(1:nrow(dataset))  # Ensures feature IDs are the row names
    }
  } else {
    # 3. If colname_features is defined, turn row names of dataset to feature IDs and remove the column
    if (colname_features %in% colnames(dataset)) {
      # Make sure there are no duplicated feature IDs
      features = dataset[[colname_features]]
      dup_features = features[duplicated(features)]
      if (length(dup_features > 0)) {
        stop(paste0("The following features in the '", colname_features, "' column of `dataset` are duplicated: ", paste(dup_features, collapse = ", ")), call. = FALSE)
      }
      rownames(dataset) <- dataset[[colname_features]]
      dataset[[colname_features]] <- NULL
    } else {
      stop(paste0("The column '", colname_features, "' does not exist in `dataset`"), call. = FALSE)
    }
  }

  # 4. Check if the column names of dataset are present in the colname_sample column of metadata
  # if (!colname_sample %in% colnames(metadata)) {
  #   stop(paste0("The column '", colname_sample, "' does not exist in `metadata.`"), call. = FALSE)
  # }

  # Ensure column names in dataset are in the colname_sample of metadata
  missing_samples <- setdiff(colnames(dataset), metadata[[colname_sample]])
  if (length(missing_samples) > 0) {
    stop(paste("The following sample IDs in dataset are not found in the metadata:", paste(missing_samples, collapse = ", ")), call. = FALSE)
  }

  # 5. Check if columns in dataset are numeric, if not, try to convert
  # Throw error if there are columns that aren't numeric or character or integer
  non_numeric_columns <- which(!sapply(dataset, function(x) is.numeric(x) || is.character(x)))
  if (length(non_numeric_columns) > 0) {
    stop(paste("The following columns in `dataset` are neither numeric nor convertible:\n",
               paste(colnames(dataset)[non_numeric_columns], collapse = ", ")), call. = FALSE)
  }

  # Try converting all character columns to numeric
  columns_not_converted <- c()  # To store columns that couldn't be converted
  columns_converted <- c()      # To store columns that were successfully converted
  for (i in seq_along(dataset)) {
    column <- dataset[[i]]

    if (is.character(column)) {
      # Replace "NaN" and "NA" strings with actual NA values
      column[column == "NaN" | column == "NA"] <- NA
      # Attempt to convert to numeric
      converted_column <- suppressWarnings(as.numeric(column))

      # If conversion introduces NA values (that weren't already NA), mark it
      if (any(is.na(converted_column) & !is.na(column))) {
        columns_not_converted <- c(columns_not_converted, colnames(dataset)[i])
      } else {
        dataset[[i]] <- converted_column
        columns_converted <- c(columns_converted, colnames(dataset)[i])
      }
    }
  }

  # If any columns couldn't be converted, throw an error listing them
  if (length(columns_not_converted) > 0) {
    stop(paste("The following columns could not be converted to numeric:\n",
               paste(columns_not_converted, collapse = ", ")), call. = FALSE)
  }

  # If there were successful conversions, give a warning
  if (length(columns_converted) > 0) {
    warning(paste("The following character columns were converted to numeric:\n",
                  paste(columns_converted, collapse = ", ")), call. = FALSE)
  }

  # 6. Ensure dataset is a matrix to save memory
  # TODO: Move to beginning
  dataset <- as.matrix(dataset)

  return(dataset)
}
