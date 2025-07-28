#' Check and Prepare a Dataset for Analysis
#'
#' This function performs a series of checks on a dataset to ensure it is suitable
#' for downstream analysis. It verifies data types, handles missing row names,
#' and confirms the presence of sample IDs in the provided metadata.
#'
#' @param dataset A data frame or matrix representing the dataset to be checked.
#'   It is expected to contain numeric data.
#' @param metadata A data frame containing metadata associated with the samples
#'   in the dataset.
#'
#' @return A matrix representation of the input dataset, after performing the checks
#'   and conversions. The matrix will have row names representing feature IDs.
#'
#' @details
#' The function performs the following checks and operations:
#' \enumerate{
#'   \item Checks if `dataset` is a data frame or matrix.
#'   \item Checks if `dataset` contains only numeric data. If it's a data frame,
#'     each column is checked.
#'   \item Converts the `dataset` to a matrix to save memory.
#'   \item Checks if the row names of `dataset` are just row numbers. If so, a
#'     warning is issued and row names are assigned. This helps ensure
#'     that each row represents a feature.
#'   \item Checks if the column names of `dataset` (representing sample IDs) are
#'     present in the row names of the `metadata` data frame and vice versa.
#'   \item Orders the columns of `dataset` to match the row names of `metadata`.
#' }
#'
#' @examples
#' # Create dummy data and metadata
#' set.seed(123)
#' data <- matrix(rnorm(100), nrow = 10)
#' colnames(data) <- paste0("sample", 1:10)
#' metadata <- data.frame(sample_id = colnames(data), group = rep(c("A", "B"), each = 5))
#' rownames(metadata) <- metadata$sample_id
#'
#' # Check the dataset
#' checked_data <- check_dataset(dataset = data, metadata = metadata)
#'
#' # Example with feature IDs as row names
#' rownames(data) <- paste0("gene", 1:10)
#' checked_data_with_ids <- check_dataset(dataset = data, metadata = metadata)
#'
#' # Example with non-numeric data
#' data_non_numeric <- data.frame(col1 = 1:10, col2 = letters[1:10])
#' metadata <- data.frame(sample_id = letters[1:10], group = rep(c("A", "B"), each = 5))
#' rownames(metadata) <- metadata$sample_id
#' result <- try(check_dataset(dataset = data_non_numeric, metadata = metadata))
#' if(inherits(result, "try-error")) {
#'   message("Error caught (as expected).")
#' }
#'
#' # Example with missing sample IDs in metadata
#' metadata_missing <- data.frame(sample_id = paste0("sample", 1:5), group = rep("A", 5))
#' rownames(metadata) <- metadata$sample_id
#' result <- try(check_dataset(dataset = data, metadata = metadata_missing))
#' if(inherits(result, "try-error")) {
#'   message("Error caught (as expected).")
#' }
#'
#' @export
check_dataset <- function(dataset, metadata) {
  # 1. Check if dataset is a data.frame or matrix
  if (!inherits(dataset, c("data.frame", "matrix"))) {
    stop("`dataset` must be a data frame or matrix.", call. = FALSE)
  }

  # 2. Check if dataset is numeric. If it is a data frame, check every column
  if (is.matrix(dataset) && !is.numeric(dataset)) {
    stop(paste0(
      "The `dataset` matrix is of type ", typeof(dataset),
      " but is expected to be numerical"
    ), call. = FALSE)
  } else {
    non_numeric_cols <- which(sapply(dataset, function(col) !is.numeric(col)))
    if (length(non_numeric_cols) > 0) {
      cols_types <- sapply(dataset, function(col) class(col))[non_numeric_cols]
      str_combined <- paste0(names(cols_types), " (", cols_types, ")")
      stop(paste(
        "All columns of `dataset` are expected to be of type numeric.",
        "The following columns have a different data type:\n",
        paste(str_combined, collapse = "\n ")
      ), call. = FALSE)
    }
  }

  # 3. Convert to matrix to save memory
  dataset <- as.matrix(dataset)

  # 4. Check if row names of dataset are just row numbers
  if (identical(rownames(dataset), as.character(1:nrow(dataset))) || is.null(rownames(dataset))) {
    # TODO: Throw an error here instead? I can't think of any scenario where the
    # user would not want to use feature IDs.
    warning(
      "The dataset does not seem to contain any feature IDs such as e.g. ",
      "gene names. Using row numbers as feature IDs. To avoid this, add ",
      "feautre IDs to `dataset` as row names.",
      call. = FALSE
    )
    # Ensure row names are not NULL
    rownames(dataset) <- as.character(1:nrow(dataset))
  }

  # 5a. Check if the column names of dataset are present in the row names of
  # metadata
  missing_samples <- setdiff(colnames(dataset), rownames(metadata))
  if (length(missing_samples) > 0) {
    stop(paste(
      "The following sample IDs in the dataset are not found in the metadata:",
      paste(missing_samples, collapse = ", ")
    ),
    call. = FALSE)
  }

  # 5b. Check if the row names of metadata are present in the column names of
  # metadata
  missing_samples <- setdiff(rownames(metadata), colnames(dataset))
  if (length(missing_samples) > 0) {
    stop(paste(
      "The following sample IDs in the metadata are not found in the dataset:",
      paste(missing_samples, collapse = ", ")
    ),
    call. = FALSE)
  }


  # 5. Check if order of samples in the dataset and metadata are the same and
  # adjust the dataset if not
  if (!identical(colnames(dataset), rownames(metadata))) {
    dataset <- dataset[, match(rownames(metadata), colnames(dataset))]
    # warning(
    #   "The order of samples in the dataset has been adjusted to match the ",
    #   "order of samples in the metadata.",
    #   call. = FALSE
    # )
  }


  return(dataset)
}
