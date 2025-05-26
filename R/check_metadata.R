#' Check Metadata for Analysis
#'
#' This function performs a series of checks on a metadata data frame to ensure
#' it is suitable for downstream analysis. It verifies the presence and type of
#' required columns, checks for uniqueness of sample IDs issues warnings
#' about ignored columns, and renames columns to ensure consistency.
#'
#' @param metadata A data frame containing metadata associated with the samples
#'   in the dataset.
#' @param colname_time A string specifying the name of the column in `metadata`
#'   that contains the time information (e.g., collection time). This column
#'   must be numeric.
#' @param colname_sample A string specifying the name of the column in
#'   `metadata` that contains the sample IDs. These IDs should be unique.
#' @param colname_group A string (optional) specifying the name of the column in
#'   `metadata` that contains group information (e.g., treatment groups,
#'   conditions). If no group information is available, leave as `NULL`
#'   (default).
#' @param colname_subject A string (optional) specifying the name of the column
#'   in `metadata` that contains subject IDs. This is useful for repeated
#'   measures designs where each subject has multiple samples taken over time.
#'   If no subject information is available, leave as `NULL` (default).
#'
#' @return A data frame with the metadata, after performing the checks and
#'   column ordering. The columns will be ordered with `colname_sample` first,
#'   followed by `colname_time`, `colname_subject`, `colname_group`, and any
#'   additional columns.
#'
#' @details
#' The function performs the following checks and operations:
#' \enumerate{
#'   \item Checks if `metadata` is a data frame.
#'   \item Checks if the required columns specified by `colname_time` and
#'     `colname_sample` are present in the `metadata` data frame.
#'   \item Checks if the optional columns specified by `colname_group` and
#'     `colname_subject` are present (if they are not `NULL`) and contain
#'     reasonable values.
#'   \item Checks if the `colname_time` column contains numeric values.
#'   \item Checks if the values in the `colname_sample` column are unique.
#'   \item Issues a warning if there are additional columns in `metadata` that
#'     are not explicitly used by the function, informing the user that these
#'     columns will be ignored.
#'   \item Converts the metadata to a standard `data.frame` (e.g. not a tibble)
#'   \item Adds the sample IDs as row names.
#'   \item Adds columns with pre-defined names and order to make downstream
#'     analysis easier.
#' }
#'
#'
#' @examples
#' # Create dummy metadata
#' metadata <- data.frame(
#'   sample_id = paste0("sample", 1:10),
#'   time = 1:10,
#'   group = rep(c("A", "B"), each = 5),
#'   subject = rep(1:2, 5),
#'   extra_col = rnorm(10)
#' )
#'
#' # Check the metadata
#' checked_metadata <- check_metadata(
#'   metadata = metadata,
#'   colname_time = "time",
#'   colname_sample = "sample_id",
#'   colname_group = "group",
#'   colname_subject = "subject"
#' )
#'
#' # Example with no group or subject information
#' checked_metadata_no_group <- check_metadata(
#'   metadata = metadata,
#'   colname_time = "time",
#'   colname_sample = "sample_id"
#' )
#'
#' # Example with missing columns
#' result <- try(check_metadata(
#'   metadata = metadata[, c("time", "group", "subject")],
#'   colname_time = "time",
#'   colname_sample = "sample_id" # This will cause an error
#' ))
#' if (inherits(result, "try-error")) {
#'   message("Error caught (as expected).")
#' }
#'
#' # Example with non-numeric time
#' metadata_non_numeric_time <- metadata
#' metadata_non_numeric_time$time <- letters[1:10]
#' result <- try(check_metadata(
#'   metadata = metadata_non_numeric_time,
#'   colname_time = "time",
#'   colname_sample = "sample_id"
#' ))
#' if (inherits(result, "try-error")) {
#'   message("Error caught (as expected).")
#' }
#'
#' # Example with duplicated sample IDs
#' metadata_duplicated_ids <- metadata
#' metadata_duplicated_ids$sample_id[1] <- metadata_duplicated_ids$sample_id[2]
#' result <- try(check_metadata(
#'   metadata = metadata_duplicated_ids,
#'   colname_time = "time",
#'   colname_sample = "sample_id"
#' ))
#' if (inherits(result, "try-error")) {
#'   message("Error caught (as expected).")
#' }
#'
#' @export
check_metadata <- function(metadata,
                           colname_sample,
                           colname_time,
                           colname_group = NULL,
                           colname_subject = NULL) {
  # 1. Check if metadata is a data.frame
  if (!inherits(metadata, "data.frame")) {
    stop("'metadata' must be a data.frame.", call. = FALSE)
  }

  # Get column names
  cnames <- colnames(metadata)

  # 2. Check if required columns (Sample ID and Time) are present
  # TODO: Add capability of using rownames as sample ID, maybe if the user
  # specified colname_sample = "row.names" or something like that
  if (!colname_sample %in% cnames) {
    stop(
      paste0(
        "Missing column '", colname_sample, "' in `metadata`. ",
        "Please make sure that `colname_sample` corresponds to the ",
        "column containing the sample IDs."
      ),
      call. = FALSE
    )
  }

  if (!colname_time %in% cnames) {
    stop(
      paste0(
        "Missing column '", colname_time, "' in `metadata`. ",
        "Please make sure that `colname_time` corresponds to the ",
        "column containing the time information."
      ),
      call. = FALSE
    )
  }

  # 3a. Check if optional column for group is present (if not NULL)
  if (!is.null(colname_group) && !colname_group %in% cnames) {
    stop(
      paste0(
        "Missing column '", colname_group, "' in `metadata`. ",
        "If applicable, please make sure that `colname_group` corresponds to the ",
        "column containing the group information or leave it as NULL otherwise."
      ),
      call. = FALSE
    )
  }

  # Print a message/warning if there is only one group in the defined column
  if (!is.null(colname_group)) unique_groups <- unique(metadata[[colname_group]])

  if (!is.null(colname_group) && !length(unique_groups) > 1) {
    message(
      paste0(
        "\nThe `colname_group` ('", colname_group, "') column of metadata ",
        "contains only one group ('", unique_groups, "'). If your data does ",
        "not consist of several groups, `colname_group` can be set to NULL ",
        "(default). Analysis will proceed assuming one group."
      )
    )
  }

  # 3b. Check if optional column for subject ID is present (if not NULL)
  if (!is.null(colname_subject) && !colname_subject %in% cnames) {
    stop(
      paste0(
        "Missing column '", colname_subject, "' in `metadata`. ",
        "If your data contains repeated measures, please make sure that ",
        "`colname_subject` corresponds to the column containing the subject IDs ",
        "or set it to NULL (default) otherwise."
      ),
      call. = FALSE
    )
  }

  # Throw an error if subject ID column contains only one value because this
  # suggests a misunderstanding by the user
  if (!is.null(colname_subject)) unique_subjects <- unique(metadata[[colname_subject]])

  if (!is.null(colname_subject) && !length(unique_subjects) > 1) {
    stop(
      paste0(
        "The `colname_subject` column of metadata ('", colname_subject, "')",
        "contains only one unique value ('", unique_groups, "'). Set ",
        "`colname_subject` to NULL (default) unless you have repeated measures."
      ),
      call. = FALSE
    )
  }

  # Print a message if all values are unique because this suggests the user
  # simply has replicates rather than repeated measures, in which case
  # `colname_sample` need not be defined.
  if (!is.null(colname_subject) && !any(duplicated(metadata[[colname_subject]]))) {
    message(
      paste0(
        "\nAll values in the `colname_subject` column of metadata ('",
        colname_subject, "') are unique. Unless you have repeated measures, ",
        "`colname_subject` can be set to to NULL (default). Analysis will ",
        "proceed assuming no repeated measures."
      )
    )

    # Set colname_subject to NULL since there are no repeated measures
    colname_subject <- NULL
  }


  # 4. Check if colname_time contains numeric values
  if (!is.numeric(metadata[[colname_time]])) {
    stop(
      paste0(
        "Column '", colname_time, "' of `metadata` is of type ", class(metadata[[colname_time]]),
        ". The column defined as `colname_time` in `metadata` must be of type numeric."
      ),
      call. = FALSE
    )
  }

  # 5. Check if the values in colname_sample are unique
  if (any(duplicated(metadata[[colname_sample]]))) {
    stop(
      paste0("Values in metadata column '", colname_sample, "' must be unique."),
      call. = FALSE
    )
  }

  # 6. If there are additional columns, warn the user that they will be ignored
  additional_cols <- setdiff(
    cnames,
    c(colname_time, colname_sample, colname_group, colname_subject)
  )
  if (length(additional_cols) > 0) {
    message(
      paste0(
        "\nThe following columns in `metadata` will be ignored: ",
        paste(additional_cols, collapse = ", "),
        "\nIf you want to use subject information (for repeated measures) or group ",
        "information, make sure to define these as `colname_subject` and ",
        "`colname_group`, respectively."
      )
    )
  }

  # 7. Make sure metadata is a data frame and not e.g. a tibble
  metadata <- as.data.frame(metadata)

  # 8. Add sample IDs as row names
  row.names(metadata) <- as.character(metadata[[colname_sample]])

  # 9. Add columns with pre-defined names and order, and remove other columns
  # Start with the required time column
  rename_map <- c("time" = colname_time)

  # Conditionally add optional columns
  if (!is.null(colname_group)) {
    rename_map["group"] <- colname_group
  }
  if (!is.null(colname_subject)) {
    rename_map["subject_ID"] <- colname_subject
  }

  # Only use relevant columns. Set `drop = FALSE` to make sure it stays a data
  # frame even if there is only one column
  metadata_final <- metadata[, rename_map, drop = FALSE]
  colnames(metadata_final) <- names(rename_map)

  return(metadata_final)
}
