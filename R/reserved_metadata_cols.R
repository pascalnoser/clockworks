#' Metadata column names used internally by clockworks
#'
#' Columns with these names are created, overwritten or read by clockworks
#' functions. Additional (unmapped) columns in the `metadata` given to
#' `CircadianData()` must therefore not use any of these names.
#'
#' * `time`, `group`, `subject_ID`: standard columns created from
#'   `colname_time`, `colname_group` and `colname_subject` (a temporary
#'   `group = "tmp"` column is added by many methods if there are no groups).
#' * `norm_factors`: edgeR normalisation factors of count data
#'   (`preprocess_counts()`, `convert_to_cpm()`).
#' * `.internal_sample_id`: sample IDs if `colname_sample = "rownames"`.
#' * `replicate`, `time_replicate`: replicate numbers for diffCircadian
#'   (`check_diffcircadian()`, `prepare_diffcircadian()`).
#' * `time_sin`, `time_cos`: harmonic terms for LimoRhyde
#'   (`check_limorhyde()`).
#' * `SampleID`: sample IDs in the meta3d design file (`prepare_meta3d()`).
#'
#' @returns A character vector of reserved column names.
#' @noRd
reserved_metadata_cols <- function() {
  c(
    "time",
    "group",
    "subject_ID",
    "norm_factors",
    ".internal_sample_id",
    "replicate",
    "time_replicate",
    "time_sin",
    "time_cos",
    "SampleID"
  )
}

#' Check additional metadata columns for clashes with internal column names
#'
#' @param cnames Column names of the user's metadata.
#' @param mapped_cols Column names passed as `colname_*` arguments (these are
#'   renamed to the standard names and are therefore allowed).
#'
#' @returns The names of the additional columns (invisibly). Throws an error if
#'   any of them clashes with a reserved name.
#' @noRd
check_extra_metadata_cols <- function(cnames, mapped_cols) {
  extra_cols <- setdiff(cnames, mapped_cols)
  clashes <- intersect(extra_cols, reserved_metadata_cols())
  if (length(clashes) == 0) {
    return(invisible(extra_cols))
  }

  standard_args <- c(
    time = "colname_time",
    group = "colname_group",
    subject_ID = "colname_subject"
  )
  details <- vapply(
    clashes,
    function(col) {
      if (col %in% names(standard_args)) {
        paste0(
          "'", col, "' (a standard column; if it contains the ",
          if (col == "subject_ID") "subject IDs" else paste0(col, " information"),
          ", pass it as `", standard_args[[col]], " = \"", col, "\"`)"
        )
      } else {
        paste0("'", col, "' (used internally by clockworks)")
      }
    },
    character(1)
  )

  stop(
    "The following columns in `metadata` have names that clockworks uses ",
    "internally: ",
    paste(details, collapse = ", "),
    ". Please rename ",
    if (length(clashes) == 1) "this column" else "these columns",
    " (e.g. '", clashes[[1]], "_user') or remove ",
    if (length(clashes) == 1) "it" else "them",
    " from `metadata`.",
    call. = FALSE
  )
}
