#' Streamlined Rhythmicity Analysis of Time Series Data
#'
#' The main `clockworks()` function is used to detect rhythms in time series
#' data using a variety of rhythmicity analysis methods. The function takes care
#' of the data wrangling and formatting internally, allowing to use different
#' methods by using the same input and changing just one parameter.
#'
#' @param dataset A data frame or matrix containing the time-series data. Each
#'   row should represent a feature (e.g., a gene or metabolite), and each
#'   column should correspond to a sample. The data within should be numeric.
#' @param metadata A data frame with metadata for the samples in `dataset`. It
#'   must contain columns for sample IDs and time points, and can optionally
#'   include grouping and subject information.
#' @param colname_sample A string specifying the column name in `metadata` that
#'   contains unique sample identifiers. These identifiers should correspond to
#'   the column names of the `dataset`.
#' @param colname_time A string specifying the column name in `metadata` that
#'   contains the time information for each sample. This column must be numeric.
#' @param colname_group A string specifying the column name in `metadata` that
#'   identifies the experimental group for each sample (e.g., "treatment" vs.
#'   "control"). Default is `NULL` for datasets without groups.
#' @param colname_subject A string specifying the column name in `metadata` that
#'   identifies the subject for each sample. This is useful for repeated
#'   measures designs where multiple samples are taken from the same subject
#'   over time. Default is `NULL`.
#' @param period The expected period of the rhythm in the same units as
#'   `colname_time`. This can be a single numeric value or a numeric vector of
#'   length two specifying the minimum and maximum period to be considered. If a
#'   method requires a single period, the mean of the range will be used.
#' @param data_type A string indicating the type of data in `dataset`. Must be
#'   either `"count"` for data that can be modeled by a negative binomial
#'   distribution (e.g., raw gene counts) or `"norm"` for data that is
#'   approximately normally distributed (e.g., log-transformed CPM values).
#' @param method A string specifying the rhythmicity detection method to use.
#'   Supported methods include "ARSER", "CircaN", "diffCircadian", "dryR",
#'   "GeneCycle", "JTK_CYCLE", "RepeatedCircadian", "LimoRhyde", "LS"
#'   (Lomb-Scargle), "meta2d", "RAIN", and "TimeCycle".
#' @param log_transformed A logical value indicating whether the data in
#'   `dataset` has been log-transformed. This information is used for
#'   calculating the relative amplitude in the output of the harmonic regression
#'   but has no influence on the chosen method.
#' @param log_base A numeric value specifying the base of the logarithm if the
#'   data is log-transformed. Only used if `log_transformed` is `TRUE`. Default
#'   is 2.
#' @param verbose A logical value indicating whether the `CircadianData` object
#'   should be printed before running the rhythmicity detection.
#' @param method_args A list of additional arguments to be passed directly to
#'   the chosen analysis method. This allows for more advanced customisation of
#'   the analysis.
#'
#' @details Additional details coming soon...
#'
#' @returns A `CircadianData` object including the results of a rhythmicity
#'   analysis using the chosen `method`.
#'
#' @export
#'
clockworks <- function(dataset,
                       metadata,
                       colname_sample,
                       colname_time,
                       colname_group = NULL,
                       colname_subject = NULL,
                       period = 24,
                       method,
                       data_type = "norm",
                       log_transformed = FALSE,
                       log_base = 2,
                       verbose = FALSE,
                       method_args = list()) {
  # Make sure method is valid
  method <- match.arg(
    method,
    choices = c(
      # "auto",
      "ARSER",
      "CircaN",
      "diffCircadian",
      "dryR",
      "GeneCycle",
      "JTK_CYCLE",
      "RepeatedCircadian",
      "LimoRhyde",
      "LS",
      "meta2d",
      "RAIN",
      "TimeCycle"
    )
  )

  # Plan session for parallel processing
  # TODO: Can probably run `parallelly::supportsMulticore()` and if TRUE use
  # multicore instead of multisession
  # TODO: Figure out if there is a way to check if the user has run `plan(...)`
  # already so we can skip it
  # future::plan(multisession, ceiling(parallelly::availableCores()/2))

  # Use function dispatch
  method_str <- gsub("[_-]", "", tolower(method)) # Remove "-" and "_" from method names
  analyze_fn <- get((paste0("analyze_", method_str)), mode = "function")

  # Make sure data type is valid
  data_type <- match.arg(
    data_type,
    choices = c("count", "norm")
  )

  # Check validity of meta data and sort columns
  metadata <- check_metadata(
    metadata = metadata,
    colname_sample = colname_sample,
    colname_time = colname_time,
    colname_group = colname_group,
    colname_subject = colname_subject
  )

  # Check if dataset is a data frame or matrix and make sure that the columns
  # correspond to the sample IDs in the metadata
  dataset <- check_dataset(dataset, metadata)

  # Create a CircadianData object
  cd <- CircadianData(dataset, metadata)

  # Add experiment info to CD object
  # TODO: Add more checks after done with benchmark
  cd <- add_experiment_info(cd, period, data_type, log_transformed, log_base)

  # Sort by group, time, and subject ID
  sort_cols <- intersect(c("time", "group", "subject_ID"), colnames(metadata(cd)))
  cd <- order_samples(cd, sort_cols)

  # Print CD object so the user sees what clockworks uses as input for the
  # functions as a kind of sanity check
  if (verbose == TRUE) print(cd)

  # Run rhythmicity detection with chosen method
  rhythmicity_results <- analyze_fn(cd, method_args)

  # Add to CD object
  results(cd)[[method]] <- rhythmicity_results

  return(cd)
}
