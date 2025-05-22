#' clockworks
#'
#' Rhythmicity analysis using your method of choice.
#'
#' @param dataset A data frame or matrix representing the dataset to be checked.
#'   It is expected to contain numeric data.
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
#' @param period A number specifying the period of the samples. Can also be a
#'   vector with two numbers giving the minimum and maximum values for the
#'   period. When using a method that requires a single value for the period but
#'   two numbers are provided, the mean of the two will be used.
#' @param data_type Type of data in `dataset`. Must be one of "count" (for data
#'   following a negative bionmial distribution) or "norm" for data roughly
#'   following a normal distribution (e.g. log-CPM values).
#' @param method A string specifying the method used to analyse the data. Needs
#'   to be one of c("CircaN", "diffCircadian", "GeneCycle", "JTK_CYCLE",
#'   "RepeatedCircadian", "LimoRhyde", "LS", "meta2d", "RAIN", "TimeCycle")
#' @param method_args Additional parameters passed to the selected method.
#'
#' @returns A list with two data frames containing the original results
#'   (`res_original`) as well as formatted results that follow the same
#'   structure for all methods (`res_formatted`).
#' @export
#'
clockworks <- function(dataset,
                       metadata,
                       colname_time,
                       colname_sample,
                       colname_group = NULL,
                       colname_subject = NULL,
                       period = 24,
                       method,
                       data_type = "norm",
                       method_args = list()) {
  # Make sure method is valid
  method <- match.arg(
    method,
    choices = c(
      # "auto",
      "CircaN",
      "diffCircadian",
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

  # Make sure data type is valid
  data_type <- match.arg(
    data_type,
    choices = c("count", "norm")
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

  # Remove `dataset` and `metadata` after this so they don't clash with the
  # accessor functions of the CircadianData object and to free up memory
  # TODO: What if the user has a `dataset` or `metadata` object in their
  # environment?
  rm(dataset)
  rm(metadata)

  # Find out what kind of data we're dealing with. Check the following (and add
  # info to experiment_info slot of CircadianData object)
  # - Is there group information (just check if user defined `colname_group`)
  # - Do we have repeated measures?
  # - Are we dealing with counts (integers) or some sort of normalized values (e.g. logCPM)?
  # - Are there missing values in the data set (?)
  # - Are there samples with missing time points?
  # - Are the time points equally spaced?
  #   - If so, what is delta t?
  # - How many cycles does the data span?
  # - Is the entered period variable?
  # - Do we have replicates?
  # - Additional checks probably added after benchmark
  # - ...
  # Add experiment info to CD object
  cd <- add_experiment_info(cd, period, data_type)


  # Print CD object so the user sees what clockworks uses as input for the
  # functions as a kind of sanity check, e.g. for the number of replicats or the
  # meta data cols and so on.
  print(cd)

  # If not specified by the user, pick a method based on the results of the previous function
  # -> Implement running the functions:
  #     - MetaCycle (ARSER, JTK_CYCLE, LS, meta2d); Probably don't include meta3d?
  #       - meta2d with Brown integration?
  #     - CircaN
  #     - GeneCycle
  #     - LimoRhyde
  #     - RAIN
  #       - RAIN with corrected p-values like in Hutchison et al. 2018?
  #     - RepeatedCircadian
  #     - TimeCycle
  #     - Others?
  # TODO: Probably remove `...` in favour of `method_args`
  # rhythmicity_results <- analyze_fn(cd, method_args, ...)
  rhythmicity_results <- analyze_fn(cd, method_args)

  # Modify output such that it is consistent among methods (e.g. name of p-value column etc)?
  # ....

  ## Optional stuff ----
  # Create some plots and other output based on the results
  # - p-value distrubutions
  # - ...
  # - If multiple methods were run, some comparisons of the results
  #   - Upset plots?
  #   - Venn diagrams?

  return(rhythmicity_results)
}
