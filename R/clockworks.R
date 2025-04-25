clockworks <- function(dataset,
                       metadata,
                       colname_time,
                       colname_sample,
                       colname_group = NULL,
                       colname_subject = NULL,
                       period = 24,
                       method = "auto",
                       ...) {
  # Make sure method is valid
  method <- match.arg(
    method,
    choices = c(
      "auto",
      "CircaN",
      "RepeatedCircadian",
      "JTK_CYCLE",
      "LS"
    )
  )

  # Plan session for parallel processing
  # TODO: Can probably run `parallelly::supportsMulticore()` and if TRUE use
  # multicore instead of multisession
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
  cd <- add_experiment_info(cd, period)

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
  rhythmicity_results <- analyze_fn(cd, ...)

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
