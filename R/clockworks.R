clockworks <- function(dataset,
                       metadata,
                       colname_time,
                       colname_sample,
                       colname_group = NULL,
                       colname_subject = NULL,
                       colname_features = NULL,
                       period = 24) {
  # Check validity of meta data and sort columns
  metadata <- check_metadata(
    metadata,
    colname_time,
    colname_sample,
    colname_group,
    colname_subject
  )

  # Check if dataset is a data frame or matrix and make sure that the columns
  # correspond to the sample IDs in the metadata
  dataset <- check_dataset(dataset, metadata)

  # Create a CircadianData object
  cd <- CircadianData(dataet, metadata)

  # Find out what kind of data we're dealing with. Check the following (and add
  # info to CircadianData object using `experimentInfo()`)
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
  # experimentInfo(cd) <- list(period = period, ...)

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

  # Modify output such that it is consistent among methods (e.g. name of p-value column etc)?
  # ....

  ## Optional stuff ----
  # Create some plots and other output based on the results
  # - p-value distrubutions
  # - ...
  # - If multiple methods were run, some comparisons of the results
  #   - Upset plots?
  #   - Venn diagrams?
}
