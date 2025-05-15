#' Detect rhythmicity with LimoRhyde
#'
#' This function runs rhythmicity detection with LimoRhyde
#'
#' @param cd A `CircadianData` object.
#' @param method_args Additional parameters passed to `limorhyde::limorhyde()`
#'
#' @importFrom limorhyde limorhyde
#' @importFrom edgeR voomLmFit
#'
#' @returns A data frame with the results of the LimoRhyde analysis.
analyze_limorhyde_gammel <- function(cd, method_args) {
  # Calculate time_cos and time_sin based on the Time column
  limo <- limorhyde::limorhyde(
    time = metadata(cd)$time,
    colnamePrefix = "time_",
    period = cd$period
  )

  # Add to metadata
  metadata(cd) <- cbind(metadata(cd), limo)

  # Define model
  if (cd$group_info == TRUE) {
    str_model <- "~ 0 + Group + Group:(time_sin + time_cos)"
  } else {
    str_model <- "~ time_cos + time_sin"
  }
  design <- model.matrix(as.formula(str_model), data = metadata(cd))

  # Replace ':' by '.' to not throw an error when defining contrasts later
  colnames(design) <- gsub(":", ".", colnames(design))

  # Fit model. Use fitting method depending on if we need to block for subject
  # ID (repeated measures) and on whether we have count data or log-expression
  # values
  # ...

  # Extract interactions with time_sin and time_cos
  # ...

  # Return results
  # ...
}
