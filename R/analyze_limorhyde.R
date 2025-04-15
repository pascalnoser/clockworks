#' Detect rhythmicity with LimoRhyde
#'
#' This function runs rhythmicity detection using LimoRhyde. It expects a
#' `CircadianDaata` object (`cd`) as input. The metadata slot of this object
#' must contain a data.frame with a column named `Time` that contains time point
#' information for a given sample. The experimentInfo of `cd` slot must contain
#' a `period` parameter.
#'
#' @param cd A `CircadianData` object.
#' @param ... Additional parameters passed to `limorhyde::limorhyde()`
#'
#' @importFrom limorhyde limorhyde
#' @importFrom edgeR voomLmFit
#'
#' @returns A data frame with the results of the LimoRhyde analysis.
analyze_limorhyde <- function(cd, ...) {
  # TODO: Add info about what model will be used and so on to documentation
  # TODO: Add info about required parameters in experimentInfo slot (group_info,
  # repeated_measures, others?)
  # TODO: Handle the "..."

  # Calculate time_cos and time_sin based on the Time column
  limo <- limorhyde::limorhyde(
    time = metadata(cd)$Time,
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
