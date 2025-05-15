#' LimoRhyde prep
#'
#' This function is used to create the input required to run rhythmicity
#' detection with LimoRhyde.
#'
#' @param cd A `CircadianData` object
#'
#' @returns A list with inputs for `execute_limorhyde()`
prepare_limorhyde <- function(cd) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # Define model
  if (!is.na(cd_local$n_groups)) {
    str_model <- "~ 0 + group + group:(time_sin + time_cos)"
  } else {
    str_model <- "~ time_cos + time_sin"
  }
  design <- model.matrix(as.formula(str_model), data = metadata(cd_local))

  # Replace ':' by '.' to not throw an error when defining contrasts later
  colnames(design) = colnames(design) %>% str_replace(":", ".")

  # Create list with inputs for run
  if (cd_local$type == "count") {
    # Use voomLmFit for count data
    ls_inputs <- list(
      counts = dataset(cd_local),
      design = design
    )

    # Add subject ID as blocking variable if present
    if (cd_local$repeated_measures == TRUE) {
      ls_inputs$block <- metadata(cd_local)[["subject_ID"]]
    }

  } else if (cd_local$type == "norm") {
    # Use limma trend for normalised data
    ls_inputs <- list(
      object = dataset(cd_local),
      design = design
    )
  }

  return(ls_inputs)
}
