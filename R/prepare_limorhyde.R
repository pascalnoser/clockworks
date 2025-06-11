#' LimoRhyde prep
#'
#' This function is used to create the input required to run rhythmicity
#' detection with LimoRhyde.
#'
#' @param cd A `CircadianData` object
#'
#' @importFrom limma duplicateCorrelation
#'
#' @returns A list with inputs for `execute_limorhyde()`
prepare_limorhyde <- function(cd) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # Define model ----
  if (!is.na(cd_local$n_groups)) {
    # Note: Add "0" to model to prevent the first group from being the
    # reference. In this case this is preferable because we are not interested
    # in differences between groups and this makes it easier to extract the
    # mesor estimate for each group.
    str_model <- "~ 0 + group + group:(time_sin + time_cos)"
  } else {
    str_model <- "~ time_cos + time_sin"

    # If repeated measures (and no groups), add subject ID to model
    if (cd_local$repeated_measures == TRUE) {
      str_model <- paste(str_model, "+ subject_ID")
    }
  }
  design <- model.matrix(as.formula(str_model), data = metadata(cd_local))

  # Replace ':' by '.' to not throw an error when defining contrasts later
  colnames(design) = gsub(":", ".", colnames(design))


  # Define function ----
  if (cd_local$data_type == "count") {
    # Use voomLmFit for count data
    inputs <- list(
      func = "voomLmFit",
      counts = dataset(cd_local),
      design = design,
      sample.weights = TRUE  # TODO: Figure out if this should be TRUE or FALSE
    )

  } else if (cd_local$data_type == "norm") {
    # Use limma trend for normalised data
    inputs <- list(
      func = "lmFit",
      object = dataset(cd_local),
      design = design
    )
  }


  # Add blocking variable ----
  if (cd_local$repeated_measures == TRUE) {
    # If we have groups, add subject ID as blocking variable
    if (!is.na(cd_local$n_groups)) {
      inputs$block <- metadata(cd_local)[["subject_ID"]]
    }

    # Add 'correlation' if using lmFit()
    if (cd_local$data_type == "norm") {
      corr_input <- inputs
      corr_input$func <- NULL
      inputs$correlation <- do.call(limma::duplicateCorrelation, corr_input)$consensus.correlation
    }
  }

  return(inputs)
}
