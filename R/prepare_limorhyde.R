#' LimoRhyde prep
#'
#' This function is used to create the input required to run rhythmicity
#' detection with LimoRhyde.
#'
#' @param cd A `CircadianData` object
#'
#' @importFrom limma duplicateCorrelation
#' @importFrom edgeR DGEList normLibSizes
#'
#' @returns A list with inputs for `execute_limorhyde()`
prepare_limorhyde <- function(cd) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # Define model ----
  # Include group in the model only if there are groups in the first place, and
  # only if there is more than one
  if (!(is.na(cd_local$n_groups) | cd_local$n_groups == 1)) {
    # Note: Add "0" to model to prevent the first group from being the
    # reference. In this case this is preferable because we are not interested
    # in differences between groups and this makes it easier to extract the
    # mesor estimate for each group.
    str_model <- "~ 0 + group + group:(time_sin + time_cos)"
  } else {
    str_model <- "~ time_sin + time_cos"

    # # If repeated measures (and no groups), add subject ID to model
    # # NOTE: If there are groups this is not possible because the resulting
    # # design matrix would not be full rank. In case of no groups it is possible,
    # # but I decided to be consistent and always add the subject ID as blocking
    # # variable. Both approaches are completely valid.
    # if (cd_local$repeated_measures == TRUE) {
    #   str_model <- paste(str_model, "+ subject_ID")
    # }
  }
  design <- model.matrix(as.formula(str_model), data = get_metadata(cd_local))

  # Replace ':' by '.' to not throw an error when defining contrasts later
  colnames(design) = gsub(":", ".", colnames(design))

  # Define function ----
  # Use voomLmFit for count data
  if (cd_local$data_type == "count") {
    ## Option 1: Calculate normalised library sizes and pass to voomLmFit
    # # Calculate normalised library sizes
    # counts <- get_dataset(cd_local)
    # meta <- get_metadata(cd_local)
    # lib_sizes = colSums(counts) * meta$norm_factors

    # # Create input list for voomLmFit
    # inputs <- list(
    #   func = "voomLmFit",
    #   counts = counts,
    #   design = design,
    #   lib.size = lib_sizes
    # )

    ## Option 2: Create DGEList object, rerun normalisation and pass to voomLmFit
    # Create DGEList object for voomLmFit
    counts <- get_dataset(cd)
    meta <- get_metadata(cd)
    group <- meta$group
    dge <- edgeR::DGEList(counts = counts, group = group)

    # Add normalisation factors to DGEList object
    dge <- edgeR::normLibSizes(dge)

    # Create input list for voomLmFit
    inputs <- list(
      func = "voomLmFit",
      counts = dge,
      design = design
    )

    # Use limma trend for normalised data
  } else if (cd_local$data_type == "norm") {
    inputs <- list(
      func = "lmFit",
      object = get_dataset(cd_local),
      design = design
    )
  } else {
    stop("`data_type` of CircadianData object must be 'count' or 'norm'.")
  }

  # Add blocking variable ----
  if (cd_local$repeated_measures == TRUE) {
    # # If we have groups, add subject ID as blocking variable
    # # NOTE: See note above about adding to model vs. blocking
    # if (!(is.na(cd_local$n_groups) | cd_local$n_groups == 1)) {
    #   inputs$block <- get_metadata(cd_local)[["subject_ID"]]
    # }

    # Add subject ID as blocking variable
    inputs$block <- get_metadata(cd_local)[["subject_ID"]]

    # Add 'correlation' if using lmFit()
    if (cd_local$data_type == "norm") {
      corr_input <- inputs
      corr_input$func <- NULL
      inputs$correlation <- do.call(
        limma::duplicateCorrelation,
        corr_input
      )$consensus.correlation
    }
  }

  return(inputs)
}
