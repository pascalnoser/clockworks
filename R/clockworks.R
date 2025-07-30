#' Streamlined Rhythmicity Analysis of Time Series Data
#'
#' The main `clockworks()` function is used to detect rhythms in time series
#' data using a variety of rhythmicity analysis methods. The function takes care
#' of the data wrangling and formatting internally, allowing to use different
#' methods by using the same input and changing just one parameter.
#'
#' @param cd A `CircadianData` object with added experiment info.
#' @param method A string specifying the rhythmicity detection method to use.
#'   Supported methods include "ARSER", "CircaN", "diffCircadian", "dryR",
#'   "GeneCycle", "JTK_CYCLE", "RepeatedCircadian", "LimoRhyde" (limma or
#'   voomLmFit), "LS" (Lomb-Scargle), "meta2d", "RAIN", and "TimeCycle".
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
clockworks <- function(cd,
                       method,
                       method_args = list()) {
  #  === Define analysis function ===
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

  # Use function dispatch
  method_str <- gsub("[_-]", "", tolower(method)) # Remove "-" and "_" from method names
  analyze_fn <- get((paste0("analyze_", method_str)), mode = "function")

  # === Ensure required experiment info is present ===
  exp_info_required <- c(
    "period",
    "data_type",
    "log_transformed",
    "n_groups",
    "repeated_measures",
    "n_replicates",
    "delta_t",
    "n_cycles"
  )
  exp_info <- names(experiment_info(cd))
  missing_info <- setdiff(exp_info_required, exp_info)

  if (is.null(exp_info)) {
    stop(
      "Missing experiment information. Please run `add_experiment_info()` ",
      "on your CircadianData object before running `clockworks()`."
    )
  } else if (length(missing_info) > 0) {
    stop(
      "The following information is missing from the experiment information: ",
      paste(missing_info, collapse = ", "),
      "\nPlease run `add_experiment_info()` or manually add the values if you ",
      "know what you're doing."
    )
  }

  # === Add wave parameters if not already present ===
  w_params <- wave_params(cd)
  if (nrow(w_params) == 0) {
    wave_params(cd) <- estimate_wave_params(cd)
  }

  # === Sort to ensure format of input ===
  # Sort by group, time, and subject ID
  sort_cols <- intersect(c("time", "group", "subject_ID"), colnames(metadata(cd)))
  cd <- order_samples(cd, sort_cols)

  # === Set up parallel processing ===
  # Plan session for parallel processing
  # TODO: Can probably run `parallelly::supportsMulticore()` and if TRUE use
  # multicore instead of multisession
  # TODO: Figure out if there is a way to check if the user has run `plan(...)`
  # already so we can skip it
  # future::plan(multisession, ceiling(parallelly::availableCores()/2))

  # === Run analysis ===
  # Run rhythmicity detection with chosen method
  rhythmicity_results <- analyze_fn(cd, method_args)

  # Add to CD object
  results(cd)[[method]] <- rhythmicity_results

  return(cd)
}
