#' Validate experiment info before analysis
#'
#' @param cd_obj A `CircadianData` object
#'
#' @returns NULL
#'
validate_exp_info <- function(cd_obj) {
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
  exp_info <- names(experiment_info(cd_obj))
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

  if (cd_obj$log_transformed == TRUE && (is.null(cd_obj$log_base) || !is.numeric(cd_obj$log_base))) {
    stop(
      "If `log_transformed` is TRUE, `log_base` must be a single numeric value."
    )
  }

  # No return needed
  invisible(NULL)
}
