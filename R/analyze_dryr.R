#' Detect rhythmicity with dryR
#'
#' This function runs rhythmicity detection with dryR
#'
#' @param cd A `CircadianData` object.
#' @param method_args Additional parameters passed to `dryR::f_24()` or
#'   `dryR::dryseq_single()`
#' @param batch_correction Logical. If `TRUE` and the data contain repeated
#'   measures, subject batch effects are removed before the analysis.
#'
#' @returns A list with the original and formatted results of the dryR analysis.
analyze_dryr <- function(cd, method_args = list(),
                         batch_correction = TRUE) {
  # TODO: Count data with repeated measures fails (`DESeqDataSet()`: "some
  # values in assay are negative") because subject effects are removed on the
  # raw counts in `check_dryr()`. Options: (1) convert to log-CPM, remove
  # subject effects and use `f_24()` like the other methods, (2) skip batch
  # correction for counts with a warning, (3) remove subject effects on log-CPM
  # and back-transform to pseudo-counts for `dryseq_single()`. Discuss with
  # supervisor. Works with `clockworks(..., batch_correction = FALSE)`.

  # Check if cd object contains necessary columns and add them if not
  cd_local <- check_dryr(cd, batch_correction)

  # Remove group column later if added temporarily by check
  added_group <- ifelse(is.na(cd_local$n_groups), TRUE, FALSE)

  # Create empty list for results
  ls_res_groups <- list()

  # Run rhythmicity detection for each group separately
  groups <- unique(get_metadata(cd_local)[["group"]])
  for (grp in groups) {
    # Prepare inputs
    inputs <- prepare_dryr(cd_local, grp)

    # Run rhythmicity analysis
    df_res_grp <- execute_dryr(inputs, grp, method_args)

    # Add to list
    ls_res_groups[[grp]] <- df_res_grp
  }

  # Postprocessing
  ls_res <- format_dryr(
    ls_res_groups = ls_res_groups,
    w_params = get_wave_params(cd_local),
    method = inputs$method,
    added_group = added_group
  )

  return(ls_res)
}
