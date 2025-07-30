#' Format TimeCycle Results
#'
#' @param ls_res_groups A list with results from a TimeCycle rhythmicity
#'   analysis, split into groups.
#' @param w_params A data frame with results from a harmonic regression
#'   analysis, split into groups.
#' @param resamplings Number of resamplings used to calculate p-values.
#' @param added_group If TRUE the "group" column will be removed
#'
#' @returns A list of data frames containing the original and formatted results
format_timecycle <- function(ls_res_groups,
                             w_params,
                             resamplings,
                             added_group) {
  # TODO: Mention in documentation that the formatted output will contain the
  # corrected p-values (can't be above 1 like in the original output)

  # Turn into one data frame
  res_original <- do.call("rbind", ls_res_groups)
  rownames(res_original) <- NULL

  # Correct p-values and adjusted p-values for formatted output
  pvals <- res_original$pVals * (resamplings / (resamplings + 1))
  pvals_adj <- p.adjust(pvals, method = "BH")

  # Create formatted results data frame
  res_formatted <- data.frame(
    feature = res_original$sampleNames,
    group = res_original$group,
    period_estimate = res_original$Period.in.Hours,
    phase_estimate = res_original$Phase.in.Hours,
    amplitude_estimate = res_original$Amp,
    pval = pvals,
    pval_adj = pvals_adj,
    method = "TimeCycle"
  )

  # Add wave parameters
  merge_cols <- intersect(c("feature", "group"), colnames(w_params))
  colnames(w_params) <- paste0("hr_", colnames(w_params))
  res_formatted <- merge(
    x = res_formatted,
    y = w_params,
    by.x = merge_cols,
    by.y = paste0("hr_", merge_cols),
    all.x = TRUE, # should not make a difference but just to be safe
    sort = FALSE
  )

  # Remove group column if added temporarily by check function at the start
  if (added_group == TRUE) {
    res_original$group <- NULL
    res_formatted$group <- NULL
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
