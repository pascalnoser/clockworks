#' Format TimeCycle Results
#'
#' @param ls_res_groups A list with results from a TimeCycle rhythmicity
#'   analysis, split into groups.
#' @param ls_harm_groups A list with results from a harmonic regression
#'   analysis, split into groups.
#' @param resamplings Number of resamplings used to calculate p-values.
#' @param added_group If TRUE the "group" column will be removed
#'
#' @returns A list of data frames containing the original and formatted results
format_timecycle <- function(ls_res_groups,
                             ls_harm_groups,
                             resamplings,
                             added_group) {
  # TODO: Mention in documentation that the formatted output will contain the
  # corrected p-values (can't be above 1 like in the original output)

  # Get harmonic regression params
  res_harm <- do.call("rbind", ls_harm_groups)
  rownames(res_harm) <- NULL

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
    method = "TimeCycle",
    hr_period = res_harm$period,
    hr_phase_estimate = res_harm$phase_estimate,
    hr_mesor_estimate = res_harm$mesor_estimate,
    hr_amplitude_estimate = res_harm$amplitude_estimate,
    hr_relative_amplitude_estimate = res_harm$relative_amplitude_estimate
  )

  # Remove group column if added temporarily by check function at the start
  if (added_group == TRUE) {
    res_original$group <- NULL
    res_formatted$group <- NULL
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
