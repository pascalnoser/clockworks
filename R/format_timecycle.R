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

  # Turn into one data frame
  df_original <- do.call("rbind", ls_res_groups)
  rownames(df_original) <- NULL

  # Get harmonic regression params
  df_harm <- do.call("rbind", ls_harm_groups)
  rownames(df_harm) <- NULL

  # Correct p-values and adjusted p-values for formatted output
  pvals <- df_original$pVals * (resamplings / (resamplings + 1))
  pvals_adj <- p.adjust(pvals, method = "BH")

  # Create formatted results data frame
  res_formatted <- data.frame(
    feature = df_original$sampleNames,
    group = df_original$group,
    period_estimate = df_original$Period.in.Hours,
    phase_estimate = df_original$Phase.in.Hours,
    amplitude_estimate = df_original$Amp,
    pval = pvals,
    pval_adj = pvals_adj,
    method = "TimeCycle",
    hr_period = df_harm$period,
    hr_phase_estimate = df_harm$phase_estimate,
    hr_mesor_estimate = df_harm$mesor_estimate,
    hr_amplitude_estimate = df_harm$amplitude_estimate,
    hr_relative_amplitude_estiamte = df_harm$relative_amplitude_estimate
  )

  # Remove group column if added temporarily by check function at the start
  if (added_group == TRUE) {
    df_original$group <- NULL
    res_formatted$group <- NULL
  }

  return(list(res_original = df_original, res_formatted = res_formatted))
}
