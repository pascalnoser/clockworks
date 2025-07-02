#' Format TimeCycle Results
#'
#' @param ls_res_groups A list with results from a TimeCycle rhythmicity
#'   analysis, split into groups.
#' @param ls_harm_groups A list with results from a harmonic regression
#'   analysis, split into groups.
#' @param added_group If TRUE the "group" column will be removed
#'
#' @returns A list of data frames containing the original and formatted results
format_timecycle <- function(ls_res_groups, ls_harm_groups, added_group) {
  # Turn into one data frame
  res_original <- do.call("rbind", ls_res_groups)

  # Remove redundant row names
  rownames(res_original) <- NULL

  # Get harmonic regression params
  res_harm <- do.call("rbind", ls_harm_groups)
  rownames(res_harm) <- NULL

  # Create formatted results data frame
  # TODO: Figure out which values to report
  res_formatted <- data.frame(
    feature = res_original$sampleNames,
    group = res_original$group,
    period_estimate = res_original$Period.in.Hours,
    phase_estimate = res_original$Phase.in.Hours,
    mesor_estimate = NA,
    amplitude_estimate = res_original$Amp,
    relative_amplitude_estimate = NA,
    pval = res_original$pVals,
    pval_adj = res_original$pVals.adj,
    method = "TimeCycle"
  )

  # Remove group column if added temporarily by check function at the start
  if (added_group == TRUE) {
    res_original$group <- NULL
    res_formatted$group <- NULL
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
