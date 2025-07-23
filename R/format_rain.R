#' Format RAIN Results
#'
#' @param ls_res_groups A list with results from a RAIN rhythmicity
#'   analysis, split into groups.
#' @param ls_harm_groups A list with results from a harmonic regression
#'   analysis, split into groups.
#' @param added_group If TRUE the "group" column will be removed
#'
#' @returns A list of data frames containing the original and formatted results
format_rain <- function(ls_res_groups, ls_harm_groups, added_group) {
  # Get harmonic regression params
  res_harm <- do.call("rbind", ls_harm_groups)
  rownames(res_harm) <- NULL

  # Turn into one data frame
  res_original <- do.call("rbind", ls_res_groups)
  rownames(res_original) <- NULL

  # Get adjusted p-values by group
  p_adj <- ave(
    res_original$pVal,
    res_original$group,
    FUN = function(p)
      p.adjust(p, method = "BH")
  )

  # Create formatted results data frame
  res_formatted <- data.frame(
    feature = res_original$feature,
    group = res_original$group,
    period_estimate = res_original$period,
    pval = res_original$pVal,
    pval_adj = p_adj,
    method = "RAIN",
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
