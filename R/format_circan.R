#' Format CircaN Results
#'
#' @param ls_res_groups A list with results from a CircaN rhythmicity
#'   analysis, split into groups.
#' @param ls_harm_groups A list with results from a harmonic regression
#'   analysis, split into groups.
#' @param added_group If TRUE the "group" column will be removed
#'
#' @returns A list of data frames containing the original and formatted results
format_circan <- function(ls_res_groups, ls_harm_groups, added_group) {
  # Turn into one data frame
  res_original <- do.call("rbind", ls_res_groups)

  # Remove redundant row names
  rownames(res_original) <- NULL

  # Get harmonic regression params
  res_harm <- do.call("rbind", ls_harm_groups)
  rownames(res_harm) <- NULL

  # TODO: Use harmonic regression or CircaN params?

  # Create formatted results data frame
  res_formatted <- data.frame(
    feature = res_original$feature,
    group = res_original$group,
    period_estimate = res_original$estimate.per,
    phase_estimate = res_original$estimate.phase,
    mesor_estimate = NA,
    amplitude_estimate = res_original$estimate.amp,
    relative_amplitude_estimate = NA,
    pval = res_original$combined_pval,
    pval_adj = res_original$BH_combined,
    method = "CircaN"
  )

  # Remove group column if added temporarily by check function at the start
  if (added_group == TRUE) {
    res_original$group <- NULL
    res_formatted$group <- NULL
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
