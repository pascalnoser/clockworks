#' Format CircaN Results
#'
#' @param ls_res_groups A list with results from a CircaN rhythmicity
#'   analysis, split into groups.
#' @param remove_group If TRUE the "group" column will be removed
#'
#' @returns A list of data frames containing the original and formatted results
format_circan <- function(ls_res_groups, remove_group) {
  # Turn into one data frame
  res_original <- do.call("rbind", ls_res_groups)

  # Remove meaningless row names
  rownames(res_original) <- NULL

  # Create formatted results data frame
  res_formatted <- data.frame(
    feature = res_original$feature,
    group = res_original$group,
    amplitude_estimate = res_original$estimate.amp,
    amplitude_pval = res_original$p.value.amp,
    amplitude_qval = res_original$BH.q.value.amp,
    phase_estimate = res_original$estimate.phase,
    phase_pval = res_original$p.value.phase,
    phase_qval = res_original$BH.q.value.phase,
    period_estimate = res_original$estimate.per,
    period_pval = res_original$p.value.per,
    period_qval = res_original$BH.q.value.per,
    mesor_estimate = NA,
    pval = res_original$combined_pval,
    qval = res_original$BH_combined,
    method = "CircaN"
  )

  # Remove group column if added temporarily by check function at the start
  if (remove_group == TRUE) {
    res_original$group <- NULL
    res_formatted$group <- NULL
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
