#' Format RepeatedCircadian Results
#'
#' @param ls_res_groups A list with results from a RepeatedCircadian rhythmicity
#'   analysis, split into groups.
#' @param added_group If TRUE the "group" column will be removed
#'
#' @returns A list of data frames containing the original and formatted results
format_repeatedcircadian <- function(ls_res_groups, added_group) {
  # Turn into one data frame
  res_original <- do.call("rbind", ls_res_groups)

  # Remove meaningless row names
  rownames(res_original) <- NULL

  # Create formatted results data frame
  # TODO: DOUBLE CHECK WHAT THE OUTPUT COLUMNS ARE EXACTLY
  # TODO: Add relative amplitude (either NA or figure out a way to calculate it)
  res_formatted <- data.frame(
    feature = res_original$feature,
    group = res_original$group,
    amplitude_estimate = res_original$A,
    amplitude_pval = NA,
    amplitude_qval = NA,
    phase_estimate = res_original$phi,
    phase_pval = NA,
    phase_qval = NA,
    period_estimate = NA,
    period_pval = NA,
    period_qval = NA,
    mesor_estimate = res_original$basal,
    # relative_amplitude_estimate = ???,
    pval = res_original$pvalue,
    qval = p.adjust(res_original$pvalue, method = "BH"),
    method = "RepeatedCircadian"
  )

  # Remove group column if added temporarily by check function at the start
  if (added_group == TRUE) {
    res_original$group <- NULL
    res_formatted$group <- NULL
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
