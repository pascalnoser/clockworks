#' Format diffCircadian Results
#'
#' @param res_original A data frame with results from a diffCircadian rhythmicity
#'   analysis.
#' @param period Period entered by the user.
#' @param added_group If TRUE the "group" column will be removed.
#'
#' @returns A list of data frames containing the original and formatted results
format_diffcircadian <- function(res_original, period, added_group) {
  # Calculate adjusted p-values by group
  grp_split <- split(res_original, res_original$group)
  p_adj <- unlist(lapply(grp_split, function(grp) {
    p.adjust(grp$pvalue, method = "BH")
  }))

  # Create formatted results data frame
  res_formatted <- data.frame(
    feature = res_original$feature,
    group = res_original$group,
    amplitude_estimate = res_original$amp,
    amplitude_pval = NA,
    amplitude_qval = NA,
    phase_estimate = res_original$phase,
    phase_pval = NA,
    phase_qval = NA,
    period_estimate = period,
    period_pval = NA,
    period_qval = NA,
    mesor_estimate = res_original$offset,
    pval = res_original$pvalue,
    qval = p_adj,
    method = "diffCircadian"
  )

  # Remove row names from formatted results
  row.names(res_formatted) <- NULL

  # Remove group column if added temporarily by check function at the start
  if (added_group == TRUE) {
    res_original$group <- NULL
    res_formatted$group <- NULL
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
