#' Format RAIN Results
#'
#' @param ls_res_groups A list with results from a RAIN rhythmicity
#'   analysis, split into groups.
#' @param remove_group If TRUE the "group" column will be removed
#'
#' @returns A list of data frames containing the original and formatted results
format_rain <- function(ls_res_groups, added_group) {
  # Turn into one data frame
  res_original <- do.call("rbind", ls_res_groups)

  # Remove redundant row names
  rownames(res_original) <- NULL

  # Get adjusted p-values
  qvals <- ave(
    res_original$pVal,
    res_original$group,
    FUN = function(p)
      p.adjust(p, method = "BH")
  )

  # Create formatted results data frame
  res_formatted <- data.frame(
    feature = res_original$feature,
    group = res_original$group,
    amplitude_estimate = NA,
    amplitude_pval = NA,
    amplitude_qval = NA,
    phase_estimate = NA,
    phase_pval = NA,
    phase_qval = NA,
    period_estimate = res_original$period,
    period_pval = NA,
    period_qval = NA,
    mesor_estimate = NA,
    pval = res_original$pVal,
    qval = qvals,
    method = "RAIN"
  )

  # Remove group column if added temporarily by check function at the start
  if (added_group == TRUE) {
    res_original$group <- NULL
    res_formatted$group <- NULL
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
