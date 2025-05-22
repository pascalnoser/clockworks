#' Format GeneCycle Results
#'
#' @param ls_res_groups A list with results from a GeneCycle rhythmicity
#'   analysis, split into groups.
#' @param added_group If TRUE the "group" column will be removed
#'
#' @returns A list of data frames containing the original and formatted results
format_genecycle <- function(ls_res_groups, added_group) {
  # Turn into one data frame
  res_original <- do.call("rbind", ls_res_groups)

  # Remove redundant row names
  rownames(res_original) <- NULL

  # Calculate adjusted p-values by group
  grp_split <- split(res_original, res_original$group)
  p_adj <- unlist(lapply(grp_split, function(grp) {
    p.adjust(grp$pval, method = "BH")
  }))

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
    period_estimate = NA,
    period_pval = NA,
    period_qval = NA,
    mesor_estimate = NA,
    pval = res_original$pval,
    qval = p_adj,
    method = "GeneCycle"
  )

  # Remove group column if added temporarily by check function at the start
  if (added_group == TRUE) {
    res_original$group <- NULL
    res_formatted$group <- NULL
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
