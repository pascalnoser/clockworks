#' Format Lomb-Scargle Results
#'
#' @param ls_res_groups A list with results from a Lomb-Scargle rhythmicity
#'   analysis, split into groups.
#' @param remove_group If TRUE the "group" column will be removed
#'
#' @returns A list of data frames containing the original and formatted results
format_ls <- function(ls_res_groups, remove_group) {
  # Turn into one data frame
  res_original <- do.call("rbind", ls_res_groups)

  # Remove meaningless row names
  rownames(res_original) <- NULL

  # Create formatted results data frame
  res_formatted <- data.frame(
    feature = res_original$CycID,
    group = res_original$group,
    amplitude_estimate = res_original$AMP,
    amplitude_pval = NA,
    amplitude_qval = NA,
    phase_estimate = NA,
    phase_pval = NA,
    phase_qval = NA,
    period_estimate = NA,
    period_pval = NA,
    period_qval = NA,
    mesor_estimate = NA,
    pval = res_original$ADJ.P,
    qval = res_original$BH.Q,
    method = "Lomb-Scargle"
  )

  # Remove group column if added temporarily by check function at the start
  if (remove_group == TRUE) {
    res_original$group <- NULL
    res_formatted$group <- NULL
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
