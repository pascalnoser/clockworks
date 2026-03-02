#' Format RAIN Results
#'
#' @param ls_res_groups A list with results from a RAIN rhythmicity
#'   analysis, split into groups.
#' @param w_params A data frame with results from a harmonic regression
#'   analysis.
#' @param t_min The minimum time point in the input data.
#' @param detlat The time interval between consecutive time points in the input data.
#' @param period The period used for the RAIN analysis.
#' @param added_group If TRUE the "group" column will be removed.
#'
#' @returns A list of data frames containing the original and formatted results
format_rain <- function(
  ls_res_groups,
  w_params,
  t_min,
  deltat,
  period,
  added_group
) {
  # Turn into one data frame
  res_original <- do.call("rbind", ls_res_groups)
  rownames(res_original) <- NULL

  # Get adjusted p-values by group
  p_adj <- ave(
    res_original$pVal,
    res_original$group,
    FUN = function(p) {
      p.adjust(p, method = "BH")
    }
  )

  # Get adjusted phase estimates
  # Note: From RAIN's perspective, the first time point is equal to detla t,
  # so we subtract delta t from the phase estimates and add the minimum time
  # point to get the correct phase estimates. This way, the phase estimates
  # are equal to the peak times (bounded between 0 and the period).
  phase_original <- res_original$phase
  phase_estimate <- (phase_original - deltat + t_min) %% period

  # Create formatted results data frame
  res_formatted <- data.frame(
    feature = res_original$feature,
    group = res_original$group,
    period_estimate = res_original$period,
    phase_estimate = phase_estimate,
    pval = res_original$pVal,
    pval_adj = p_adj,
    method = "RAIN"
  )

  # Add wave parameters
  merge_cols <- intersect(c("feature", "group"), colnames(w_params))
  colnames(w_params) <- paste0("hr_", colnames(w_params))
  res_formatted <- merge(
    x = res_formatted,
    y = w_params,
    by.x = merge_cols,
    by.y = paste0("hr_", merge_cols),
    all.x = TRUE, # should not make a difference but just to be safe
    sort = FALSE
  )

  # Remove group column if added temporarily by check function at the start
  if (added_group == TRUE) {
    res_original$group <- NULL
    res_formatted$group <- NULL
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
