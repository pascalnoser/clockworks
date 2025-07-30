#' Format GeneCycle Results
#'
#' @param ls_res_groups A list with results from a GeneCycle rhythmicity
#'   analysis, split into groups.
#' @param w_params A data frame with results from a harmonic regression
#'   analysis.
#' @param added_group If TRUE the "group" column will be removed
#'
#' @returns A list of data frames containing the original and formatted results
format_genecycle <- function(ls_res_groups, w_params, added_group) {
  # Turn into one data frame
  res_original <- do.call("rbind", ls_res_groups)

  # Remove redundant row names
  rownames(res_original) <- NULL

  # Calculate adjusted p-values by group
  p_adj <- ave(
    res_original$pval,
    res_original$group,
    FUN = function(p)
      p.adjust(p, method = "BH")
  )

  # Create formatted results data frame
  res_formatted <- data.frame(
    feature = res_original$feature,
    group = res_original$group,
    pval = res_original$pval,
    pval_adj = p_adj,
    method = "GeneCycle"
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
