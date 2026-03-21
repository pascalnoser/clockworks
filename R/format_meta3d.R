#' Format meta3d Results
#'
#' @param w_params A data frame with results from a harmonic regression
#'   analysis.
#' @param meta3d_method The method used in the meta3d analysis.
#'
#' @returns A list of data frames containing the original and formatted results
format_meta3d <- function(ls_res, w_params, meta3d_method) {
  # Turn list of groups into a single data frame with group as a column
  res_original <- do.call(rbind, ls_res)
  row.names(res_original) <- NULL

  # Add method column to original results
  res_original$meta3d_method <- meta3d_method

  # Create formatted results data frame
  res_formatted <- data.frame(
    feature = res_original$CycID,
    group = res_original$group,
    period_estimate = res_original$meta3d_Period,
    phase_estimate = res_original$meta3d_Phase,
    mesor_estimate = res_original$meta3d_Base,
    amplitude_estimate = res_original$meta3d_AMP,
    relative_amplitude_estimate = res_original$meta3d_rAMP,
    pval = res_original$meta3d_Pvalue,
    pval_adj = res_original$meta3d_BH.Q,
    method = paste0("meta3d(", meta3d_method, ")")
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

  # Remove group column if there were no groups
  if (length(ls_res) == 1 && all(ls_res[[1]]$group == "AllSubjects")) {
    res_formatted$group <- NULL
    res_original$group <- NULL
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
