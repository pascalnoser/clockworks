#' Format LimoRhyde Results
#'
#' @param res_original A data frame with results from a LimoRhyde rhythmicity
#'   analysis.
#' @param period Period from `CircadianData` object
#'
#' @returns A list of data frames containing the original and formatted results
format_limorhyde <- function(res_original, period) {
  # TODO: Add estimates for mesor (probably from intercept), pahse, amplitude
  # Remove redundant row names
  row.names(res_original) <- NULL

  # Create formatted results data frame
  res_formatted <- data.frame(
    feature = res_original$feature,
    amplitude_estimate = NA,
    amplitude_pval = NA,
    amplitude_qval = NA,
    phase_estimate = NA,
    phase_pval = NA,
    phase_qval = NA,
    period_estimate = period,
    period_pval = NA,
    period_qval = NA,
    mesor_estimate = NA,
    pval = res_original$P.Value,
    qval = res_original$adj.P.Val,
    method = "LimoRhyde"
  )

  # Add group in second position if present
  if (!is.null(res_original$group)) {
    res_formatted <- cbind(
      res_formatted[1],
      group = res_original$group,
      res_formatted[2:ncol(res_formatted)]
    )
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
