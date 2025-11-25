#' Format LimoRhyde Results
#'
#' @param res_original A data frame with results from a LimoRhyde rhythmicity
#'   analysis.
#' @param w_params A data frame with results from a harmonic regression
#'   analysis.
#' @param period Period from `CircadianData` object
#'
#' @returns A list of data frames containing the original and formatted results
format_limorhyde <- function(res_original, w_params, period) {
  # Get wave params from fitted parameters
  beta_0 <- res_original$AveExpr
  beta_1 <- res_original$time_sin
  beta_2 <- res_original$time_cos
  mesor_estimate <- beta_0
  amplitude_estimate <- sqrt(beta_1^2 + beta_2^2)
  # Phase estimates for the following two models. I'm going with cosine
  # y = M + A * cos(2 * pi / T * (t - phi))
  # phase_estimate <- (atan2(beta_2, beta_1) * period / (2 * pi)) %% period
  # y = M + A * sin(2 * pi / T * (t + phi))
  phase_estimate <- (atan2(beta_1, beta_2) * period / (2 * pi)) %% period

  # Create formatted results data frame
  res_formatted <- data.frame(
    feature = res_original$feature,
    period_estimate = period,
    phase_estimate = phase_estimate,
    mesor_estimate = mesor_estimate,
    amplitude_estimate = amplitude_estimate,
    pval = res_original$P.Value,
    pval_adj = res_original$adj.P.Val,
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

  return(list(res_original = res_original, res_formatted = res_formatted))
}
