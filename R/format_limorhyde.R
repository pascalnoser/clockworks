#' Format LimoRhyde Results
#'
#' @param res_original A data frame with results from a LimoRhyde rhythmicity
#'   analysis.
#' @param ls_harm_groups A list with results from a harmonic regression
#'   analysis, split into groups.
#' @param period Period from `CircadianData` object
#'
#' @returns A list of data frames containing the original and formatted results
format_limorhyde <- function(res_original, ls_harm_groups, period) {
  # Get harmonic regression params
  res_harm <- do.call("rbind", ls_harm_groups)
  rownames(res_harm) <- NULL

  # Get wave params from fitted parameters
  beta_0 <- res_original$AveExpr
  beta_1 <- res_original$time_sin
  beta_2 <- res_original$time_cos
  mesor_estimate <- beta_0
  amplitude_estimate <- sqrt(beta_1^2 + beta_2^2)
  phase_estimate <- atan2(beta_2, beta_1) * (period / (2 * pi))

  # Create formatted results data frame
  df_formatted <- data.frame(
    feature = res_original$feature,
    period_estimate = period,
    phase_estimate = phase_estimate,
    mesor_estimate = mesor_estimate,
    amplitude_estimate = amplitude_estimate,
    pval = res_original$P.Value,
    qval = res_original$adj.P.Val,
    method = "LimoRhyde",
    hr_period = res_harm$period,
    hr_phase_estimate = res_harm$phase_estimate,
    hr_mesor_estimate = res_harm$mesor_estimate,
    hr_amplitude_estimate = res_harm$amplitude_estimate,
    hr_relative_amplitude_estimate = res_harm$relative_amplitude_estimate
  )

  # Add group in second position if present
  if (!is.null(res_original$group)) {
    df_formatted <- cbind(
      df_formatted[1],
      group = res_original$group,
      df_formatted[2:ncol(df_formatted)]
    )
  }

  return(list(res_original = res_original, res_formatted = df_formatted))
}
