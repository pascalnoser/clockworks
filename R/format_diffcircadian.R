#' Format diffCircadian Results
#'
#' @param df_original A data frame with results from a diffCircadian rhythmicity
#'   analysis.
#' @param ls_harm_groups A list with results from a harmonic regression
#'   analysis, split into groups.
#' @param period Period entered by the user.
#' @param added_group If TRUE the "group" column will be removed.
#' @param log_transformed Logical, if the data is log-transformed
#' @param log_base Logarithmic base of data. Only relevant if `log_transformed`
#'   is `TRUE`.
#'
#' @returns A list of data frames containing the original and formatted results
format_diffcircadian <- function(df_original,
                                 ls_harm_groups,
                                 period,
                                 added_group,
                                 log_transformed = FALSE,
                                 log_base = 2) {
  # TODO: Probably remove `log_transformed` and `log_base` parameters since we
  # will likely only report the relative amplitude from the harmonic regression.
  # Alternatively, uncomment the relative amplitude calculation below.

  # Get harmonic regression params
  df_harm <- do.call("rbind", ls_harm_groups)
  rownames(df_harm) <- NULL

  # Get adjusted p-values by group
  p_adj <- ave(
    df_original$pvalue,
    df_original$group,
    FUN = function(p)
      p.adjust(p, method = "BH")
  )

  # # Get relative amplitude. If data is log-transformed, calculate relative
  # # amplitude in linear scale
  # if (log_transformed == FALSE) {
  #   rel_amp <- df_original$amp / df_original$offset
  # } else {
  #   log_amp <- df_original$amp
  #   rel_amp <- compute_relative_amplitude(log_amp, log_base)
  # }

  # Create formatted results data frame
  res_formatted <- data.frame(
    feature = df_original$feature,
    group = df_original$group,
    period_estimate = period,
    phase_estimate = df_original$phase,
    mesor_estimate = df_original$offset,
    amplitude_estimate = df_original$amp,
    # relative_amplitude_estimate = rel_amp,
    pval = df_original$pvalue,
    pval_adj = p_adj,
    method = "diffCircadian",
    hr_period = df_harm$period,
    hr_phase_estimate = df_harm$phase_estimate,
    hr_mesor_estimate = df_harm$mesor_estimate,
    hr_amplitude_estimate = df_harm$amplitude_estimate,
    hr_relative_amplitude_estiamte = df_harm$relative_amplitude_estimate
  )

  # Remove row names from formatted results
  row.names(res_formatted) <- NULL

  # Remove group column if added temporarily by check function at the start
  if (added_group == TRUE) {
    df_original$group <- NULL
    res_formatted$group <- NULL
  }

  return(list(res_original = df_original, res_formatted = res_formatted))
}
