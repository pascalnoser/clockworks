#' Format RepeatedCircadian Results
#'
#' @param ls_res_groups A list with results from a RepeatedCircadian rhythmicity
#'   analysis, split into groups.
#' @param ls_harm_groups A list with results from a harmonic regression
#'   analysis, split into groups.
#' @param added_group If TRUE the "group" column will be removed
#' @param log_transformed Logical, if the data is log-transformed
#' @param log_base Logarithmic base of data. Only relevant if `log_transformed`
#'   is `TRUE`.
#'
#' @returns A list of data frames containing the original and formatted results
format_repeatedcircadian <- function(ls_res_groups,
                                     ls_harm_groups,
                                     added_group,
                                     log_transformed = FALSE,
                                     log_base = 2) {

  # TODO: Probably remove `log_transformed` and `log_base` parameters since we
  # will likely only report the relative amplitude from the harmonic regression.
  # Alternatively, uncomment the relative amplitude calculation below.

  # Get harmonic regression params
  df_harm <- do.call("rbind", ls_harm_groups)
  rownames(df_harm) <- NULL

  # Turn into one data frame
  df_original <- do.call("rbind", ls_res_groups)
  rownames(df_original) <- NULL

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
  #   rel_amp <- df_original$A / df_original$basal
  # } else {
  #   log_amp <- df_original$A
  #   rel_amp <- compute_relative_amplitude(log_amp, log_base)
  # }

  # Create formatted results data frame
  # TODO: DOUBLE CHECK WHAT THE OUTPUT COLUMNS ARE EXACTLY
  # TODO: Add relative amplitude (either NA or figure out a way to calculate it)
  res_formatted <- data.frame(
    feature = df_original$feature,
    group = df_original$group,
    period_estimate = df_original$period,
    phase_estimate = df_original$phi,
    mesor_estimate = df_original$basal,
    amplitude_estimate = df_original$A,
    # relative_amplitude_estimate = rel_amp,
    pval = df_original$pvalue,
    pval_adj = p_adj,
    method = "RepeatedCircadian",
    hr_period = df_harm$period,
    hr_phase_estimate = df_harm$phase_estimate,
    hr_mesor_estimate = df_harm$mesor_estimate,
    hr_amplitude_estimate = df_harm$amplitude_estimate,
    hr_relative_amplitude_estiamte = df_harm$relative_amplitude_estimate
  )

  # Remove group column if added temporarily by check function at the start
  if (added_group == TRUE) {
    df_original$group <- NULL
    res_formatted$group <- NULL
  }

  return(list(res_original = df_original, res_formatted = res_formatted))
}
