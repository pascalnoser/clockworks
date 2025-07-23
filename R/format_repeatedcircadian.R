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
  res_harm <- do.call("rbind", ls_harm_groups)
  rownames(res_harm) <- NULL

  # Turn into one data frame
  res_original <- do.call("rbind", ls_res_groups)
  rownames(res_original) <- NULL

  # Get adjusted p-values by group
  p_adj <- ave(
    res_original$pvalue,
    res_original$group,
    FUN = function(p)
      p.adjust(p, method = "BH")
  )

  # # Get relative amplitude. If data is log-transformed, calculate relative
  # # amplitude in linear scale
  # if (log_transformed == FALSE) {
  #   rel_amp <- res_original$A / res_original$basal
  # } else {
  #   log_amp <- res_original$A
  #   rel_amp <- compute_relative_amplitude(log_amp, log_base)
  # }

  # Create formatted results data frame
  # TODO: DOUBLE CHECK WHAT THE OUTPUT COLUMNS ARE EXACTLY
  # TODO: Add relative amplitude (either NA or figure out a way to calculate it)
  res_formatted <- data.frame(
    feature = res_original$feature,
    group = res_original$group,
    period_estimate = res_original$period,
    phase_estimate = res_original$phi,
    mesor_estimate = res_original$basal,
    amplitude_estimate = res_original$A,
    # relative_amplitude_estimate = rel_amp,
    pval = res_original$pvalue,
    pval_adj = p_adj,
    method = "RepeatedCircadian",
    hr_period = res_harm$period,
    hr_phase_estimate = res_harm$phase_estimate,
    hr_mesor_estimate = res_harm$mesor_estimate,
    hr_amplitude_estimate = res_harm$amplitude_estimate,
    hr_relative_amplitude_estimate = res_harm$relative_amplitude_estimate
  )

  # Remove group column if added temporarily by check function at the start
  if (added_group == TRUE) {
    res_original$group <- NULL
    res_formatted$group <- NULL
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
