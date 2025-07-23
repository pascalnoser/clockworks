#' Format ARSER Results
#'
#' @param ls_res_groups A list with results from a ARSER rhythmicity analysis,
#'   split into groups.
#' @param ls_harm_groups A list with results from a harmonic regression
#'   analysis, split into groups.
#' @param added_group If TRUE the "group" column will be removed
#' @param log_transformed Logical, if the data is log-transformed
#' @param log_base Logarithmic base of data. Only relevant if `log_transformed`
#'   is `TRUE`.
#'
#' @returns A list of data frames containing the original and formatted results
format_arser <- function(ls_res_groups,
                         ls_harm_groups,
                         added_group,
                         log_transformed = FALSE,
                         log_base = 2) {
  # TODO: Probably remove `log_transformed` and `log_base` parameters since we
  # will likely only report the relative amplitude from the harmonic regression.
  # Alternatively, uncomment the relative amplitude calculation below.

  # TODO: Note in the documentation that sometimes the phase estimates might
  # differ between a method and the results of the harmonic regression (e. g.
  # ARSER fits a cosine wave, whereas I adjusted the HarmonicRegression output
  # to give the phase of a sine wave).

  # Get harmonic regression params
  res_harm <- do.call("rbind", ls_harm_groups)
  rownames(res_harm) <- NULL

  # Turn list of lists into one list with one data frame per method
  method_names <- c("ARS", "meta")
  res_original <- lapply(method_names, function(x) {
    dfs <- lapply(ls_res_groups, `[[`, x)
    df_combined <- do.call(rbind, dfs)
    rownames(df_combined) = NULL
    return(df_combined)
  })
  names(res_original) <- method_names

  # Get meta and ARS results
  df_meta <- res_original$meta
  df_ars <- res_original$ARS

  # # Get relative amplitude
  # amp <- as.numeric(df_ars$amplitude)
  # mesor <- df_ars$mean
  # if (log_transformed == FALSE) {
  #   rel_amp <- amp / mesor
  # } else {
  #   rel_amp <- compute_relative_amplitude(amp, log_base)
  # }

  # Create formatted results data frame
  res_formatted <- data.frame(
    feature = df_ars$CycID,
    group = df_ars$group,
    period_estimate = as.numeric(df_ars$period),
    phase_estimate = as.numeric(df_ars$phase), # Identical to ARS_adjphase from df_meta
    mesor_estimate = df_ars$mean,
    amplitude_estimate = as.numeric(df_ars$amplitude),
    # relative_amplitude_estimate = rel_amp,
    pval = df_ars$pvalue,
    pval_adj = df_ars$fdr_BH,
    method = "ARSER",
    hr_period = res_harm$period,
    hr_phase_estimate = res_harm$phase_estimate,
    hr_mesor_estimate = res_harm$mesor_estimate,
    hr_amplitude_estimate = res_harm$amplitude_estimate,
    hr_relative_amplitude_estimate = res_harm$relative_amplitude_estimate
  )

  # Remove group column if added temporarily by check function at the start
  if (added_group == TRUE) {
    res_formatted$group <- NULL
    res_original <- lapply(res_original, function(df){
      df$group <- NULL
      return(df)
    })
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
