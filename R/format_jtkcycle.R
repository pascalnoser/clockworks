#' Format JTK_CYCLE Results
#'
#' @param ls_res_groups A list with results from a JTK_CYCLE rhythmicity
#'   analysis, split into groups.
#' @param ls_harm_groups A list with results from a harmonic regression
#'   analysis, split into groups.
#' @param added_group If TRUE the "group" column will be removed
#' @param log_transformed Logical, if the data is log-transformed
#' @param log_base Logarithmic base of data. Only relevant if `log_transformed`
#'   is `TRUE`.
#'
#' @returns A list of data frames containing the original and formatted results
format_jtkcycle <- function(ls_res_groups,
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

  # Turn list of lists into one list with one data frame per method
  method_names <- c("JTK", "meta")
  res_original <- lapply(method_names, function(x) {
    dfs <- lapply(ls_res_groups, `[[`, x)
    df_combined <- do.call(rbind, dfs)
    rownames(df_combined) = NULL
    return(df_combined)
  })
  names(res_original) <- method_names

  # Get meta and JTK results
  df_meta <- res_original$meta
  df_jtk <- res_original$JTK

  # # Get relative amplitude. If data is log-transformed, calculate relative
  # # amplitude in linear scale
  df_meta <- res_original$meta
  # if (log_transformed == FALSE) {
  #   rel_amp <- df_meta$meta2d_rAMP
  # } else {
  #   log_amp <- df_meta$meta2d_AMP
  #   rel_amp <- compute_relative_amplitude(log_amp, log_base)
  # }

  # Create formatted results data frame
  res_formatted <- data.frame(
    feature = df_meta$CycID,
    group = df_meta$group,
    period_estimate = df_meta$JTK_period,
    phase_estimate = df_meta$JTK_adjphase,  # TODO: Mention in documentation that we the adjusted phase
    # mesor_estimate = df_meta$meta2d_Base,
    amplitude_estimate = df_meta$JTK_amplitude,
    # relative_amplitude_estimate = rel_amp,
    pval = df_meta$JTK_pvalue,
    pval_adj = df_meta$JTK_BH.Q,
    method = "JTK_CYCLE",
    hr_period = df_harm$period,
    hr_phase_estimate = df_harm$phase_estimate,
    hr_mesor_estimate = df_harm$mesor_estimate,
    hr_amplitude_estimate = df_harm$amplitude_estimate,
    hr_relative_amplitude_estiamte = df_harm$relative_amplitude_estimate
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
