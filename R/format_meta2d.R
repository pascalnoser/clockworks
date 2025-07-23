#' Format meta2d Results
#'
#' @param ls_res_groups A list with results from a meta2d rhythmicity
#'   analysis, split into groups.
#' @param ls_harm_groups A list with results from a harmonic regression
#'   analysis, split into groups.
#' @param added_group If TRUE the "group" column will be removed
#' @param log_transformed Logical, if the data is log-transformed
#' @param log_base Logarithmic base of data. Only relevant if `log_transformed`
#'   is `TRUE`.
#'
#' @returns A list of data frames containing the original and formatted results
format_meta2d <- function(ls_res_groups,
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

  # Turn list of lists into one list with one data frame per method
  method_names <- c("ARS", "LS", "JTK", "meta")
  res_original <- lapply(method_names, function(x) {
    dfs <- lapply(ls_res_groups, `[[`, x)
    df_combined <- do.call(rbind, dfs)
    rownames(df_combined) = NULL
    return(df_combined)
  })
  names(res_original) <- method_names

  # Get meta results
  df_meta <- res_original$meta

  # # Get relative amplitude. If data is log-transformed, calculate relative
  # # amplitude in linear scale
  # df_meta <- res_original$meta
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
    period_estimate = df_meta$meta2d_period,
    phase_estimate = df_meta$meta2d_phase,
    mesor_estimate = df_meta$meta2d_Base,
    amplitude_estimate = df_meta$meta2d_AMP,
    relative_amplitude_estimate = df_meta$meta2d_rAMP,
    pval = df_meta$meta2d_pvalue,
    pval_adj = df_meta$meta2d_BH.Q,
    method = "meta2d",
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
