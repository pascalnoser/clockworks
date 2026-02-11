#' Format JTK_CYCLE Results
#'
#' @param ls_res_groups A list with results from a JTK_CYCLE rhythmicity
#'   analysis, split into groups.
#' @param w_params A data frame with results from a harmonic regression
#'   analysis.
#' @param added_group If TRUE the "group" column will be removed
#' @param t_min Minimum value in time column, i.e. starting point of time-series
#' @param log_transformed Logical, if the data is log-transformed
#' @param log_base Logarithmic base of data. Only relevant if `log_transformed`
#'   is `TRUE`.
#'
#' @returns A list of data frames containing the original and formatted results
format_jtkcycle <- function(
  ls_res_groups,
  w_params,
  added_group,
  t_min,
  log_transformed = FALSE,
  log_base = 2
) {
  # TODO: Probably remove `log_transformed` and `log_base` parameters since we
  # will likely only report the relative amplitude from the harmonic regression.
  # Alternatively, uncomment the relative amplitude calculation below.

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
    phase_estimate = df_meta$JTK_adjphase, # TODO: Mention in documentation that we use the adjusted phase
    # peak_time_estimate = df_jtk$LAG + t_min,
    # mesor_estimate = df_meta$meta2d_Base,
    amplitude_estimate = df_meta$JTK_amplitude,
    # relative_amplitude_estimate = rel_amp,
    pval = df_meta$JTK_pvalue,
    pval_adj = df_meta$JTK_BH.Q,
    method = "JTK_CYCLE"
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

  # Remove group column if added temporarily by check function at the start
  if (added_group == TRUE) {
    res_formatted$group <- NULL
    res_original <- lapply(res_original, function(df) {
      df$group <- NULL
      return(df)
    })
  }

  # Set period estimate to NA if it is 0
  res_formatted$period_estimate[res_formatted$period_estimate == 0] <- NA

  return(list(res_original = res_original, res_formatted = res_formatted))
}
