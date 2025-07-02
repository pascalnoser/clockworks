#' Format Lomb-Scargle Results
#'
#' @param ls_res_groups A list with results from a Lomb-Scargle rhythmicity
#'   analysis, split into groups.
#' @param added_group If TRUE the "group" column will be removed
#' @param log_transformed Logical, if the data is log-transformed
#' @param log_base Logarithmic base of data. Only relevant if `log_transformed`
#'   is `TRUE`.
#'
#' @returns A list of data frames containing the original and formatted results
format_ls <- function(ls_res_groups, added_group, log_transformed, log_base) {
  # Turn list of lists into one list with one data frame per method
  method_names <- c("LS", "meta")
  res_original <- lapply(method_names, function(x) {
    dfs <- lapply(ls_res_groups, `[[`, x)
    df_combined <- do.call(rbind, dfs)
    rownames(df_combined) = NULL
    return(df_combined)
  })

  names(res_original) <- method_names

  # Get relative amplitude. If data is log-transformed, calculate relative
  # amplitude in linear scale
  df_meta <- res_original$meta
  if (log_transformed == FALSE) {
    rel_amp <- df_meta$meta2d_rAMP
  } else {
    log_amp <- df_meta$meta2d_AMP
    rel_amp <- compute_relative_amplitude(log_amp, log_base)
  }

  # Create formatted results data frame
  res_formatted <- data.frame(
    feature = df_meta$CycID,
    group = df_meta$group,
    period_estimate = df_meta$LS_period,
    phase_estimate = df_meta$LS_adjphase,
    mesor_estimate = df_meta$meta2d_Base,
    amplitude_estimate = df_meta$meta2d_AMP,  # Note: I think the "LS_amplitude" is actually a mesor?
    relative_amplitude_estimate = rel_amp,
    pval = df_meta$LS_pvalue,
    pval_adj = df_meta$LS_BH.Q,
    method = "Lomb-Scargle"
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
