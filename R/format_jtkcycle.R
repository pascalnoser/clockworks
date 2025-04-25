#' Format JTK_CYCLE Results
#'
#' @param ls_res_groups A list with results from a JTK_CYCLE rhythmicity
#'   analysis, split into groups.
#' @param remove_group If TRUE the "group" column will be removed
#'
#' @returns A list of data frames containing the original and formatted results
format_jtkcycle <- function(ls_res_groups, remove_group) {
  # Turn list of lists into one list with one data frame per method
  method_names <- c("ARS", "JTK", "LS", "meta")
  res_original <- lapply(method_names, function(x) {
    dfs <- lapply(ls_res_groups, `[[`, x)
    df_combined <- do.call(rbind, dfs)
    rownames(df_combined) = NULL
    return(df_combined)
  })

  names(res_original) <- method_names

  # Create formatted results data frame
  df_meta <- res_original$meta
  res_formatted <- data.frame(
    feature = df_meta$CycID,
    group = df_meta$group,
    amplitude_estimate = df_meta$JTK_amplitude,  # TODO: Use JTK_amplitude or meta2d_AMP?
    amplitude_pval = NA,
    amplitude_qval = NA,
    phase_estimate = df_meta$JTK_adjphase,
    phase_pval = NA,
    phase_qval = NA,
    period_estimate = df_meta$JTK_period,
    period_pval = NA,
    period_qval = NA,
    mesor_estimate = df_meta$meta2d_Base,
    relative_amplitude_estimate = df_meta$meta2d_rAMP,
    pval = df_meta$JTK_pvalue,
    qval = df_meta$JTK_BH.Q,
    method = "JTK_CYCLE"
  )

  # Remove group column if added temporarily by check function at the start
  if (remove_group == TRUE) {
    res_formatted$group <- NULL
    res_original <- lapply(res_original, function(df){
      df$group <- NULL
      return(x)
    })
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
