#' Format dryR Results
#'
#' @param ls_res_groups A list with results from a dryR rhythmicity analysis,
#'   split into groups.
#' @param ls_harm_groups A list with results from a harmonic regression
#'   analysis, split into groups.
#' @param method Which method ("f_24" or "dryseq_single") was used in the
#'   analysis.
#' @param added_group If TRUE the "group" column will be removed
#'
#' @returns A list of data frames containing the original and formatted results
format_dryr <- function(ls_res_groups,
                        ls_harm_groups,
                        method,
                        added_group) {
  # Get harmonic regression params
  res_harm <- do.call("rbind", ls_harm_groups)
  rownames(res_harm) <- NULL

  if (method == "dryseq_single") {
    # Get "results" data frame from each method and concatenate them
    df_combined <- do.call(rbind, lapply(names(ls_res_groups), function(x) {
      df <- ls_res_groups[[x]]$results
      df$group <- x
      df$period <- ls_res_groups[[x]]$period
      df
    }))

    # Create formatted results
    res_formatted <- data.frame(
      feature = row.names(df_combined),
      group = df_combined$group,
      period_estimate = df_combined$period,
      phase_estimate = df_combined$phase,
      mesor_estimate = df_combined$Intercept,
      amplitude_estimate = df_combined$amp,
      pval = df_combined$pvalue,
      pval_adj = df_combined$padj,
      method = "dryR(dryseq_single)"
    )

  } else if (method == "f_24") {
    # Get "parameters" data frame from each method and concatenate them
    df_combined <- do.call(rbind, lapply(names(ls_res_groups), function(x) {
      df <- as.data.frame(ls_res_groups[[x]]$parameters)
      df$group <- x
      df$period <- ls_res_groups[[x]]$period
      df
    }))

    # Create formatted results
    res_formatted <- data.frame(
      feature = row.names(df_combined),
      group = df_combined$group,
      period_estimate = df_combined$period,
      phase_estimate = df_combined$phase,
      mesor_estimate = df_combined$mean,
      amplitude_estimate = df_combined$amp,
      relative_amplitude_estimate = df_combined$relamp,
      pval = df_combined$pval,
      pval_adj = df_combined$padj,
      method = "dryR(f_24)"
    )
  }

  # Add harmonic regression results to output
  res_formatted <- data.frame(
    res_formatted,
    hr_period = res_harm$period,
    hr_phase_estimate = res_harm$phase_estimate,
    hr_mesor_estimate = res_harm$mesor_estimate,
    hr_amplitude_estimate = res_harm$amplitude_estimate,
    hr_relative_amplitude_estimate = res_harm$relative_amplitude_estimate
  )

  # Remove group column if added temporarily by check function at the start
  if (added_group == TRUE) {
    res_original <- ls_res_groups[[1]]
    res_original$single <- NULL
    res_formatted$group <- NULL
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
