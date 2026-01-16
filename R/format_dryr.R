#' Format dryR Results
#'
#' @param ls_res_groups A list with results from a dryR rhythmicity analysis,
#'   split into groups.
#' @param w_params A data frame with results from a harmonic regression
#'   analysis.
#' @param method Which method ("f_24" or "dryseq_single") was used in the
#'   analysis.
#' @param added_group If TRUE the "group" column will be removed
#'
#' @returns A list of data frames containing the original and formatted results
format_dryr <- function(ls_res_groups,
                        w_params,
                        method,
                        added_group) {
  if (method == "dryseq_single") {
    # Get "results" data frame from each method and concatenate them
    df_combined <- do.call(rbind, lapply(names(ls_res_groups), function(x) {
      df <- ls_res_groups[[x]]$results
      df$group <- x
      df$period <- ls_res_groups[[x]]$period
      df$feature <- row.names(ls_res_groups[[x]]$results)
      df[, c("feature", "pvalue", "padj", "Intercept", "phase", "amp", "group", "period")]
    }))

    # Create formatted results
    # TODO: FIGURE OUT SCALE OF INTERCEPT AND AMPLITUDE AND WHAT PHASE IS EXACTLY
    # TODO: FIGURE OUT SCALE OF INTERCEPT AND AMPLITUDE AND WHAT PHASE IS EXACTLY
    # TODO: FIGURE OUT SCALE OF INTERCEPT AND AMPLITUDE AND WHAT PHASE IS EXACTLY
    # TODO: FIGURE OUT SCALE OF INTERCEPT AND AMPLITUDE AND WHAT PHASE IS EXACTLY
    # TODO: FIGURE OUT SCALE OF INTERCEPT AND AMPLITUDE AND WHAT PHASE IS EXACTLY
    # TODO: FIGURE OUT SCALE OF INTERCEPT AND AMPLITUDE AND WHAT PHASE IS EXACTLY
    res_formatted <- data.frame(
      feature = df_combined$feature,
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
      df$feature <- row.names(ls_res_groups[[x]]$results)
      df
    }))

    # Create formatted results
    # TODO: Mention in documentation that amplitude output is divided by 2 since
    # they seem to define amplitude as peak to trough distance. Weirdly enough,
    # the relative amplitude seems to be mesor-to-peak-distance divided by
    # mesor, i.e. the "correct" way.
    res_formatted <- data.frame(
      feature = df_combined$feature,
      group = df_combined$group,
      period_estimate = df_combined$period,
      phase_estimate = df_combined$phase,
      mesor_estimate = df_combined$mean,
      amplitude_estimate = df_combined$amp / 2,
      relative_amplitude_estimate = df_combined$relamp,
      pval = df_combined$pval,
      pval_adj = df_combined$padj,
      method = "dryR(f_24)"
    )
  }

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
  res_original <- ls_res_groups
  if (added_group == TRUE) {
    res_original <- res_original[[1]]
    res_original$single <- NULL
    res_formatted$group <- NULL
  }

  return(list(res_original = res_original, res_formatted = res_formatted))
}
