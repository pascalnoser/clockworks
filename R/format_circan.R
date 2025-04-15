format_circan <- function(res) {
  # TODO: Add group and mesor
  df_modified <- data.frame(
    feature = res$feature,
    amplitude_estimate = res$estimate.amp,
    amplitude_pval = res$p.value.amp,
    amplitude_qval = res$BH.q.value.amp,
    phase_estimate = res$estimate.phase,
    phase_pval = res$p.value.phase,
    phase_qval = res$BH.q.value.phase,
    period_estimate = res$estimate.per,
    period_pval = res$p.value.per,
    period_qval = res$BH.q.value.per,
    pval = res$combined_pval,
    qval = res$BH_combined,
    method = "CircaN"
  )

  return(df_modified)
}
