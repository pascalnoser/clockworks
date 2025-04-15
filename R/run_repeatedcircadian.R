#' Detect rhythmicity with RepeatedCircadian
#'
#' This function runs rhythmicity detection using RepeatedCircadian
#'
#' @param cd A `CircadianData` object.
#' @param ... Additional parameters passed to `RepeatedCircadian::rpt_rhythmicity()`
#'
#' @import RepeatedCircadian
#' @importFrom future.apply future_lapply
#'
#' @returns A data frame with the results of the RepeatedCircadian analysis.
#' @examples
#' data(cw_data)
#' data(cw_metadata)
#' cw_metadata <- clockworks:::check_metadata(
#'   cw_metadata,
#'   colname_sample = "Sample_ID",
#'   colname_time = "Time",
#'   colname_group = "Group",
#'   colname_subject = "Subject_ID"
#' )
#' cd <- CircadianData(cw_data,
#'                     cw_metadata,
#'                     experiment_info = list(
#'                       period = 24,
#'                       repeated_measures = TRUE,
#'                       n_groups = 2
#'                     ))
#' #results <- clockworks:::run_repeatedcircadian(cd)
#' #head(results)
run_repeatedcircadian <- function(cd, ...) {
  # Make sure samples are ordered by time and subject
  cd_sorted <- order_samples(cd, c(".time", ".subject_ID"))

  ### ----- ###
  # # Prepare data
  # df_prep <- prep_repeatedcircadian_long(cd_sorted)
  # df_prep[1:8, 1:8]
  #
  # # Now what?
  ### ----- ###

  # Get metadata
  df_metadata = metadata(cd_sorted)

  # Add temporary group if there are none
  if (!".group" %in% colnames(df_metadata)){
    df_metadata[[".group"]] <- "tmp"
    metadata(cd_sorted) <- df_metadata

    # Note that group needs to be removed from results in the end
    remove_group <- TRUE
  }

  # Get groups
  groups <- unique(df_metadata[[".group"]])

  # Create empty list for results
  ls_res = list()

  # Run rhythmicity detection for each group separately
  for (grp in unique(df_metadata$Group)) {
    # Prepare inputs
    ls_prep <- prep_repeatedcircadian(cd_sorted, grp)

    # Run rhythmicity detection for each feature separately
    ls_res_temp <- future_lapply()
  }




  ## BENCHMARK VERSION
  # Run rhythmicity detection for each group separately
  for (grp in unique(df_metadata$Group)) {
    # Filter data
    df_meta_filt = df_metadata[df_metadata[[".group"]] == grp]
    df_synth_filt = df_synth[, df_meta_filt$Sample_ID]

    # Get relevant parameters
    n_genes = nrow(df_synth_filt)
    tt = df_meta_filt$Time
    subj_ID = df_meta_filt$Subject_ID

    # Run rhythmicity detection for each gene, poarallelised to speed up
    ls_res_temp = mclapply(c(1:n_genes), function(gene) {
      yy = as.numeric(df_synth_filt[gene, ])
      unlist(rpt_rhythmicity(tt, yy, subj_ID, period = 24))
    }, mc.cores = n_cores)

    # Turn results list into data frame
    df_res_temp = as.data.frame(do.call(rbind, ls_res_temp))

    # Add Gene ID and group to results df
    df_res_temp = data.frame(Gene_ID = rownames(df_synth_filt), Group = grp, df_res_temp)

    # Add adjusted p-values
    df_res_temp = mutate(df_res_temp, p_adj = p.adjust(pvalue, method = 'BH'), .after = pvalue)

    # Add to list
    ls_res_synth[[grp]] = df_res_temp
  }

  # Turn results into data frame
  df_res_synth = bind_rows(ls_res_synth)

  # Remove 'group' column if only added temporarily at the start
  if (remove_group == TRUE) {
    df_results_original$group = NULL
  }

  # Postprocessing
  df_results_modified <- postprocess_repeatedcircadian(df_results_original)


  return(list(df_results_original = df_results_original, df_results_modified = df_results_modified))
}
