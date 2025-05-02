#' #' Format <Method> Results
#' #'
#' #' @param ls_res_groups A list with results from a <Method> rhythmicity
#' #'   analysis, split into groups.
#' #' @param remove_group If TRUE the "group" column will be removed
#' #'
#' #' @returns A list of data frames containing the original and formatted results
#' format_<method> <- function(ls_res_groups, added_group) {
#'   # Turn into one data frame
#'   res_original <- do.call("rbind", ls_res_groups)
#'
#'   # Remove redundant row names
#'   rownames(res_original) <- NULL
#'
#'   # Create formatted results data frame
#'   res_formatted <- data.frame(
#'     feature = res_original$,
#'     group = res_original$group,
#'     amplitude_estimate = res_original$,
#'     amplitude_pval = res_original$,
#'     amplitude_qval = res_original$,
#'     phase_estimate = res_original$,
#'     phase_pval = res_original$,
#'     phase_qval = res_original$,
#'     period_estimate = res_original$,
#'     period_pval = res_original$,
#'     period_qval = res_original$,
#'     mesor_estimate = res_original$,
#'     pval = res_original$,
#'     qval = res_original$,
#'     method = "<Method>"
#'   )
#'
#'   # Remove group column if added temporarily by check function at the start
#'   if (added_group == TRUE) {
#'     res_original$group <- NULL
#'     res_formatted$group <- NULL
#'   }
#'
#'   return(list(res_original = res_original, res_formatted = res_formatted))
#' }
