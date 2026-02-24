#' Lomb-Scargle CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with Lomb-Scargle.
#'
#' @param cd A `CircadianData` object
#'
#' @importFrom edgeR cpm
#'
#' @returns A `CircadianData` object
check_ls <- function(cd) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # Turn to logCPM values if we have count data
  if (cd_local$data_type == "count") {
    counts <- get_dataset(cd_local)
    logCPM <- edgeR::cpm(counts, log = TRUE)
    cd_local@dataset <- logCPM
  }

  # If we have repeated measures, remove subject batch effect
  if (cd_local$repeated_measures == TRUE) {
    cd_local <- remove_batch_effects(cd_local)
  }

  # Extract meta data to add necessary columns
  df_meta_temp <- get_metadata(cd_local)

  # Add temporary group if there is no group column
  if (is.na(cd_local$n_groups)) {
    df_meta_temp[["group"]] <- "tmp"
  }

  # Add meta data back to CD object
  metadata(cd_local) <- df_meta_temp

  # Make sure samples are ordered by time and group (and subject ID if relevant)
  sort_cols <- intersect(
    c("time", "group", "subject_ID"),
    colnames(df_meta_temp)
  )
  cd_local <- order_samples(cd_local, sort_cols)

  return(cd_local)
}
