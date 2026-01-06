#' CircaN CircadianData check
#'
#' This function checks whether a `CircadianData` object contains all the meta
#' data columns necessary to run rhythmicity detection with `CircaN`.
#'
#' @param cd A `CircadianData` object
#'
#' @returns A `CircadianData` object
check_circan <- function(cd) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # Normalise if count data
  if (cd_local$data_type == "count") {
    cd_local <- normalise_dataset(cd_local)
  }

  # Extract meta data to add necessary columns
  df_meta_temp <- get_metadata(cd_local)

  # If the user has picked this method but there are no repeated measures, add a
  # subject ID column with a unique subject ID for each sample because CircaN
  # requires a column named "ind". If there are subject IDs already, replace
  # them by unique numerical identifiers (required by CircaN)
  if (cd_local$repeated_measures == FALSE) {
    df_meta_temp$ind <- 1:nrow(df_meta_temp)
  } else {
    ids_orig <- df_meta_temp[["subject_ID"]]
    df_meta_temp$ind <- as.numeric(factor(ids_orig))
  }

  # Add required "time" and "sample" columns
  df_meta_temp$time <- df_meta_temp[["time"]]
  df_meta_temp$sample <- rownames(df_meta_temp)

  # Add temporary group if there is no group column
  if (is.na(cd_local$n_groups)){
    df_meta_temp[["group"]] <- "tmp"
  }

  # Add meta data back to CD object
  metadata(cd_local) <- df_meta_temp

  # Make sure samples are ordered by time and subject
  cd_local <- order_samples(cd_local, c("time", "group", "ind"))

  return(cd_local)
}
