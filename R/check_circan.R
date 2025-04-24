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

  # Extract meta data to add necessary columns
  df_meta_temp <- metadata(cd_local)

  # If the user has picked this method but there are no repeated measures, add a
  # subject ID column with a unique subject ID for each sample because CircaN
  # requires a column named "ind"
  if (cd_local$repeated_measures == FALSE) {
    df_meta_temp$ind <- paste0("S", 1:nrow(df_meta_temp))
  } else {
    df_meta_temp$ind <- df_meta_temp[[".subject_ID"]]
  }

  # Add required "time" and "sample" columns
  df_meta_temp$time <- df_meta_temp[[".time"]]
  df_meta_temp$sample <- rownames(df_meta_temp)

  # Add temporary group if there is no group column
  if (is.na(cd_local$n_groups)){
    df_meta_temp[[".group"]] <- "tmp"
  }

  # Add meta data back to CD object
  metadata(cd_local) <- df_meta_temp

  # Make sure samples are ordered by time and subject
  cd_local <- order_samples(cd_local, c("time", ".group", "ind"))

  return(cd_local)
}
