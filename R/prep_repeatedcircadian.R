prep_repeatedcircadian <- function(cd, grp){
  # Filter CD object by group
  cd_filt <- filter_samples_by_value(cd, col = ".group", value = grp)

  # Get relevant parameters
  n_features <- nrow(cd_filt)
  tt <- metadata(cd_filt)[[".time"]]
  subj_id <-
}
