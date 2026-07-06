#' JTK_CYCLE prep
#'
#' This function is used to check the number of non-NA values per feature in a `CircadianData` object.
#'
#' @param cd A `CircadianData` object
#'
#' @returns A data frame with the number of non-NA values per feature and the number
#'    of time points with at least one non-NA value.
count_valid <- function(cd) {
  # Extract data and meta data
  dat <- get_dataset(cd)
  meta <- get_metadata(cd)

  # Number of non-NA values per feature
  n_nonNA <- rowSums(!is.na(dat))

  # Number of time points with at least one non-NA value
  time_groups <- split(seq_len(ncol(dat)), meta$time)

  n_timepoints <- sapply(time_groups, function(cols) {
    rowSums(!is.na(dat[, cols, drop = FALSE])) > 0
  })

  n_timepoints <- rowSums(n_timepoints)

  # Combine into a data frame
  result <- data.frame(
    n_nonNA = n_nonNA,
    n_timepoints = n_timepoints,
    row.names = rownames(dat)
  )

  return(result)
}
