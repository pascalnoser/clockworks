#' Add experiment information to CircadianData object
#'
#' @param cd A `CircadianData` object.
#' @param period A number defining the period length of the circadian data.
#' @param type Type of data in `dataset` slot of `CircadianData` object. Must be
#'   one of "count" (for data following a negative bionmial distribution) or
#'   "norm" for data roughly following a normal distribution (e.g. log-CPM
#'   values).
#' @param estimate_delta_t If TRUE, sampling interval will be estimated from
#'   meta data.
#'
#' @returns The `CircadianData` object with added experiment info
add_experiment_info <- function(cd, period = NULL, type = NULL, estimate_delta_t = TRUE) {
  # Create local copy of cd to prevent accidental changes to main object
  cd_local <- cd

  # Get meta data column names
  meta_cnames <- colnames(metadata(cd_local))

  # Add period ----
  if (!is.null(period)) cd_local$period <- period

  # Add data type ----
  # TODO: ADD CHECK SOMEWHERE THAT type IS A VALID VALUE. Maybe `match.arg()`?
  if (!is.null(period)) cd_local$type <- type

  # Add group info ----
  # TODO: Remove "n_groups" and replace with "groups"?
  groups <- unique(metadata(cd_local)[["group"]])
  n_groups <- length(groups)

  if ("group" %in% meta_cnames) {
    cd_local$n_groups <- n_groups
    # cd_local$groups <- groups
  } else {
    cd_local$n_groups <- NA
    # cd_local$groups <- NA
  }


  # Add repeated measures info ----
  if ("subject_ID" %in% meta_cnames) {
    cd_local$repeated_measures <- TRUE
  } else {
    cd_local$repeated_measures <- FALSE
  }


  # Add replicate info ----
  # Make sure to show all time points in every group
  t_unique <- sort(unique(metadata(cd_local)[["time"]]))

  if (is.na(cd_local$n_groups)) {
    cd_local$n_replicates <- table(factor(metadata(cd_local)[["time"]], levels = t_unique))
  } else {
    grp_split <- split(metadata(cd_local), metadata(cd_local)[["group"]])
    cd_local$n_replicates <- lapply(grp_split, function(x) {
      table(factor(x[["time"]], levels = t_unique))
    })
  }


  # Add sampling interval ----
  if (estimate_delta_t) {
    # Sort by group and time
    sort_cols <- intersect(c("group", "time"), colnames(metadata(cd_local)))
    cd_sorted <- order_samples(cd_local, by_columns = sort_cols)

    # Extract time differences
    delta_ts <- diff(sort(unique(metadata(cd_sorted)[["time"]])))
    delta_freqs <- sort(table(delta_ts), decreasing = TRUE)
    delta_t_unique <- as.numeric(names(delta_freqs))

    # Add to CD object
    if (length(delta_t_unique) == 1) {
      cd_local$delta_t <- delta_t_unique[1]
    } else {
      # Get most common delta_t
      most_common <- delta_t_unique[1]
      other_deltas <- delta_t_unique[-1]

      # If less common delta_t's are multiples of most common, we likely have a
      # constant delta_t but missing values
      if (all(other_deltas %% most_common == 0)) {
        cd_local$delta_t <- most_common
      } else {
        cd_local$delta_t <- NA
        message(
          "WARNING: Unable to determine a regular sampling interval. Proceeding ",
          "with the analysis under the assumption of irregular sampling."
        )
      }
    }
  }


  # Add number of cycles ----
  dt <- cd_local$delta_t

  # No groups
  if (is.na(cd_local$n_groups) && !(is.na(dt) || is.null(dt))) {
    t_min <- min(metadata(cd_local)[["time"]])
    t_max <- max(metadata(cd_local)[["time"]])
    cd_local$n_cycles <- ((t_max + dt) - t_min) / mean(cd_local$period)
  }

  # Group-wise
  if (!is.na(cd_local$n_groups) && !(is.na(dt) || is.null(dt))) {
    grp_split <- split(metadata(cd_local), metadata(cd_local)[["group"]])

    cd_local$n_cycles <- lapply(grp_split, function(x) {
      t_min <- min(x[["time"]])
      t_max <- max(x[["time"]])
      ((t_max + dt) - t_min) / mean(cd_local$period)
    })
  }

  # Sampling interval NA or NULL
  if (is.na(dt) || is.null(dt)) {
    cd_local$n_cycles <- NA
  }


  return(cd_local)
}
