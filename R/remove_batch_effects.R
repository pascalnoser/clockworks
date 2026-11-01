#' Remove batch effect from repeated measures
#'
#' Removes subject-level batch effects from a CircadianData object using
#' limma's removeBatchEffect function. Handles both single and multiple group designs.
#'
#' @param cd A `CircadianData` object with repeated measures
#' @param verbose Logical, whether a message should be printed
#'
#' @importFrom limma removeBatchEffect
#'
#' @returns A `CircadianData` object with batch effects removed from the dataset
#'
remove_batch_effects <- function(cd, verbose = TRUE) {
  # Check if cd object contains repeated measures
  if (!experiment_info(cd)$repeated_measures) {
    stop(
      "Cannot remove batch effects because no repeated measures (subject_ID) found in metadata.",
      call. = FALSE
    )
  }

  # If there is only one subject, we cannot remove batch effects, so return the original object
  if (length(unique(get_metadata(cd)$subject_ID)) == 1) {
    warning(
      "Not removing batch effects because only one subject found in metadata."
    )
    return(cd)
  }

  if (verbose == TRUE) {
    message("\nRemoving subject batch effects")
  }

  dat <- get_dataset(cd)
  mdata <- get_metadata(cd)
  if ("group" %in% colnames(mdata)) {
    grp <- as.character(mdata$group)
  } else {
    grp <- rep("tmp", nrow(mdata))
  }

  # If every subject belongs to only one group (subjects nested in groups),
  # remove subject effects within each group separately. Fitting all groups
  # jointly would make one subject coefficient non-estimable (subjects and
  # groups are confounded), and setting it to zero shifts the group means.
  n_grp_per_subject <- tapply(grp, mdata$subject_ID, function(x) length(unique(x)))
  if (all(n_grp_per_subject == 1)) {
    idx_list <- split(seq_len(ncol(dat)), grp)
  } else {
    idx_list <- list(seq_len(ncol(dat)))
  }

  out <- character(0)
  for (idx in idx_list) {
    # Drop subjects from other groups (unused factor levels)
    subject <- droplevels(factor(mdata$subject_ID[idx]))

    # Nothing to remove in a group with only one subject
    if (nlevels(subject) < 2) {
      next
    }
    out <- c(out, utils::capture.output(
      dat[, idx] <- limma::removeBatchEffect(
        x = dat[, idx, drop = FALSE],
        batch = subject,
        design = batch_effect_design(
          time = mdata$time[idx],
          group = grp[idx],
          subject = subject,
          period = mean(cd$period)
        )
      )
    ))
  }

  if (verbose == TRUE) {
    cat(out, sep = "\n")
  }

  dataset(cd) <- dat
  return(cd)
}


#' Design matrix of the signal to keep when removing subject batch effects
#'
#' Without a design, `limma::removeBatchEffect()` estimates each subject effect
#' as that subject's mean. If a subject is not observed at all time points
#' (missing values or unbalanced sampling), its mean also contains part of the
#' rhythm, which is then removed together with the subject effect. To prevent
#' this, the subject effects are estimated together with one mean per time point
#' (and group). This does not assume any wave shape. If the time point means and
#' subject effects cannot be estimated together (e.g. because subjects share no
#' time points), a harmonic regression design (cosine and sine at the mean
#' period) is used instead.
#'
#' @param time Numeric vector of sample times
#' @param group Vector of sample groups
#' @param subject Vector of subject IDs
#' @param period Period used for the fallback harmonic regression design
#'
#' @returns A design matrix
batch_effect_design <- function(time, group, subject, period) {
  # Subject columns as constructed by `limma::removeBatchEffect()`
  batch <- droplevels(factor(subject))
  stats::contrasts(batch) <- stats::contr.sum(levels(batch))
  x_batch <- stats::model.matrix(~batch)[, -1, drop = FALSE]
  is_estimable <- function(design) {
    X <- cbind(design, x_batch)
    qr(X)$rank == ncol(X)
  }

  # One mean per time point (and group)
  cells <- interaction(factor(group), factor(time), drop = TRUE)
  if (nlevels(cells) > 1) {
    design <- stats::model.matrix(~cells)
  } else {
    design <- matrix(1, nrow = length(time), ncol = 1)
  }
  if (is_estimable(design)) {
    return(design)
  }

  # Fallback: harmonic regression (per group if there are several groups)
  cos_t <- cos(2 * pi * time / period)
  sin_t <- sin(2 * pi * time / period)
  if (length(unique(group)) > 1) {
    design <- stats::model.matrix(~ factor(group) * (cos_t + sin_t))
  } else {
    design <- stats::model.matrix(~ cos_t + sin_t)
  }
  if (!is_estimable(design)) {
    warning(
      "Subject effects are confounded with time and cannot be separated ",
      "from the rhythm. They are removed without accounting for time.",
      call. = FALSE
    )
    if (length(unique(group)) > 1) {
      design <- stats::model.matrix(~ factor(group))
    } else {
      design <- matrix(1, nrow = length(time), ncol = 1)
    }
  }

  return(design)
}
