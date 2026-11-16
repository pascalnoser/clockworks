#' Validate the period
#'
#' Checks that `period` is a single positive number, a sorted range
#' (minimum, maximum) of two positive numbers, or `NA` (no period). Throws an
#' error otherwise.
#'
#' @param period The period passed by the user.
#'
#' @returns `period`, invisibly.
check_period <- function(period) {
  # A single NA means that no period is defined
  if (is_no_period(period)) {
    return(invisible(period))
  }
  if (!is.numeric(period) || !(length(period) %in% c(1, 2))) {
    stop(
      "'period' must be a numeric vector of length 1 or 2.",
      call. = FALSE
    )
  }
  if (any(!is.finite(period))) {
    stop("'period' must not contain missing or infinite values.", call. = FALSE)
  }
  if (any(period <= 0)) {
    stop("'period' has to be greater than 0.", call. = FALSE)
  }
  if (length(period) == 2 && period[1] >= period[2]) {
    stop(
      "When using a range for 'period', the values must be sorted ",
      "(minimum, maximum) and different from each other.",
      call. = FALSE
    )
  }

  invisible(period)
}

#' Check if no period is defined
#'
#' @param period The period stored in the experiment info.
#'
#' @returns `TRUE` if `period` is a single `NA`, `FALSE` otherwise.
is_no_period <- function(period) {
  length(period) == 1 && is.na(period)
}

#' Use the mean period for methods that test a single period
#'
#' Some methods can only test one fixed period. If `period` of `cd` is a range,
#' this function warns the user and sets it to the mean of the range.
#'
#' @param cd A `CircadianData` object.
#' @param method The name of the method, used in the warning.
#'
#' @returns The `CircadianData` object with a single value as `period`.
use_mean_period <- function(cd, method) {
  if (length(cd$period) > 1) {
    per_mean <- mean(cd$period)
    warning(
      method,
      " can only test a single period, but 'period' is a range (",
      paste(cd$period, collapse = " - "),
      "). The mean of the range (",
      per_mean,
      ") will be used instead.",
      call. = FALSE
    )
    cd$period <- per_mean
  }

  return(cd)
}
