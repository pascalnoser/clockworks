#' Warn if a method tests other periods than requested
#'
#' Some methods can only test periods on a grid that depends on the sampling
#' design (e.g. whole multiples of the sampling interval). This function warns
#' the user if the periods that are actually tested differ from the requested
#' single period or fall outside the requested period range.
#'
#' @param method The name of the method, used in the warning.
#' @param requested The requested period, either a single value or a range
#'   `c(min, max)`.
#' @param tested Numeric vector of the periods that are actually tested.
#' @param rule A string describing which periods the method can test, e.g.
#'   `"whole multiples of the sampling interval (here 5)"`.
#' @param note A string appended to the warning, e.g. to say what is reported
#'   as `period_estimate`.
#' @param tol Relative tolerance for comparing periods.
#'
#' @returns `TRUE` (invisibly) if a warning was given, `FALSE` otherwise.
warn_tested_periods <- function(
  method,
  requested,
  tested,
  rule,
  note = "The period with the best fit is reported as 'period_estimate'.",
  tol = 1e-6
) {
  tested <- tested[!is.na(tested)]
  if (length(tested) == 0 || all(is.na(requested))) {
    return(invisible(FALSE))
  }
  lo <- min(requested)
  hi <- max(requested)
  eps <- tol * hi

  if (abs(hi - lo) <= eps) {
    mismatch <- any(abs(tested - lo) > eps)
    req_str <- format(signif(lo, 4))
  } else {
    mismatch <- any(tested < lo - eps | tested > hi + eps)
    req_str <- paste0("the range ", signif(lo, 4), " - ", signif(hi, 4))
  }
  if (!mismatch) {
    return(invisible(FALSE))
  }

  tested_str <- paste(signif(sort(tested), 4), collapse = ", ")
  tested_str <- if (length(tested) > 1) {
    paste0("the periods ", tested_str, " are")
  } else {
    paste0("the period ", tested_str, " is")
  }
  warning(
    method, " can only test periods that are ", rule, ". Instead of ",
    req_str, ", ", tested_str, " tested. ", note,
    call. = FALSE
  )
  invisible(TRUE)
}

#' Show each warning only once
#'
#' Evaluates `expr` and collects its warnings. Each distinct warning is given
#' again once at the end, so that a loop over groups with the same design does
#' not repeat the same warning for every group.
#'
#' @param expr An expression to evaluate.
#'
#' @returns The value of `expr`.
warn_once <- function(expr) {
  msgs <- character(0)
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  for (msg in unique(msgs)) {
    warning(msg, call. = FALSE)
  }
  value
}

#' Periods tested by JTK_CYCLE
#'
#' Reproduces how `MetaCycle:::runJTK()` turns `minper` and `maxper` into the
#' periods (in whole numbers of sampling intervals) that are tested.
#'
#' @param timepoints Numeric vector with the time of each sample.
#' @param minper,maxper The requested minimum and maximum period.
#'
#' @seealso [warn_jtk_periods()]
#'
#' @returns A numeric vector of the tested periods, with the sampling interval
#'   used by MetaCycle as attribute `"interval"`. `NULL` if MetaCycle cannot
#'   test any period (it gives an error in this case).
jtk_tested_periods <- function(timepoints, minper, maxper) {
  uni_time <- sort(unique(timepoints))
  if (length(uni_time) < 2) {
    return(NULL)
  }
  freq <- uni_time[2] - uni_time[1]
  data_endtime <- length(uni_time) * freq
  upper <- if (data_endtime >= maxper) maxper else data_endtime
  if (upper < minper || round(upper / freq) < 2) {
    return(NULL)
  }
  lower <- max(round(minper / freq), 2)
  tested <- seq(lower, round(upper / freq)) * freq
  attr(tested, "interval") <- freq
  tested
}

#' Warn if JTK tests other periods than requested
#'
#' Used by all methods that run JTK through MetaCycle (JTK_CYCLE, meta2d,
#' meta3d).
#'
#' @param method The name of the method, used in the warning.
#' @param timepoints Numeric vector with the time of each sample.
#' @param minper,maxper The requested minimum and maximum period.
#' @param note A string appended to the warning.
#'
#' @returns `TRUE` (invisibly) if a warning was given, `FALSE` otherwise.
warn_jtk_periods <- function(
  method,
  timepoints,
  minper,
  maxper,
  note = "The period with the best fit is reported as 'period_estimate'."
) {
  tested <- jtk_tested_periods(timepoints, minper, maxper)
  warn_tested_periods(
    method,
    requested = c(minper, maxper),
    tested = tested,
    rule = paste0(
      "whole multiples of the sampling interval (here ",
      attr(tested, "interval"),
      ") and no longer than the duration of the time series"
    ),
    note = note
  )
}
