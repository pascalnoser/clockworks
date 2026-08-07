#' Safe Spearman rank correlation
#'
#' A drop-in replacement for [GeneCycle::robust.g.test()]'s internal
#' `spearman()` function. The original uses [stats::cor()] with
#' `method = "spearman"`, which returns `NA` with a warning when either input
#' vector is constant (zero standard deviation). This can happen when tied
#' expression values in a permuted time series align at a specific lag during
#' the rank-based spectrum estimation, eventually causing [stats::density()] to
#' fail downstream. This version returns `0` for those cases instead, which is
#' the statistically appropriate choice: a constant vector has no rank
#' correlation with anything.
#'
#' @param x,y Numeric vectors of equal length.
#' @param N Integer. Full time series length (used as a normalisation factor).
#' @param version One of `"builtin"` (default) or `"miika"`. Passed through to
#'   the original `spearman()` logic for the non-degenerate case.
#'
#' @return A single numeric value: the scaled Spearman correlation, or `0` if
#'   either `x` or `y` is constant.
#'
#' @noRd
safe_spearman <- function(x, y, N, version = c("builtin", "miika")) {
  version <- match.arg(version)
  if (sd(x) == 0 || sd(y) == 0) {
    return(0)
  }
  cor(x, y, method = "spearman") * length(x) / N
}
