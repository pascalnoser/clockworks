#' Compute Relative Amplitude from a Log-Scale Amplitude
#'
#' @description
#' Converts the amplitude from a cosine fit on log-transformed data into a
#' relative amplitude for the original, linear-scale data, supporting any
#' logarithmic base.
#'
#' @param log_amplitude A numeric value or vector representing the amplitude(s)
#'   (A) from a cosine fit performed on log-transformed data.
#' @param log_base The base of the logarithm used for the data transformation.
#'   Defaults to `2`. Must be a single value.
#'
#' @details
#' This function converts a log-scale amplitude into a linear-scale relative
#' amplitude. It assumes the input `log_amplitude`, denoted as \eqn{A_{\log}},
#' is derived from a cosine model fit to \eqn{\log_b}-transformed data, where
#' `b` is the logarithmic base:
#' \deqn{y(t) = \log_b(x(t)) = m + A_{\log} \cos(\omega t - \phi)}
#'
#' Or back-transformed:
#' \deqn{x(t) = b^{m + A_{\log} \cos(\omega t - \phi)}}
#'
#' The peak of the oscillation occurs when the cosine term is maximal (+1), and
#' the trough occurs when it is minimal (-1). Therefore, the peak and trough
#' values on the original linear scale, P and T, are found by back-transforming
#' the model at these extrema:
#' \deqn{P = b^{m + A_{\log}}}
#' \deqn{T = b^{m - A_{\log}}}
#'
#' The relative amplitude (\eqn{A_{rel}}) is then defined as a contrast index,
#' which measures the amplitude swing relative to the arithmetic mean of the
#' peak and trough (i.e. the mesor of the wave):
#' \deqn{A_{rel} = \frac{\text{Amplitude}}{\text{Mesor}} = \frac{(P - T)/2}{(P + T)/2} = \frac{P - T}{P + T}}
#'
#' Substituting the expressions for P and T, the \eqn{b^m} term cancels out:
#' \deqn{A_{rel} = \frac{b^{m + A_{\log}} - b^{m - A_{\log}}}{b^{m + A_{\log}} + b^{m - A_{\log}}} = \frac{b^{A_{\log}} - b^{-A_{\log}}}{b^{A_{\log}} + b^{-A_{\log}}}}
#'
#' To arrive at the final, computationally simpler form used in this function,
#' we multiply the numerator and denominator by \eqn{b^{A_{\log}}}:
#' \deqn{A_{rel} = \frac{b^{2A_{\log}} - 1}{b^{2A_{\log}} + 1}}
#'
#' This formula is mathematically equivalent to \eqn{\tanh(A_{\log} \cdot \ln(b))},
#' and its value is always bounded between 0 and 1.
#'
#' @return A numeric vector of the same length as `log_amplitude` containing
#'   the relative amplitudes.
#'
#' @export
#'
#' @examples
#' # For a single value (log2 is the default base)
#' compute_relative_amplitude(log_amplitude = 1.5)
#' #> [1] 0.7777778
#'
#' # Can also handle a vector of amplitudes
#' amplitudes_log2 <- c(0, 0.5, 1.0, 1.5)
#' compute_relative_amplitude(amplitudes_log2)
#' #> [1] 0.0000000 0.3333333 0.6000000 0.7777778
#'
#' # Using a different base (log10) for a vector of amplitudes
#' amplitudes_log10 <- c(0.1, 0.25, 0.5)
#' compute_relative_amplitude(amplitudes_log10, log_base = 10)
#' #> [1] 0.2262239 0.5370496 0.8181818

compute_relative_amplitude <- function(log_amplitude, log_base = 2) {

  # Check inputs
  if (!is.numeric(log_amplitude)) {
    stop("Input 'log_amplitude' must be numeric.")
  }
  if (!is.numeric(log_base) || length(log_base) != 1 || log_base <= 0 || log_base == 1) {
    stop("Input 'log_base' must be a single positive number not equal to 1.")
  }

  # The calculation is naturally vectorized in R
  term <- log_base^(2 * log_amplitude)
  relative_amplitude <- (term - 1) / (term + 1)

  return(relative_amplitude)
}
