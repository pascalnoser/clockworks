#' Harmonic regression
#'
#' This function is used to get an estimate for the amplitude and phase using
#' harmonic regression.
#'
#' @param cd A `CircadianData` object
#' @param grp A string specifying a value in the "group" column of the metadata
#'   slot of `cd` which is used for filtering.
#'
#' @import HarmonicRegression
#'
#' @details
#' The returned parameters correspond to the model \deqn{y = M + A
#' sin(\frac{2\pi}{p} (t - \phi))} With \eqn{M} the mesor, \eqn{A} the
#' amplitude, \eqn{p} the period, \eqn{t} the time and \eqn{\phi} the phase in
#' the same units as \eqn{t} (e.g. hours).
#' EXPLAIN STUFF ABOUT RELATIVE AMPLITUDE!
#'
#'
#' @returns A data frame with the results of a harmonic regression.
estimate_wave_params <- function(cd, grp = NA) {
  # Filter cd object to only include the current group
  if (!is.na(grp)) {
    cd <- filter_samples(cd, col = "group", value = grp)
  }

  # Run harmonic regression
  res_harm <- HarmonicRegression::harmonic.regression(
    inputts = t(dataset(cd)),
    inputtime = metadata(cd)[["time"]],
    Tau = mean(cd$period),
    normalize = FALSE,
    trend.eliminate = FALSE,  # TODO: make this a variable,
    trend.degree = 1  # TODO: make this a variable
  )

  # Get phase estimate in hours.`harmonic.regression()` uses a cosine, but I
  # tend to think of sine waves. Amplitude and mesor stay the same, but the
  # phase is shifted by -pi/2 in radians, so by -period/4 in hours. Using the
  # modulo operator negative phase values "wrap" around and become positive.
  per <- mean(cd$period)
  phase_estimate_rad <- res_harm$pars$phi
  phase_estimate_h_cos <- phase_estimate_rad * per / (2 * pi)
  phase_estimate_h_sin <- (phase_estimate_h_cos - per / 4) %% per

  df_out <- data.frame(
    feature = row.names(res_harm$pars),
    period = mean(cd$period),
    phase_estimate = phase_estimate_h_sin
  )

  # Add mesor and amplitude estimates
  if (cd$log_transformed == TRUE) {
    # Get logarithmic base
    b <- cd$log_base

    # Get estimates for mesor and amplitude in log scale
    log_mesors <- res_harm$means
    log_amps <- res_harm$pars$amp

    # Get values for peak and trough
    log_peaks <- b^(log_mesors + log_amps)
    log_troughs <- b^(log_mesors - log_amps)

    # Get mesor and amplitude in linear scale
    lin_mesors <- (log_peaks + log_troughs) / 2
    lin_amplitudes <- (log_peaks - log_troughs) / 2

    # Get relative amplitude
    lin_relative_amplitudes <- lin_amplitudes / lin_mesors
    # lin_relative_amplitudes <- compute_relative_amplitude(log_amps, b)

    # Add to data frame

    # TODO: Be clear in clockworks documentation that in this case (i.e. with
    # log-transformed data) the values you get for `mesor_estimate` and
    # `amplitude_estimate` are in the log-scale, i.e. in the scale of the data
    # the user provided, but the `relative_amplitude_estimate` is in the linear
    # scale. We decided to do it this way because it seems appropriate to give
    # the mesor and amplitude for the actual data the user provides. On the
    # other hand, getting the relative amplitude in the linear scale is not a
    # super trivial task (but it might still be of interest), so we provide
    # that. The relative amplitude in the log-scale is easy to get by simply
    # dividing the amplitude estimate by the mesor estimate.
    df_out$mesor_estimate <- log_mesors
    df_out$amplitude_estimate <- log_amps
    df_out$relative_amplitude_estimate <- lin_relative_amplitudes

  } else {
    df_out$mesor_estimate <- res_harm$means
    df_out$amplitude_estimate <- res_harm$pars$amp
    df_out$relative_amplitude_estimate <- df_out$amplitude_estimate / df_out$mesor_estimate
  }

  # Add group
  if (!is.na(grp)) {
    df_out$group <- grp
  }

  return(df_out)
}
