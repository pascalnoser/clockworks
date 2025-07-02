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
#' @returns A data frame with the results of a harmonic regression.
estimate_wave_params <- function(cd, grp) {
  # Filter cd object to only include the current group
  cd_filt <- filter_samples(cd, col = "group", value = grp)

  # Run harmonic regression
  res_harm <- HarmonicRegression::harmonic.regression(
    inputts = t(dataset(cd_filt)),
    inputtime = metadata(cd_filt)[["time"]],
    Tau = mean(cd_filt$period),
    normalize = FALSE,
    trend.eliminate = TRUE,  # TODO: make this a variable,
    trend.degree = 1  # TODO: make this a variable
  )

  # Add phase estimate (in hours) to output
  df_out <- data.frame(
    feature = row.names(res_harm$pars),
    period = mean(cd_filt$period),
    phase_estimate = res_harm$pars$phi / (2*pi) * mean(cd_filt$period)
  )

  # Add mesor and amplitude estimates
  if (cd_filt$log_transformed == TRUE) {
    # Get logarithmic base
    b <- cd_filt$log_base

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
  df_out$group <- grp

  return(df_out)
}
