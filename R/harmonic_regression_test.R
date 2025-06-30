# noise.levels <- c(0, 0.02, 0.05)
# timeStart <- 0
# timeEnd <- 47
# time <- timeStart:timeEnd
# amplitude <- 8
# baseline <- 10
# period <- 24
#
#
# tt_pred = seq(timeStart, timeEnd, 0.25)
#
# getPred <- function(tt, amp, mesor, period = 24, phase = 0) {
#   amp * cos(2 * pi/period * tt - phase) + mesor
# }
#
#
#
# ##-----------------------------------------------
# ## Linear scale
# ##-----------------------------------------------
# test_linear <- apply(matrix(noise.levels, nrow = 1), 2, function(noise){
#   timecourse = baseline + amplitude * cos((time) / period * 2 * pi) + rnorm(length(time), 0, noise)
# })
#
# colnames(test_linear) <- paste("Gene", 1:ncol(test_linear), sep = "_")
# rownames(test_linear) <- paste("Time", 1:nrow(test_linear), sep = "_")
#
# hreg <- harmonic.regression(inputts = test_linear,
#                             inputtime = time,
#                             Tau = period,
#                             normalize = FALSE,
#                             norm.pol = F,
#                             norm.pol.degree = 1,
#                             trend.eliminate = TRUE,
#                             trend.degree = 1)
#
#
# ##-----------------------------------------------
# ## Log2 cale
# ##-----------------------------------------------
# test_log2 <- log2(test_linear)
#
# hreg_log <- harmonic.regression(inputts = test_log2,
#                             inputtime = time,
#                             Tau = period,
#                             normalize = FALSE,
#                             norm.pol = F,
#                             norm.pol.degree = 1,
#                             trend.eliminate = TRUE,
#                             trend.degree = 1)
#
# hreg_log$means
# hreg_log$pars
#
#
# log_mes = hreg_log$means[1]
# log_amp = hreg_log$pars$amp[1]
# log_peak = 2^(log_mes + log_amp)
# log_trough = 2^(log_mes - log_amp)
#
# # Amplitude estimate in linear scale
# lin_amp = (log_peak - log_trough) / 2
# # Mesor estimate in linear scale
# lin_mes = (log_peak + log_trough) / 2
# # lin_mes = 2^log_mes
# # Relative amp in linear scale
# lin_relamp = ((log_peak - log_trough) / 2) / ((log_peak + log_trough) / 2)
#
#
# par(mfrow = c(1,2))
# ## Plot the time series (linear scale)
# plot(
#   time,
#   test_linear[, 1],
#   xlab = "Time",
#   ylab = "Linear Scale",
#   ylim = c(baseline - 1.5 * amplitude, baseline + 1.5 * amplitude)
# )
# # Add predicted wave
# vals_pred_lin <- getPred(tt_pred, hreg$pars$amp[1], hreg$means[1])
# vals_pred_log <- getPred(tt_pred, lin_amp, lin_mes)
# lines(tt_pred, vals_pred_lin, col = "red")
# lines(tt_pred, vals_pred_log, col = "blue")
#
# ## Plot the time series (log2 scale)
# plot(
#   time,
#   test_log2[, 1],
#   xlab = "Time",
#   ylab = "Log2 Scale",
#   ylim = c(log_mes - 1.5 * log_amp, log_mes + 1.5 * log_amp)
# )
# # Add predicted wave
# vals_pred <- getPred(tt_pred, hreg_log$pars$amp[1], hreg_log$means[1])
# lines(tt_pred, vals_pred, col = "red")
#
