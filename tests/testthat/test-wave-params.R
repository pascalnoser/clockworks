times <- seq(0, 46, by = 2)

test_that("harmonic regression recovers mesor, amplitude and phase", {
  mat <- rbind(
    F1 = cosine(times, mesor = 5, amp = 2, phase = 6),
    F2 = cosine(times, mesor = 10, amp = 0.5, phase = 20),
    F3 = cosine(times, mesor = 3, amp = 1, phase = 0)
  )
  cd <- make_synth_cd(mat, times)
  w <- get_wave_params(cd)
  w <- w[match(rownames(mat), w$feature), ]

  expect_equal(w$feature, c("F1", "F2", "F3"))
  expect_equal(w$mesor_estimate, c(5, 10, 3), tolerance = 1e-8)
  expect_equal(w$amplitude_estimate, c(2, 0.5, 1), tolerance = 1e-8)
  # Compare phases on the circle (phase 0 may come back as ~24)
  phase_diff <- (w$phase_estimate - c(6, 20, 0) + 12) %% 24 - 12
  expect_true(all(abs(phase_diff) < 1e-6))
  expect_true(all(w$phase_estimate >= 0 & w$phase_estimate <= 24))
  expect_equal(w$relative_amplitude_estimate, c(2 / 5, 0.5 / 10, 1 / 3), tolerance = 1e-8)
  expect_equal(unique(w$period), 24)
})

test_that("harmonic regression recovers parameters with a non-default period", {
  mat <- rbind(F1 = cosine(times, 4, 1.5, 9, per = 12), F2 = rnorm(length(times)))
  cd <- make_synth_cd(mat, times, period = 12)
  w <- get_wave_params(cd)
  w1 <- w[w$feature == "F1", ]
  expect_equal(w1$mesor_estimate, 4, tolerance = 1e-8)
  expect_equal(w1$amplitude_estimate, 1.5, tolerance = 1e-8)
  expect_equal(w1$phase_estimate, 9, tolerance = 1e-8)
})

test_that("harmonic regression works for a single feature", {
  mat <- matrix(cosine(times, 5, 2, 6), nrow = 1, dimnames = list("F1", NULL))
  cd <- make_synth_cd(mat, times)
  w <- get_wave_params(cd)
  expect_equal(nrow(w), 1)
  expect_equal(w$feature, "F1")
  expect_equal(w$mesor_estimate, 5, tolerance = 1e-8)
  expect_equal(w$amplitude_estimate, 2, tolerance = 1e-8)
  expect_equal(w$phase_estimate, 6, tolerance = 1e-8)
})

test_that("harmonic regression works with missing values", {
  mat <- rbind(
    F1 = cosine(times, 5, 2, 6),
    F2 = cosine(times, 10, 1, 15)
  )
  mat["F1", c(2, 7, 13)] <- NA
  cd <- make_synth_cd(mat, times)
  w <- get_wave_params(cd)
  w <- w[match(rownames(mat), w$feature), ]
  expect_equal(w$mesor_estimate, c(5, 10), tolerance = 1e-8)
  expect_equal(w$amplitude_estimate, c(2, 1), tolerance = 1e-8)
  expect_equal(w$phase_estimate, c(6, 15), tolerance = 1e-8)
})

test_that("a feature observed at only 2 distinct phases gives NA parameters", {
  # All samples at only 2 phases (0 and 12 h) -> rank-deficient design
  t2 <- c(0, 12, 24, 36, 0, 12, 24, 36)
  mat <- rbind(F1 = c(1, 3, 1, 3, 1.1, 2.9, 1.2, 3.1), F2 = rnorm(8))
  cd <- expect_no_error(make_synth_cd(mat, t2))
  w <- get_wave_params(cd)
  expect_equal(nrow(w), 2)
  expect_true(all(is.na(w$amplitude_estimate)))
  expect_true(all(is.na(w$phase_estimate)))
  expect_true(all(is.na(w$mesor_estimate)))
})

test_that("only the feature with 2 observed phases is NA when data are missing", {
  mat <- rbind(
    F1 = cosine(times, 5, 2, 6),
    F2 = cosine(times, 10, 1, 15)
  )
  # Keep F2 only at times 0, 12, 24, 36 (phases 0 and 12)
  mat["F2", !times %in% c(0, 12, 24, 36)] <- NA
  cd <- expect_no_error(make_synth_cd(mat, times))
  w <- get_wave_params(cd)
  w <- w[match(rownames(mat), w$feature), ]
  expect_equal(w$amplitude_estimate[1], 2, tolerance = 1e-8)
  expect_true(is.na(w$amplitude_estimate[2]))
  expect_true(is.na(w$phase_estimate[2]))
})

test_that("wave params exist per group and in both versions for repeated measures", {
  cd <- make_cd()
  w_orig <- get_wave_params(cd, version = "original")
  w_bc <- get_wave_params(cd, version = "batch_corrected")
  expect_equal(nrow(w_orig), 20)
  expect_equal(nrow(w_bc), 20)
  expect_setequal(as.character(w_orig$group), c("A", "B"))
  expect_setequal(w_orig$feature, rownames(cw_data))
  expect_identical(get_wave_params(cd), w_bc)
})

test_that("delta_t is found for regular non-integer sampling with a gap", {
  t04 <- seq(0, 9.6, by = 0.4)
  t04 <- t04[-6] # drop one time point (2.0)
  mat <- rbind(F1 = cosine(t04, 5, 1, 3, per = 4.8), F2 = rnorm(length(t04)))
  cd <- make_synth_cd(mat, t04, period = 4.8)
  expect_equal(cd$delta_t, 0.4)
  expect_false(is.na(cd$n_cycles))
})

test_that("delta_t is NA for irregular sampling", {
  t_irr <- c(0, 1, 3, 4.5, 7, 12, 13.3, 20)
  cd <- make_synth_cd(rbind(F1 = rnorm(8), F2 = rnorm(8)), t_irr)
  expect_true(is.na(cd$delta_t))
})
