# Synthetic data set with two rhythmic features and one noise feature
make_gc_cd <- function(dt, n, per_true = 24) {
  set.seed(1)
  t <- (0:(n - 1)) * dt
  mat <- rbind(
    R1 = cosine(t, 5, 2, 3, per_true) + rnorm(n, 0, 0.2),
    R2 = cosine(t, 5, 2, 8, per_true) + rnorm(n, 0, 0.2),
    N1 = rnorm(n, 5, 1)
  )
  make_synth_cd(mat, t, period = 24)
}

test_that("genecycle_index_period() converts spectrum indices to periods", {
  # Rank-based spectrum is zero-padded to twice the length
  expect_equal(genecycle_index_period(5, 10, 5, "rank"), 25)
  expect_equal(genecycle_index_period(11, 24, 5, "rank"), 24)
  # Regression-based spectrum uses the standard Fourier frequencies
  expect_equal(genecycle_index_period(3, 24, 2, "regression"), 24)
  # Index 1 is the mean, unknown delta_t gives NA
  expect_true(is.na(genecycle_index_period(1, 24, 2, "rank")))
  expect_true(is.na(genecycle_index_period(5, 24, NA, "rank")))
})

test_that("GeneCycle warns and reports the tested period if it differs", {
  cd <- make_gc_cd(dt = 5, n = 10)
  expect_warning(
    cd <- suppressMessages(clockworks(cd, "GeneCycle")),
    "Instead of 24, the period 25 is tested"
  )
  res <- get_results(cd, "GeneCycle")
  expect_true("period_estimate" %in% colnames(res))
  expect_equal(res$period_estimate, rep(25, 3))
})

test_that("GeneCycle tests the requested period if it fits the design", {
  cd <- make_gc_cd(dt = 5, n = 24)
  expect_no_warning(cd <- suppressMessages(clockworks(cd, "GeneCycle")))
  res <- get_results(cd, "GeneCycle")
  expect_equal(res$period_estimate, rep(24, 3))
})

test_that("GeneCycle without a period reports the period of the spectrum peak", {
  cd <- make_gc_cd(dt = 2, n = 24)
  cd <- run_quiet(cd, "GeneCycle", method_args = list(index = NA))
  res <- get_results(cd, "GeneCycle")
  expect_equal(res$period_estimate[res$feature %in% c("R1", "R2")], c(24, 24))
})

test_that("GeneCycle warns about missing time points", {
  cd <- make_gc_cd(dt = 2, n = 24)
  cd <- suppressMessages(filter_samples(cd, time != 10))
  w <- capture_warnings(suppressMessages(clockworks(cd, "GeneCycle")))
  expect_true(any(grepl("equally spaced time points", w)))
})

test_that("GeneCycle results include period_estimate for every group", {
  cd <- run_quiet(make_cd(), "GeneCycle")
  res <- get_results(cd, "GeneCycle")
  expect_equal(nrow(res), 20)
  expect_equal(res$period_estimate, rep(24, 20))
})
