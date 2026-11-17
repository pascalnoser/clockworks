# Synthetic data set with one rhythmic and one noise feature
make_grid_cd <- function(dt, n, period = 24) {
  set.seed(1)
  t <- (0:(n - 1)) * dt
  mat <- rbind(
    R1 = cosine(t, 5, 2, 3) + rnorm(n, 0, 0.2),
    N1 = rnorm(n, 5, 1)
  )
  make_synth_cd(mat, t, period = period)
}

test_that("warn_tested_periods() only warns if the tested periods differ", {
  rule <- "whole multiples of 5"
  expect_warning(
    warn_tested_periods("X", 24, c(20, 25), rule),
    "Instead of 24, the periods 20, 25 are tested"
  )
  expect_warning(
    warn_tested_periods("X", c(20, 28), c(20, 25, 30), rule),
    "Instead of the range 20 - 28, the periods 20, 25, 30 are tested"
  )
  expect_no_warning(warn_tested_periods("X", 24, 24, rule))
  expect_no_warning(warn_tested_periods("X", c(20, 28), c(20, 24, 28), rule))
  expect_no_warning(warn_tested_periods("X", c(20, 28), c(22, 24), rule))
  expect_no_warning(warn_tested_periods("X", NA, 25, rule))
})

test_that("warn_once() shows repeated warnings only once", {
  w <- capture_warnings(warn_once(for (i in 1:3) warning("same")))
  expect_identical(w, "same")
})

test_that("jtk_tested_periods() reproduces MetaCycle's period grid", {
  t5 <- (0:19) * 5
  expect_equal(as.numeric(jtk_tested_periods(t5, 24, 24)), 25)
  expect_equal(as.numeric(jtk_tested_periods(t5, 20, 28)), c(20, 25, 30))
  t2 <- (0:23) * 2
  expect_equal(as.numeric(jtk_tested_periods(t2, 20, 28)), seq(20, 28, 2))
  # Capped at the duration of the time series
  expect_equal(as.numeric(jtk_tested_periods((0:5) * 4, 20, 28)), c(20, 24))
})

test_that("RAIN warns if the period is no multiple of the sampling interval", {
  cd <- make_grid_cd(dt = 5, n = 20)
  expect_warning(
    cd <- suppressMessages(clockworks(cd, "RAIN")),
    "RAIN can only test .* Instead of 24, the periods 20, 25 are tested"
  )
  expect_true(all(get_results(cd, "RAIN")$period_estimate %in% c(20, 25)))
  expect_equal(get_results(cd, "RAIN")$period_estimate[1], 25)

  expect_no_warning(suppressMessages(clockworks(make_grid_cd(2, 24), "RAIN")))
  expect_no_warning(suppressMessages(
    clockworks(make_grid_cd(2, 24, c(20, 28)), "RAIN")
  ))
})

test_that("JTK_CYCLE warns if the period is no multiple of the sampling interval", {
  cd <- make_grid_cd(dt = 5, n = 20)
  expect_warning(
    cd <- suppressMessages(clockworks(cd, "JTK_CYCLE")),
    "JTK_CYCLE can only test .* Instead of 24, the period 25 is tested"
  )
  expect_equal(get_results(cd, "JTK_CYCLE")$period_estimate[1], 25)

  expect_warning(
    suppressMessages(clockworks(make_grid_cd(5, 20, c(20, 28)), "JTK_CYCLE")),
    "the periods 20, 25, 30 are tested"
  )
  expect_no_warning(suppressMessages(
    clockworks(make_grid_cd(2, 24), "JTK_CYCLE")
  ))
})

test_that("period warnings are shown once for several groups", {
  meta <- cw_metadata[cw_metadata$Time %% 4 == 0, ]
  cd <- make_cd(
    dataset = cw_data[, meta$Sample_ID],
    metadata = meta,
    subject = FALSE
  )
  # delta_t = 4 and period = 24 fit, so make the period not fit via method_args
  w <- capture_warnings(suppressMessages(
    clockworks(cd, "RAIN", method_args = list(period = 26))
  ))
  expect_equal(sum(grepl("RAIN can only test", w)), 1)
})

test_that("meta2d warns about the periods tested by its JTK part", {
  cd <- make_grid_cd(dt = 5, n = 20)
  expect_warning(
    suppressMessages(clockworks(cd, "meta2d")),
    "meta2d \\(JTK\\) can only test .* Instead of 24, the period 25 is tested"
  )
  # No warning if JTK is not run
  expect_no_warning(suppressMessages(
    clockworks(cd, "meta2d", method_args = list(cycMethod = c("ARS", "LS")))
  ))
  expect_no_warning(suppressMessages(clockworks(make_grid_cd(2, 24), "meta2d")))
})

test_that("meta3d warns once about the periods tested by JTK", {
  set.seed(1)
  t <- (0:19) * 5
  times <- rep(t, 3)
  mat <- rbind(
    R1 = cosine(times, 5, 2, 3) + rnorm(60, 0, 0.2),
    N1 = rnorm(60, 5, 1)
  )
  colnames(mat) <- paste0("X", seq_along(times))
  meta <- data.frame(
    sample = colnames(mat),
    time = times,
    subject = rep(paste0("S", 1:3), each = 20)
  )
  cd <- suppressMessages(CircadianData(
    mat, meta, "sample", "time",
    colname_subject = "subject", period = 24
  ))

  capture.output(
    w <- capture_warnings(cd_res <- suppressMessages(clockworks(cd, "meta3d")))
  )
  expect_equal(sum(grepl("meta3d \\(JTK\\) can only test", w)), 1)
  expect_equal(get_results(cd_res, "meta3d")$period_estimate[1], 25)

  capture.output(
    w <- capture_warnings(suppressMessages(
      clockworks(cd, "meta3d", method_args = list(cycMethodOne = "LS"))
    ))
  )
  expect_false(any(grepl("can only test", w)))
})
