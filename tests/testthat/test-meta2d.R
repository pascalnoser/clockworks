make_meta2d_cd <- function(times) {
  set.seed(1)
  n <- length(times)
  mat <- rbind(
    R1 = cosine(times, 5, 2, 3) + rnorm(n, 0, 0.2),
    N1 = rnorm(n, 5, 1)
  )
  make_synth_cd(mat, times)
}

test_that("meta2d works if only one method is run", {
  cd <- make_meta2d_cd((0:23) * 2)
  cols <- c("feature", "period_estimate", "phase_estimate", "pval", "pval_adj")
  for (m in c("ARS", "JTK", "LS")) {
    expect_warning(
      cd_res <- suppressMessages(
        clockworks(cd, "meta2d", method_args = list(cycMethod = m))
      ),
      paste0("meta2d only ran ", m)
    )
    res <- get_results(cd_res, "meta2d")
    expect_equal(nrow(res), 2)
    expect_true(all(cols %in% colnames(res)))
    expect_false(anyNA(res[, cols]))
    expect_lt(res$pval[res$feature == "R1"], 0.05)
  }
})

test_that("meta2d warns if MetaCycle skips all but one of the selected methods", {
  # Two replicates per time point: ARS is skipped by meta2d
  cd <- make_meta2d_cd(rep((0:11) * 4, each = 2))
  expect_warning(
    suppressMessages(
      clockworks(cd, "meta2d", method_args = list(cycMethod = c("ARS", "LS")))
    ),
    "meta2d only ran LS"
  )
})

test_that("meta2d does not warn about integration if several methods run", {
  cd <- make_meta2d_cd((0:23) * 2)
  expect_no_warning(cd_res <- suppressMessages(clockworks(cd, "meta2d")))
  expect_false(anyNA(get_results(cd_res, "meta2d")$pval))
})
