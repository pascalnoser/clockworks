test_that("clockworks() stores standardised results", {
  cd <- cd_jtk()
  res <- get_results(cd, "JTK_CYCLE")
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 20) # 10 features x 2 groups
  expect_true(all(
    c("feature", "group", "period_estimate", "phase_estimate", "pval",
      "pval_adj", "method") %in% colnames(res)
  ))
  expect_setequal(res$feature, rownames(cw_data))
  expect_true(all(res$method == "JTK_CYCLE"))
  expect_true(all(res$pval_adj >= res$pval - 1e-12, na.rm = TRUE))
  # The two rhythmic genes are detected in both groups
  sig <- res$feature[res$pval_adj < 0.05]
  expect_true(all(c("Gene_01", "Gene_02") %in% sig))
})

test_that("batch_correction changes results for repeated measures", {
  cd <- make_cd()
  res_bc <- get_results(run_quiet(cd, "JTK_CYCLE", batch_correction = TRUE))
  res_raw <- get_results(run_quiet(cd, "JTK_CYCLE", batch_correction = FALSE))
  key <- function(x) paste(x$feature, x$group)
  res_raw <- res_raw[match(key(res_bc), key(res_raw)), ]
  expect_identical(key(res_bc), key(res_raw))
  expect_false(isTRUE(all.equal(res_bc$pval, res_raw$pval)))
})

test_that("get_results() returns a data frame for one method and a list for several", {
  # Without subjects RAIN runs in independent mode, which is fast
  cd <- make_cd(subject = FALSE)
  cd <- run_quiet(cd, "JTK_CYCLE")

  expect_s3_class(get_results(cd), "data.frame")
  expect_s3_class(get_results(cd, "JTK_CYCLE"), "data.frame")

  cd <- run_quiet(cd, "RAIN")
  res <- get_results(cd)
  expect_type(res, "list")
  expect_false(is.data.frame(res))
  expect_named(res, c("JTK_CYCLE", "RAIN"))
  expect_s3_class(res$RAIN, "data.frame")
  expect_s3_class(get_results(cd, "RAIN"), "data.frame")
  expect_identical(get_results(cd, "RAIN"), res$RAIN)

  expect_error(get_results(cd, "LS"), "Unknown method")
  expect_error(get_results(make_cd()), "No results")
})

test_that("rerunning a method replaces its results", {
  cd <- run_quiet(cd_jtk(), "JTK_CYCLE")
  expect_named(cd@results, "JTK_CYCLE")
  expect_equal(nrow(get_results(cd, "JTK_CYCLE")), 20)
})
