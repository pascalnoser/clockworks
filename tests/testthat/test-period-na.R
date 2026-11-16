make_cd_no_period <- function() {
  suppressMessages(CircadianData(
    cw_data,
    cw_metadata,
    colname_sample = "Sample_ID",
    colname_time = "Time",
    colname_group = "Group",
    colname_subject = "Subject_ID",
    period = NA
  ))
}

test_that("period = NA creates an object with a warning and no wave params", {
  expect_warning(cd <- make_cd_no_period(), "No period defined")
  expect_s4_class(cd, "CircadianData")
  expect_true(is.na(cd$period))
  w <- get_wave_params(cd)
  expect_true(is.null(w) || nrow(w) == 0)
})

test_that("plot_phase_estimates() stops without a period", {
  cd <- suppressWarnings(make_cd_no_period())
  expect_error(plot_phase_estimates(cd), "no period")
})

test_that("plot_heatmap() stops without a period", {
  cd <- suppressWarnings(make_cd_no_period())
  expect_error(plot_heatmap(cd, groups = "A"), "no period")
})

test_that("plot_feature() works without a period and draws no wave", {
  cd <- suppressWarnings(make_cd_no_period())
  p <- plot_feature(cd, "Gene_01")
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomLine" %in% geoms)

  p2 <- plot_feature(cd, "Gene_01", plot_type = "mean_sd")
  expect_s3_class(p2, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p2))
})

test_that("clockworks() refuses methods that need a period", {
  cd <- suppressWarnings(make_cd_no_period())
  expect_error(clockworks(cd, "JTK_CYCLE"), "requires a period")
})
