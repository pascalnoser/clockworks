build <- function(p) suppressWarnings(ggplot2::ggplot_build(p))

# ---- plot_feature() ----

test_that("plot_feature() returns ggplot objects for both plot types", {
  cd <- make_cd()
  for (type in c("points", "mean_sd")) {
    p <- plot_feature(cd, "Gene_01", plot_type = type)
    expect_s3_class(p, "ggplot")
    expect_no_error(build(p))
  }
  p <- plot_feature(cd, 2, add_line = TRUE, background_cutoffs = c(12, 24, 36))
  expect_s3_class(p, "ggplot")
  expect_no_error(build(p))
  p <- plot_feature(cd, "Gene_01", groups = "A", add_line = TRUE)
  expect_no_error(build(p))
  expect_error(plot_feature(cd, "nope"), "not found")
})

test_that("plot_feature() handles data without groups or subjects", {
  cd <- make_cd(subject = FALSE, group = FALSE)
  for (type in c("points", "mean_sd")) {
    p <- plot_feature(cd, "Gene_02", plot_type = type, add_line = TRUE)
    expect_s3_class(p, "ggplot")
    expect_no_error(build(p))
  }
})

test_that("plot_feature() with mean_sd does not produce NA means for NA values", {
  dat <- cw_data
  dat["Gene_01", 1] <- NA
  cd <- make_cd(dataset = dat)
  expect_no_error(plot_feature(cd, "Gene_01"))

  p <- plot_feature(cd, "Gene_01", plot_type = "mean_sd", add_line = TRUE)
  expect_s3_class(p, "ggplot")
  # Means (lines/points) must not be NA. Error bars can be NA where only one
  # value is left at a time point (sd of a single value).
  b <- build(p)
  for (layer_data in b$data) {
    if ("y" %in% names(layer_data)) {
      expect_false(anyNA(layer_data$y))
    }
  }
  err_layer <- which(vapply(
    p$layers, function(l) inherits(l$geom, "GeomErrorbar"), logical(1)
  ))
  err <- b$data[[err_layer]]
  # Two samples per time point and group, one of them NA: only that bar is NA
  expect_equal(sum(is.na(err$ymin)), 1)
})

# ---- plot_phase_estimates() ----

test_that("plot_phase_estimates() returns a ggplot", {
  p <- plot_phase_estimates(make_cd())
  expect_s3_class(p, "ggplot")
  expect_no_error(build(p))

  p <- plot_phase_estimates(make_cd(group = FALSE))
  expect_s3_class(p, "ggplot")
  expect_no_error(build(p))
})

test_that("plot_phase_estimates() with method and the default cutoff keeps all features", {
  cd <- cd_jtk()
  p <- plot_phase_estimates(cd, method = "JTK_CYCLE")
  expect_s3_class(p, "ggplot")
  expect_no_error(build(p))
  expect_equal(sum(p$data$count), nrow(get_wave_params(cd)))

  # A stricter cutoff keeps fewer features
  p_sig <- plot_phase_estimates(cd, method = "JTK_CYCLE", pval_adj_cutoff = 0.05)
  n_sig <- sum(get_results(cd, "JTK_CYCLE")$pval_adj < 0.05, na.rm = TRUE)
  expect_equal(sum(p_sig$data$count), n_sig)
})

# ---- plot_heatmap() ----

test_that("plot_heatmap() returns a ggplot", {
  cd <- make_cd()
  p <- plot_heatmap(cd, groups = "A")
  expect_s3_class(p, "ggplot")
  expect_no_error(build(p))

  p <- plot_heatmap(make_cd(group = FALSE), scale = FALSE)
  expect_s3_class(p, "ggplot")
  expect_no_error(build(p))

  expect_error(plot_heatmap(cd), "more than one group")
})

test_that("plot_heatmap() filters and orders by method results", {
  cd <- cd_jtk()
  p <- plot_heatmap(cd, method = "JTK_CYCLE", groups = "B")
  expect_s3_class(p, "ggplot")
  expect_no_error(build(p))
  p <- plot_heatmap(
    cd, method = "JTK_CYCLE", phase_from = "method", groups = "A"
  )
  expect_s3_class(p, "ggplot")
  expect_no_error(build(p))
  expect_error(
    plot_heatmap(cd, method = "JTK_CYCLE", pval_adj_cutoff = 0, groups = "A"),
    "No features left"
  )
})

# ---- plot_upset() ----

test_that("plot_upset() works with one method and compare = 'groups'", {
  cd <- cd_jtk()
  p <- suppressWarnings(plot_upset(cd, compare = "groups"))
  expect_s3_class(p, "ggplot")
  expect_error(plot_upset(cd), "Only one method")
})

test_that("plot_upset() warns instead of erroring when nothing is significant", {
  cd <- cd_jtk()
  expect_warning(
    res <- plot_upset(cd, pval_adj_cutoff = 0, compare = "groups"),
    "No significant features"
  )
  expect_false(inherits(res, "ggplot"))
})
