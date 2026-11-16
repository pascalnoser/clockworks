expect_in_sync <- function(cd) {
  expect_identical(colnames(get_dataset(cd)), rownames(get_metadata(cd)))
  expect_identical(colnames(cd), rownames(get_metadata(cd)))
}

# ---- Subsetting ----

test_that("[ subsets by index and keeps dataset and metadata in sync", {
  cd <- make_cd()
  sub <- cd[1:3, 1:10]
  expect_equal(dim(sub), c(3L, 10L))
  expect_identical(rownames(sub), rownames(cw_data)[1:3])
  expect_identical(colnames(sub), colnames(cd)[1:10])
  expect_in_sync(sub)
  expect_identical(
    get_dataset(sub),
    get_dataset(cd)[1:3, 1:10, drop = FALSE]
  )
})

test_that("[ subsets by name", {
  cd <- make_cd()
  feats <- c("Gene_05", "Gene_02")
  samples <- rev(colnames(cd)[c(1, 20, 50, 96)])
  # With 4 samples, subjects are confounded with time when the wave
  # parameters are re-estimated, which gives a warning
  sub <- suppressWarnings(cd[feats, samples])
  expect_identical(rownames(sub), feats)
  expect_identical(colnames(sub), samples)
  expect_in_sync(sub)
  expect_identical(get_dataset(sub), get_dataset(cd)[feats, samples])
  expect_equal(
    get_metadata(sub)$time,
    get_metadata(cd)[samples, "time"]
  )
  expect_error(cd["not_a_gene", ], "not found")
  expect_error(cd[, "not_a_sample"], "not found")
})

test_that("[ subsets by logical", {
  cd <- make_cd()
  keep_s <- get_metadata(cd)$group == "A"
  keep_f <- rep(c(TRUE, FALSE), 5)
  sub <- cd[keep_f, keep_s]
  expect_equal(dim(sub), c(5L, 48L))
  expect_in_sync(sub)
  expect_true(all(get_metadata(sub)$group == "A"))
  # Unused factor levels are dropped
  expect_identical(levels(get_metadata(sub)$group), "A")
  expect_equal(sub$n_groups, 1)
})

test_that("[ subsets wave params by feature", {
  cd <- make_cd()
  sub <- cd[c("Gene_01", "Gene_03"), ]
  w <- get_wave_params(sub)
  expect_setequal(w$feature, c("Gene_01", "Gene_03"))
  w_full <- get_wave_params(cd)
  key <- function(x) paste(x$feature, x$group)
  expect_equal(
    w$amplitude_estimate,
    w_full$amplitude_estimate[match(key(w), key(w_full))]
  )
})

test_that("[ errors when results are stored", {
  cd <- cd_jtk()
  expect_error(cd[1:2, ], "invalidate stored results")
  expect_error(cd[, 1:10], "invalidate stored results")
})

# ---- rownames<- / dimnames<- ----

test_that("rownames<- renames features in wave params by name", {
  cd <- make_cd()
  old <- rownames(cd)
  new <- paste0("new_", old)
  w_old <- get_wave_params(cd)
  rownames(cd) <- new
  expect_identical(rownames(cd), new)
  expect_identical(rownames(get_dataset(cd)), new)
  for (version in c("original", "batch_corrected")) {
    w <- get_wave_params(cd, version = version)
    expect_setequal(w$feature, new)
  }
  w_new <- get_wave_params(cd)
  expect_identical(w_new$feature, paste0("new_", w_old$feature))
  expect_identical(w_new$amplitude_estimate, w_old$amplitude_estimate)
})

test_that("rownames<- renames features in shuffled formatted results by name", {
  cd <- cd_jtk()
  res <- cd@results$JTK_CYCLE$res_formatted
  set.seed(1)
  res_shuffled <- res[sample(nrow(res)), ]
  cd@results$JTK_CYCLE$res_formatted <- res_shuffled

  # Map every feature to a different one, so that a positional mapping fails
  old <- rownames(cd)
  new <- paste0(old, "_renamed")
  expect_warning(rownames(cd) <- new, "formatted results only")

  res_new <- get_results(cd, "JTK_CYCLE")
  expect_identical(res_new$feature, paste0(res_shuffled$feature, "_renamed"))
  expect_identical(res_new$pval, res_shuffled$pval)

  # Same check with a permutation of the existing names
  cd2 <- cd_jtk()
  cd2@results$JTK_CYCLE$res_formatted <- res_shuffled
  perm <- rev(rownames(cd2))
  expect_warning(rownames(cd2) <- perm, "formatted results only")
  mapping <- stats::setNames(perm, rownames(cd_jtk()))
  res_perm <- get_results(cd2, "JTK_CYCLE")
  expect_identical(res_perm$feature, unname(mapping[res_shuffled$feature]))
})

test_that("rownames<- validates input", {
  cd <- make_cd()
  expect_error(rownames(cd) <- letters[1:3], "Incorrect number")
  expect_error(rownames(cd) <- rep("a", 10), "unique")
})

test_that("dimnames<- also updates wave params", {
  cd <- make_cd()
  w_old <- get_wave_params(cd, version = "original")
  new_f <- paste0("f", 1:10)
  new_s <- paste0("s", 1:96)
  dimnames(cd) <- list(new_f, new_s)
  expect_identical(rownames(cd), new_f)
  expect_identical(colnames(cd), new_s)
  expect_identical(rownames(get_metadata(cd)), new_s)
  expect_in_sync(cd)
  w <- get_wave_params(cd, version = "original")
  expect_identical(
    w$feature,
    new_f[match(w_old$feature, rownames(cw_data))]
  )
})

test_that("colnames<- keeps dataset and metadata in sync", {
  cd <- make_cd()
  colnames(cd) <- paste0("x", seq_len(ncol(cd)))
  expect_in_sync(cd)
})

# ---- order_samples() ----

test_that("order_samples() sorts samples and keeps them in sync", {
  set.seed(2)
  meta_shuffled <- cw_metadata[sample(nrow(cw_metadata)), ]
  cd <- make_cd(metadata = meta_shuffled)

  sorted <- order_samples(cd, "time")
  expect_false(is.unsorted(get_metadata(sorted)$time))
  expect_in_sync(sorted)
  # Values move together with their sample
  s <- colnames(sorted)[1:5]
  expect_identical(get_dataset(sorted)[, s], get_dataset(cd)[, s])

  sorted2 <- order_samples(
    cd,
    c("group", "time"),
    decreasing = c(FALSE, TRUE)
  )
  md <- get_metadata(sorted2)
  expect_false(is.unsorted(as.integer(md$group)))
  for (g in levels(md$group)) {
    expect_false(is.unsorted(rev(md$time[md$group == g])))
  }
  expect_in_sync(sorted2)

  expect_error(order_samples(cd, "nonexistent"), "not found")
})

# ---- filter_samples() ----

test_that("filter_samples() works with the internal column names", {
  cd <- make_cd()

  cd_a <- filter_samples(cd, group == "A")
  expect_equal(ncol(cd_a), 48)
  expect_true(all(get_metadata(cd_a)$group == "A"))
  expect_in_sync(cd_a)
  expect_setequal(as.character(get_wave_params(cd_a)$group), "A")

  cd_early <- filter_samples(cd, time < 24)
  expect_true(all(get_metadata(cd_early)$time < 24))
  expect_equal(ncol(cd_early), 48)
  expect_in_sync(cd_early)

  subj <- levels(get_metadata(cd)$subject_ID)[1:2]
  cd_subj <- filter_samples(cd, subject_ID %in% subj)
  expect_setequal(as.character(get_metadata(cd_subj)$subject_ID), subj)
  expect_in_sync(cd_subj)

  expect_error(filter_samples(cd, no_such_column < 24), "Error evaluating")
})

test_that("filter_samples() errors when results are stored", {
  expect_error(filter_samples(cd_jtk(), time < 24), "invalidate stored results")
})
