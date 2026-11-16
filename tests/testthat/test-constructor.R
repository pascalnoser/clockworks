small_mat <- function() {
  matrix(
    rnorm(16, 10),
    nrow = 2,
    dimnames = list(c("G1", "G2"), paste0("S", 1:8))
  )
}
small_meta <- function() {
  data.frame(sample = paste0("S", 1:8), time = seq(0, 21, by = 3))
}
construct <- function(dataset = small_mat(), metadata = small_meta(), ...) {
  suppressMessages(CircadianData(
    dataset = dataset,
    metadata = metadata,
    colname_sample = "sample",
    colname_time = "time",
    ...
  ))
}

test_that("valid input creates a CircadianData object", {
  cd <- construct()
  expect_s4_class(cd, "CircadianData")
  expect_equal(dim(cd), c(2L, 8L))
  expect_equal(colnames(get_dataset(cd)), rownames(get_metadata(cd)))
  expect_equal(get_experiment_info(cd)$period, 24)
})

test_that("bundled example data can be used", {
  cd <- make_cd()
  expect_equal(dim(cd), c(10L, 96L))
  expect_true(cd$repeated_measures)
  expect_equal(cd$n_groups, 2)
  expect_equal(cd$delta_t, 2)
  expect_setequal(names(get_experiment_info(cd)), c(
    "period", "data_type", "log_transformed", "log_base", "n_groups",
    "repeated_measures", "delta_t", "n_cycles", "missing_data_fraction",
    "n_replicates"
  ))
})

test_that("count data are accepted", {
  cd <- suppressMessages(CircadianData(
    cw_data_counts,
    cw_metadata,
    colname_sample = "Sample_ID",
    colname_time = "Time",
    colname_group = "Group",
    data_type = "count"
  ))
  expect_equal(cd$data_type, "count")
  expect_equal(nrow(get_wave_params(cd)), 2 * nrow(cw_data_counts))
})

test_that("non-logical log_transformed errors", {
  expect_error(construct(log_transformed = "yes"), "log_transformed")
  expect_error(construct(log_transformed = NA), "log_transformed")
  expect_error(construct(log_transformed = c(TRUE, FALSE)), "log_transformed")
})

test_that("NA or non-finite time values error", {
  meta <- small_meta()
  meta$time[3] <- NA
  expect_error(construct(metadata = meta), "missing or non-finite")
  meta$time[3] <- Inf
  expect_error(construct(metadata = meta), "missing or non-finite")
})

test_that("non-numeric time errors", {
  meta <- small_meta()
  meta$time <- as.character(meta$time)
  expect_error(construct(metadata = meta), "must be of type numeric")
})

test_that("duplicated sample columns in dataset error", {
  mat <- small_mat()
  colnames(mat)[8] <- "S1"
  expect_error(construct(dataset = mat), "must be unique.*S1")
})

test_that("duplicated sample IDs in metadata error", {
  meta <- small_meta()
  meta$sample[8] <- "S1"
  expect_error(construct(metadata = meta), "must be unique")
})

test_that("duplicated feature row names error", {
  mat <- small_mat()
  rownames(mat) <- c("G1", "G1")
  expect_error(construct(dataset = mat), "Row names \\(feature IDs\\).*G1")
})

test_that("sample ID mismatch between dataset and metadata errors", {
  mat <- small_mat()
  colnames(mat)[8] <- "S99"
  expect_error(construct(dataset = mat), "not found in the metadata: S99")

  meta <- small_meta()
  mat <- small_mat()[, 1:7]
  expect_error(
    construct(dataset = mat, metadata = meta),
    "not found in the dataset: S8"
  )
})

test_that("dataset without column names errors", {
  mat <- small_mat()
  colnames(mat) <- NULL
  expect_error(construct(dataset = mat), "column names")
})

test_that("missing metadata columns error", {
  expect_error(
    suppressMessages(CircadianData(
      small_mat(), small_meta(),
      colname_sample = "sample", colname_time = "zt"
    )),
    "missing from metadata"
  )
  expect_error(construct(colname_group = "condition"), "Missing column")
})

test_that("invalid period errors", {
  expect_error(construct(period = "24"), "numeric vector of length 1 or 2")
  expect_error(construct(period = c(20, 24, 28)), "numeric vector of length 1 or 2")
  expect_error(construct(period = Inf), "missing or infinite")
  expect_error(construct(period = c(20, NA)), "missing or infinite")
  expect_error(construct(period = 0), "greater than 0")
  expect_error(construct(period = -24), "greater than 0")
  expect_error(construct(period = c(28, 20)), "sorted")
  expect_error(construct(period = c(24, 24)), "sorted")
})

test_that("valid period ranges are accepted", {
  cd <- construct(period = c(20, 28))
  expect_equal(cd$period, c(20, 28))
  # The harmonic regression uses the mean of the range
  expect_equal(unique(get_wave_params(cd)$period), 24)
})

test_that("invalid data_type errors", {
  expect_error(construct(data_type = "tpm"), "data_type")
})
