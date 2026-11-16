# Helpers shared by all test files

# CircadianData object from the bundled example data
make_cd <- function(
  dataset = cw_data,
  metadata = cw_metadata,
  subject = TRUE,
  group = TRUE,
  ...
) {
  suppressMessages(CircadianData(
    dataset = dataset,
    metadata = metadata,
    colname_sample = "Sample_ID",
    colname_time = "Time",
    colname_group = if (group) "Group" else NULL,
    colname_subject = if (subject) "Subject_ID" else NULL,
    log_transformed = TRUE,
    period = 24,
    ...
  ))
}

# Synthetic data set without groups or subjects. `mat` is features x samples,
# `times` gives the time of each sample.
make_synth_cd <- function(mat, times, ...) {
  mat <- as.matrix(mat)
  if (is.null(rownames(mat))) {
    rownames(mat) <- paste0("F", seq_len(nrow(mat)))
  }
  colnames(mat) <- paste0("S", seq_along(times))
  meta <- data.frame(sample = colnames(mat), time = times)
  suppressMessages(CircadianData(
    dataset = mat,
    metadata = meta,
    colname_sample = "sample",
    colname_time = "time",
    ...
  ))
}

cosine <- function(t, mesor, amp, phase, per = 24) {
  mesor + amp * cos(2 * pi / per * (t - phase))
}

# Run a quick method once and reuse the result across tests
run_quiet <- function(cd, method, ...) {
  suppressWarnings(suppressMessages(clockworks(cd, method, ...)))
}

.cache <- new.env()
cd_jtk <- function() {
  if (is.null(.cache$cd_jtk)) {
    .cache$cd_jtk <- run_quiet(make_cd(), "JTK_CYCLE")
  }
  .cache$cd_jtk
}
