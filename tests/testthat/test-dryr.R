test_that("dryR skips batch correction for count data with repeated measures", {
  cd <- suppressMessages(CircadianData(
    dataset = cw_data_counts,
    metadata = cw_metadata,
    colname_sample = "Sample_ID",
    colname_time = "Time",
    colname_group = "Group",
    colname_subject = "Subject_ID",
    data_type = "count",
    period = 24
  ))
  expect_warning(
    check_dryr(cd, batch_correction = TRUE),
    "batch correction is skipped"
  )
  expect_no_warning(check_dryr(cd, batch_correction = FALSE))
})
