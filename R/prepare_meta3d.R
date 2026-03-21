#' meta3d prep
#'
#' This function is used to create the input required to run rhythmicity
#' detection with meta3d.
#'
#' @param cd A `CircadianData` object
#' @param cycMethodOne The method to use for rhythmicity detection in meta3d.
#'
#' @returns A list with inputs for `execute_meta3d()`
prepare_meta3d <- function(cd, cycMethodOne = "JTK") {
  # TODO: Set default method to the one that perfroms best in benchmarking
  # TODO: Set default method to the one that perfroms best in benchmarking
  # TODO: Set default method to the one that perfroms best in benchmarking
  # TODO: Set default method to the one that perfroms best in benchmarking

  # Define paths to temporary files for meta3d input and ouptput
  temp_file_data <- tempfile(pattern = "meta3d_input_data_", fileext = ".csv")
  temp_file_design <- tempfile(
    pattern = "meta3d_input_design_",
    fileext = ".csv"
  )
  temp_dir_out <- tempdir()

  # Get data and meta data from CD object
  dat <- dataset(cd)
  meta <- metadata(cd)

  # Add gene ID as a column to the data frame and ensure it's the first column
  dat <- cbind(GeneID = rownames(dat), as.data.frame(dat))

  # Add sample ID column to meta data and ensure consistent ordering of columns
  meta$SampleID <- rownames(meta)
  desired_cols <- c("SampleID", "time", "subject_ID", "group")
  present <- intersect(desired_cols, names(meta))
  meta <- meta[present]

  # Write to temporary files (explicitly specify parameters to ensure correct formatting)
  write.table(
    dat,
    temp_file_data,
    row.names = FALSE,
    quote = TRUE,
    sep = "\t",
    dec = "."
  )
  write.table(
    meta,
    temp_file_design,
    row.names = FALSE,
    quote = TRUE,
    sep = "\t",
    dec = "."
  )

  # Create list with inputs for run
  inputs <- list(
    datafile = temp_file_data,
    designfile = temp_file_design,
    filestyle = c("\t", "\"", "."), # Field separator, quote character, decimal point character
    cycMethodOne = cycMethodOne,
    outdir = temp_dir_out,
    minper = min(cd$period), # 20 is default
    maxper = max(cd$period), # 28 is default
    design_libColm = 1,
    design_hrColm = 2,
    design_subjectColm = 3,
    parallelize = FALSE, # TODO: Add option to parallelize and specify number of cores as an argument
    nCores = 1
  )

  # Add group column if present in meta data
  if ("group" %in% present) {
    inputs$design_groupColm <- 4
  }

  return(inputs)
}
