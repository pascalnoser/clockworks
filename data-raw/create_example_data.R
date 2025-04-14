## code to prepare example data goes here

# Load example data and metadata from raw file
cw_data <- as.matrix(read.table("data-raw/data_example.txt.gz"))
cw_metadata <- read.csv("data-raw/metadata_example.txt.gz", sep = "\t")

# Make sure columns are in wanted order (required columns first)
cw_metadata <- cw_metadata[, c("Sample_ID", "Time", "Group", "Subject_ID")]

## Create example CD object for internal use
# Prepare meta data
meta_modified <- check_metadata(
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group",
  colname_subject = "Subject_ID"
)

# Create CircadianData object
cd <- CircadianData(
  cw_data,
  meta_modified,
  experiment_info = list(
    period = 24,
    n_groups = length(unique(meta_modified[[".group"]])),
    repeated_measures = TRUE
  )
)

# Save objects
usethis::use_data(cw_data, overwrite = TRUE)
usethis::use_data(cw_metadata, overwrite = TRUE)
usethis::use_data(cd, overwrite = TRUE, internal = TRUE)
