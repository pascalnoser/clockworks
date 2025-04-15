## code to prepare example data goes here

# Load example data and metadata from raw file
cw_data <- as.matrix(read.table("data-raw/data_example.txt.gz"))
cw_metadata <- read.csv("data-raw/metadata_example.txt.gz", sep = "\t")

# Make sure columns are in wanted order (required columns first)
cw_metadata <- cw_metadata[, c("Sample_ID", "Time", "Group", "Subject_ID")]

## Create example CD objects for internal use
# CD object with groups and repeated measures
meta_full <- check_metadata(
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group",
  colname_subject = "Subject_ID"
)

cd_full <- CircadianData(cw_data, meta_full)
cd_full <- add_CD_info(cd, period = 24)

# "Minimal" CD object with no groups and no repeated measures
meta_min <- check_metadata(
  metadata = cw_metadata[, c("Sample_ID", "Time")],
  colname_sample = "Sample_ID",
  colname_time = "Time"
)

cd_min <- CircadianData(cw_data, meta_min)
cd_min <- add_CD_info(cd_min, period = 24)

# CD object with groups but no repeated measures
meta_grp <- check_metadata(
  metadata = cw_metadata[, c("Sample_ID", "Time", "Group")],
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group"
)

cd_grp <- CircadianData(cw_data, meta_grp)
cd_grp <- add_CD_info(cd_min, period = 24)

# CD object no groups but repeated measures
meta_rep <- check_metadata(
  metadata = cw_metadata[, c("Sample_ID", "Time", "Subject_ID")],
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_subject = "Subject_ID"
)

cd_rep <- CircadianData(cw_data, meta_rep)
cd_rep <- add_CD_info(cd_min, period = 24)


# Save objects
usethis::use_data(cw_data, overwrite = TRUE)
usethis::use_data(cw_metadata, overwrite = TRUE)
usethis::use_data(cd_full, cd_min, cd_grp, cd_rep, overwrite = TRUE, internal = TRUE)
