## code to prepare example data goes here

# Load example data and metadata from raw file
cw_data <- as.matrix(read.table("data-raw/data_example.txt.gz"))
cw_metadata <- read.csv("data-raw/metadata_example.txt.gz", sep = "\t")

# Make sure columns are in wanted order (required columns first)
cw_metadata <- cw_metadata[, c("Sample_ID", "Time", "Group", "Subject_ID")]



# Create example CD objects for internal use ----
## CD object with groups and repeated measures ----
meta_full <- check_metadata(
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group",
  colname_subject = "Subject_ID"
)

cd_full <- CircadianData(cw_data, meta_full)
cd_full <- add_experiment_info(cd_full, period = 24)


## "Minimal" CD object with no groups and no replicates ----
meta_min <- check_metadata(
  metadata = cw_metadata[, c("Sample_ID", "Time")],
  colname_sample = "Sample_ID",
  colname_time = "Time"
)

cd_min <- CircadianData(cw_data, meta_min)
cd_min <- add_experiment_info(cd_min, period = 24)

# Only keep first sample of each time point
sample_IDs <- grep("_S1", rownames(meta_min), value = TRUE)
cd_min <- cd_min[, sample_IDs]


## CD object with groups ----
meta_grp <- check_metadata(
  metadata = cw_metadata[, c("Sample_ID", "Time", "Group")],
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group"
)

cd_grp <- CircadianData(cw_data, meta_grp)
cd_grp <- add_experiment_info(cd_grp, period = 24)

# Only keep first sample of each time point and group
sample_IDs <- grep("_S(1|3)", rownames(meta_grp), value = TRUE)
cd_grp <- cd_grp[, sample_IDs]


## CD object with groups and replicates ----
meta_grp_repl <- check_metadata(
  metadata = cw_metadata[, c("Sample_ID", "Time", "Group")],
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group"
)

cd_grp_repl <- CircadianData(cw_data, meta_grp_repl)
cd_grp_repl <- add_experiment_info(cd_grp_repl, period = 24)


## CD object with replicates ----
meta_replicates <- check_metadata(
  metadata = cw_metadata[, c("Sample_ID", "Time", "Subject_ID")],
  colname_sample = "Sample_ID",
  colname_time = "Time"
)

cd_replicates <- CircadianData(cw_data, meta_replicates)
cd_replicates <- add_experiment_info(cd_replicates, period = 24)


## CD object with repeated measures ----
meta_repeated <- check_metadata(
  metadata = cw_metadata[, c("Sample_ID", "Time", "Subject_ID")],
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_subject = "Subject_ID"
)

cd_repeated <- CircadianData(cw_data, meta_repeated)
cd_repeated <- add_experiment_info(cd_repeated, period = 24)


# Save objects ----
usethis::use_data(cw_data, overwrite = TRUE)
usethis::use_data(cw_metadata, overwrite = TRUE)
usethis::use_data(cd_full, cd_min, cd_grp, cd_grp_repl, cd_repeated, cd_replicates, overwrite = TRUE, internal = TRUE)
