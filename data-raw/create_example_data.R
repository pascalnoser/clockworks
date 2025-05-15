## code to prepare example data goes here

# Load example data and metadata from raw file
cw_data <- as.matrix(read.table("data-raw/data_example.txt.gz"))
cw_metadata <- read.csv("data-raw/metadata_example.txt.gz", sep = "\t")

# Make sure columns are in wanted order (required columns first)
cw_metadata <- cw_metadata[, c("Sample_ID", "Time", "Group", "Subject_ID")]


# Create example CD objects for internal use ----
## 1) 2 Groups, 2 replicates each, repeated measures ----
meta_full <- check_metadata(
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group",
  colname_subject = "Subject_ID"
)

cd_full <- CircadianData(cw_data, meta_full)
cd_full <- add_experiment_info(cd_full, period = 24, type = "norm")


## 2) No groups, 2 replicates, repeated measures ----
meta_rpl_rpt <- check_metadata(
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_subject = "Subject_ID"
)

cd_rpl_rpt <- CircadianData(cw_data, meta_rpl_rpt)

# Only keep the first two samples of each time point
sample_IDs <- grep("_S(1|2)", rownames(meta_rpl_rpt), value = TRUE)
cd_rpl_rpt <- cd_rpl_rpt[, sample_IDs]
cd_rpl_rpt <- add_experiment_info(cd_rpl_rpt, period = 24, type = "norm")


## 3) 2 Groups, no replicates, repeated measures ----
meta_grp_rpt <- check_metadata(
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group",
  colname_subject = "Subject_ID"
)

cd_grp_rpt <- CircadianData(cw_data, meta_grp_rpt)

# Only keep first sample of each time point and group
sample_IDs <- grep("_S(1|3)", rownames(meta_grp_rpt), value = TRUE)
cd_grp_rpt <- cd_grp_rpt[, sample_IDs]
cd_grp_rpt <- add_experiment_info(cd_grp_rpt, period = 24, type = "norm")


## 4) No Groups, no replicates, repeated measures ----
meta_rpt <- check_metadata(
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_subject = "Subject_ID"
)

cd_rpt <- CircadianData(cw_data, meta_rpt)

# Only keep first sample of each time point
sample_IDs <- grep("_S1", rownames(meta_rpt), value = TRUE)
cd_rpt <- cd_rpt[, sample_IDs]
cd_rpt <- add_experiment_info(cd_rpt, period = 24, type = "norm")


## 5) 2 Groups, 2 replicates each, no repeated measures ----
meta_grp_rpl <- check_metadata(
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group"
)

cd_grp_rpl <- CircadianData(cw_data, meta_grp_rpl)
cd_grp_rpl <- add_experiment_info(cd_grp_rpl, period = 24, type = "norm")


## 6) No groups, 2 replicates, no repeated measures ----
meta_rpl <- check_metadata(
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time"
)

cd_rpl <- CircadianData(cw_data, meta_rpl)

# Only keep the first two samples of each time point
sample_IDs <- grep("_S(1|2)", rownames(meta_rpl), value = TRUE)
cd_rpl <- cd_rpl[, sample_IDs]
cd_rpl <- add_experiment_info(cd_rpl, period = 24, type = "norm")


## 7) 2 groups, no replicates, no repeated measures ----
meta_grp <- check_metadata(
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group"
)

cd_grp <- CircadianData(cw_data, meta_grp)

# Only keep first sample of each time point and group
sample_IDs <- grep("_S(1|3)", rownames(meta_grp), value = TRUE)
cd_grp <- cd_grp[, sample_IDs]
cd_grp <- add_experiment_info(cd_grp, period = 24, type = "norm")


## 8) No groups, no replicates, no repeated measures ----
meta_min <- check_metadata(
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time"
)

cd_min <- CircadianData(cw_data, meta_min)

# Only keep first sample of each time point
sample_IDs <- grep("_S1", rownames(meta_min), value = TRUE)
cd_min <- cd_min[, sample_IDs]
cd_min <- add_experiment_info(cd_min, period = 24, type = "norm")



# Save objects ----
usethis::use_data(cw_data, overwrite = TRUE)
usethis::use_data(cw_metadata, overwrite = TRUE)
usethis::use_data(cd_full, cd_rpl_rpt, cd_grp_rpt, cd_rpt, cd_grp_rpl, cd_rpl, cd_grp, cd_min, overwrite = TRUE, internal = TRUE)
