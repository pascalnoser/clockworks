## code to prepare example data goes here

# Load example data and metadata from raw file
cw_data <- as.matrix(read.table("data-raw/data_example.txt.gz"))
cw_metadata <- read.csv("data-raw/metadata_example.txt.gz", sep = "\t")

# Make sure columns are in wanted order (required columns first)
cw_metadata <- cw_metadata[, c("Sample_ID", "Time", "Group", "Subject_ID")]


# Create example CD objects for internal use ----
## 1) 2 Groups, 2 replicates each, repeated measures ----
cd_full <- CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group",
  colname_subject = "Subject_ID"
)

cd_full <- add_experiment_info(
  cd_obj = cd_full,
  period = 24,
  data_type = "norm",
  log_transformed = TRUE,
  log_base = 2
)

cd_full <- estimate_wave_params(cd_full)


## 2) No groups, 2 replicates, repeated measures ----
cd_rpl_rpt <- CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_subject = "Subject_ID"
)

# Only keep the first two samples of each time point
sample_IDs <- grep("_S(1|2)", colnames(cd_rpl_rpt), value = TRUE)
cd_rpl_rpt <- cd_rpl_rpt[, sample_IDs]
cd_rpl_rpt <- add_experiment_info(
  cd_obj = cd_rpl_rpt,
  period = 24,
  data_type = "norm",
  log_transformed = TRUE,
  log_base = 2
)

cd_rpl_rpt <- estimate_wave_params(cd_rpl_rpt)


## 3) 2 Groups, no replicates, repeated measures ----
cd_grp_rpt <- CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group",
  colname_subject = "Subject_ID"
)

# Only keep first sample of each time point and group
sample_IDs <- grep("_S(1|3)", colnames(cd_grp_rpt), value = TRUE)
cd_grp_rpt <- cd_grp_rpt[, sample_IDs]
cd_grp_rpt <- add_experiment_info(
  cd_obj = cd_grp_rpt,
  period = 24,
  data_type = "norm",
  log_transformed = TRUE,
  log_base = 2
)

cd_grp_rpt <- estimate_wave_params(cd_grp_rpt)


## 4) No Groups, no replicates, repeated measures ----
cd_rpt <- CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_subject = "Subject_ID"
)

# Only keep first sample of each time point
sample_IDs <- grep("_S1", colnames(cd_rpt), value = TRUE)
cd_rpt <- cd_rpt[, sample_IDs]
cd_rpt <- add_experiment_info(
  cd_obj = cd_rpt,
  period = 24,
  data_type = "norm",
  log_transformed = TRUE,
  log_base = 2
)

cd_rpt <- estimate_wave_params(cd_rpt)


## 5) 2 Groups, 2 replicates each, no repeated measures ----
cd_grp_rpl <- CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group"
)

cd_grp_rpl <- add_experiment_info(
  cd_obj = cd_grp_rpl,
  period = 24,
  data_type = "norm",
  log_transformed = TRUE,
  log_base = 2
)

cd_grp_rpl <- estimate_wave_params(cd_grp_rpl)


## 6) No groups, 2 replicates, no repeated measures ----
cd_rpl <- CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time"
)

# Only keep the first two samples of each time point
sample_IDs <- grep("_S(1|2)", colnames(cd_rpl), value = TRUE)
cd_rpl <- cd_rpl[, sample_IDs]
cd_rpl <- add_experiment_info(
  cd_obj = cd_rpl,
  period = 24,
  data_type = "norm",
  log_transformed = TRUE,
  log_base = 2
)

cd_rpl <- estimate_wave_params(cd_rpl)


## 7) 2 groups, no replicates, no repeated measures ----
cd_grp <- CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group"
)

# Only keep first sample of each time point and group
sample_IDs <- grep("_S(1|3)", colnames(cd_grp), value = TRUE)
cd_grp <- cd_grp[, sample_IDs]
cd_grp <- add_experiment_info(
  cd_obj = cd_grp,
  period = 24,
  data_type = "norm",
  log_transformed = TRUE,
  log_base = 2
)

cd_grp <- estimate_wave_params(cd_grp)


## 8) No groups, no replicates, no repeated measures ----
cd_min <- CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time"
)

# Only keep first sample of each time point
sample_IDs <- grep("_S1", colnames(cd_min), value = TRUE)
cd_min <- cd_min[, sample_IDs]
cd_min <- add_experiment_info(
  cd_obj = cd_min,
  period = 24,
  data_type = "norm",
  log_transformed = TRUE,
  log_base = 2
)

cd_min <- estimate_wave_params(cd_min)


# Save objects ----
usethis::use_data(cw_data, overwrite = TRUE)
usethis::use_data(cw_metadata, overwrite = TRUE)
usethis::use_data(cd_full, cd_rpl_rpt, cd_grp_rpt, cd_rpt, cd_grp_rpl, cd_rpl, cd_grp, cd_min, overwrite = TRUE, internal = TRUE)
