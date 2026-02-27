## code to prepare example data goes here

# Load example data and metadata from raw file
cw_data <- as.matrix(read.table("data-raw/data_example.txt.gz"))
cw_data_counts <- as.matrix(read.table("data-raw/data_example_counts.txt.gz"))
cw_metadata <- read.csv("data-raw/metadata_example.txt.gz", sep = "\t")

# Create example CD objects for internal use ----
## 1) 2 Groups, 2 replicates each, repeated measures ----
cd_full <- CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group",
  colname_subject = "Subject_ID",
  log_transformed = TRUE,
  log_base = 2
)

## 2) No groups, 2 replicates, repeated measures ----
cd_rpl_rpt <- CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_subject = "Subject_ID",
  log_transformed = TRUE,
  log_base = 2
)

# Only keep the first two samples of each time point
sample_IDs <- grep("_S(1|2)", colnames(cd_rpl_rpt), value = TRUE)
cd_rpl_rpt <- cd_rpl_rpt[, sample_IDs]

## 3) 2 Groups, no replicates, repeated measures ----
cd_grp_rpt <- CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group",
  colname_subject = "Subject_ID",
  log_transformed = TRUE,
  log_base = 2
)

# Only keep first sample of each time point and group
sample_IDs <- grep("_S(1|3)", colnames(cd_grp_rpt), value = TRUE)
cd_grp_rpt <- cd_grp_rpt[, sample_IDs]

## 4) No Groups, no replicates, repeated measures ----
cd_rpt <- CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_subject = "Subject_ID",
  log_transformed = TRUE,
  log_base = 2
)

# Only keep first sample of each time point
sample_IDs <- grep("_S1", colnames(cd_rpt), value = TRUE)
cd_rpt <- cd_rpt[, sample_IDs]

## 5) 2 Groups, 2 replicates each, no repeated measures ----
cd_grp_rpl <- CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group",
  log_transformed = TRUE,
  log_base = 2
)

## 6) No groups, 2 replicates, no repeated measures ----
cd_rpl <- CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  log_transformed = TRUE,
  log_base = 2
)

# Only keep the first two samples of each time point
sample_IDs <- grep("_S(1|2)", colnames(cd_rpl), value = TRUE)
cd_rpl <- cd_rpl[, sample_IDs]

## 7) 2 groups, no replicates, no repeated measures ----
cd_grp <- CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group",
  log_transformed = TRUE,
  log_base = 2
)

# Only keep first sample of each time point and group
sample_IDs <- grep("_S(1|3)", colnames(cd_grp), value = TRUE)
cd_grp <- cd_grp[, sample_IDs]

## 8) No groups, no replicates, no repeated measures ----
cd_min <- CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  log_transformed = TRUE,
  log_base = 2
)

# Only keep first sample of each time point
sample_IDs <- grep("_S1", colnames(cd_min), value = TRUE)
cd_min <- cd_min[, sample_IDs]

## 9) 2 Groups, 2 replicates each, repeated measures, count data ----
cd_full_counts <- CircadianData(
  dataset = cw_data_counts,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group",
  colname_subject = "Subject_ID",
  data_type = "count",
  filter_counts = TRUE
)

## 10) Groups and replicates, no repeated measures, added results ----
cd_results <- cd_grp_rpl

methods <- c(
  "ARSER",
  # "CircaN",
  "diffCircadian",
  "dryR",
  "GeneCycle",
  "JTK_CYCLE",
  # "RepeatedCircadian",
  "LimoRhyde",
  "LS",
  "meta2d",
  "RAIN",
  "TimeCycle"
)

for (i_method in methods) {
  cat("\n\nMethod:", i_method, "\n")
  cd_results <- clockworks(cd_results, method = i_method)
}


# Save objects ----
usethis::use_data(cw_data, overwrite = TRUE)
usethis::use_data(cw_data_counts, overwrite = TRUE)
usethis::use_data(cw_metadata, overwrite = TRUE)
usethis::use_data(
  cd_full,
  cd_rpl_rpt,
  cd_grp_rpt,
  cd_rpt,
  cd_grp_rpl,
  cd_rpl,
  cd_grp,
  cd_min,
  cd_full_counts,
  cd_results,
  overwrite = TRUE,
  internal = TRUE
)
