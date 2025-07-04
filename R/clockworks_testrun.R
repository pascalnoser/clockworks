# # This short script is used to quickly test the clockworks function
#
# # LEAVE EVERYTHING COMMENTED OUT WHEN NOT TESTING IN ORDER TO PREVENT AN ERROR
# # UPON LOADING THE PACKAGE
#
# rm(list = ls(all = TRUE))
#
# method = "RepeatedCircadian"
#
#
# # Test function with all CD objects ----
# method_str <- gsub("[_-]", "", tolower(method))
# analyze_fn <- get((paste0("analyze_", method_str)), mode = "function")
#
# # No groups, no replicates, no repeated measures
# tst_min <- analyze_fn(cd_min, method_args = list())
#
# # 2 groups, no replicates, no repeated measures
# tst_grp <- analyze_fn(cd_grp, method_args = list())
#
# # No groups, 2 replicates, no repeated measures
# tst_rpl <- analyze_fn(cd_rpl, method_args = list())
#
# # 2 Groups, 2 replicates each, no repeated measures
# tst_grp_rpl <- analyze_fn(cd_grp_rpl, method_args = list())
#
# # No Groups, no replicates, repeated measures
# tst_rpt <- analyze_fn(cd_rpt, method_args = list())
#
# # 2 Groups, no replicates, repeated measures
# tst_grp_rpt <- analyze_fn(cd_grp_rpt, method_args = list())
#
# # No groups, 2 replicates, repeated measures
# tst_rpl_rpt <- analyze_fn(cd_rpl_rpt, method_args = list())
#
# # 2 Groups, 2 replicates each, repeated measures
# tst_full <- analyze_fn(cd_full, method_args = list())
#
#
# # Test `clockworks()` ----
# data("cw_data")
# data("cw_metadata")
#
# method = "LS"
#
# res <- clockworks(
#   dataset = cw_data,
#   metadata = cw_metadata,
#   colname_time = "Time",
#   colname_sample = "Sample_ID",
#   colname_group = "Group",
#   colname_subject = "Subject_ID",
#   period = 24,
#   method = method
# )
#
# lapply(res, head)
#
# # Test `clockworks()` on Simone's data ----
# sim_data = read.csv("data-raw/sim_data_transposed.txt", sep = "\t", check.names = FALSE)
# sim_meta <- read.csv("data-raw/sim_metadata.txt", sep = "\t")
# method = "TimeCycle"
#
# res <- clockworks(
#   dataset = sim_data,
#   metadata = sim_meta,
#   colname_time = "zt48",
#   colname_sample = "mice",
#   colname_group = NULL,
#   colname_subject = NULL,
#   period = 24,
#   method = method
# )
#
# res
