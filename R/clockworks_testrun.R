# # This short script is used to quickly test the clockworks function
#
# # LEAVE EVERYTHING COMMENTED OUT WHEN NOT TESTING IN ORDER TO PREVENT AN ERROR
# # UPON LOADING THE PACKAGE
#
# rm(list = ls(all = TRUE))
#
# method = "LS"
#
#
# # Test function with all CD objects ----
# method_str <- gsub("[_-]", "", tolower(method))
# analyze_fn <- get((paste0("analyze_", method_str)), mode = "function")
#
# # No groups, no replicates, no repeated measures
# tst_min <- analyze_fn(cd_min)
#
# # 2 groups, no replicates, no repeated measures
# tst_grp <- analyze_fn(cd_grp)
#
# # No groups, 2 replicates, no repeated measures
# tst_rpl <- analyze_fn(cd_rpl)
#
# # 2 Groups, 2 replicates each, no repeated measures
# tst_grp_rpl <- analyze_fn(cd_grp_rpl)
#
# # No Groups, no replicates, repeated measures
# tst_rpt <- analyze_fn(cd_rpt)
#
# # 2 Groups, no replicates, repeated measures
# tst_grp_rpt <- analyze_fn(cd_grp_rpt)
#
# # No groups, 2 replicates, repeated measures
# tst_rpl_rpt <- analyze_fn(cd_rpl_rpt)
#
# # 2 Groups, 2 replicates each, repeated measures
# tst_full <- analyze_fn(cd_full)
#
#
# # Test `clockworks()` ----
# data("cw_data")
# data("cw_metadata")
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
