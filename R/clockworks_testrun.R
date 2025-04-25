# # This short script is used to quickly test the clockworks function
# rm(list = ls(all = TRUE))
#
# method = "JTK_CYCLE"
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
# # No groups, 4 replicates, no repeated measures
# tst_replicates <- analyze_fn(cd_replicates)
#
# # 2 Groups, 2 replicates each, no repeated measures
# tst_grp_repl <- analyze_fn(cd_grp_repl)
#
# # No groups, 4 replicates, repeated measures
# tst_repeated <- analyze_fn(cd_repeated)
#
# # 2 Groups, 2 replicates each, repeated measures
# tst_full <- analyze_fn(cd_full)
#
