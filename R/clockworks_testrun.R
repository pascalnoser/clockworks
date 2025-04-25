# # This short script is used to quickly test the clockworks function
# rm(list = ls(all = TRUE))
#
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
#   method = "JTK_CYCLE"
# )
#
# lapply(res, head)
