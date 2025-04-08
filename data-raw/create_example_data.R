## code to prepare `DATASET` dataset goes here
dataset <- read.table("inst/extdata/SynthData_example.txt.gz")
dataset <- as.matrix(dataset)
meta <- read.csv("inst/extdata/SynthData_example_metadata.txt.gz", sep = "\t")
rownames(meta) <- meta$Sample_ID
meta$Sample_ID <- NULL

usethis::use_data(dataset, overwrite = TRUE, compress = "xz")
usethis::use_data(meta, overwrite = TRUE, compress = "xz")
