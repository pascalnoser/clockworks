#' Example rhythmic data
#'
#' These data sets are examples for what the `dataset` and `metadata` arguments
#' for the `clockworks` function could look like. They contain log-CPM values
#' (`cw_data`) or RNA-seq count data (`cw_data_counts`) for 10 genes. Of these,
#' the first two are rhythmic while the remaining eight are just noise. These
#' synthetic data sets contain repeated measures of four individuals (S1 - S4)
#' that are split into two groups ("A" and "B"), sampled over a span of 48 hours
#' at 2 hour intervals. Although the number of samples, features, and their
#' names are identical in the two data sets, they are not related (i.e.
#' `cw_data` is not a log-CPM version of `cw_data_counts`).
#'
#' @format ## `cw_data` A matrix with 10 rows and 96 columns containing log-CPM
#'   (normalised) data
"cw_data"

#' @rdname cw_data
#' @format ## `cw_data_counts`
#' A matrix with 10 rows and 96 columns containing RNA-seq count data
"cw_data_counts"

#' @rdname cw_data
#' @format ## `cw_metadata`
#' A data frame with 96 rows and 4 columns
#' \describe{
#'  \item{Sample_ID}{Sample IDs, correspond to the row names of `cw_data`}
#'  \item{Time}{Sample collection time}
#'  \item{Group}{Experimental group / condition}
#'  \item{Subject_ID}{Subject IDs to mark repeated measures}
#' }
"cw_metadata"
