#' Example rhythmic data
#'
#' This data set is an example for what the `dataset` and `metadata` arguments
#' for the `clockworks` function could look like. It contains log-CPM values for
#' 10 genes. Of these, the first two are rhythmic while the remaining eight are
#' just noise. This synthetic data set contains repeated measures of four
#' individuals (S1 - S4) that are split into two groups ("A" and "B"), sampled
#' over a span of 48 hours at 2 hour intervals.
#'
#' @format ## `cw_data`
#' A matrix with 10 rows and 96 columns
"cw_data"

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
