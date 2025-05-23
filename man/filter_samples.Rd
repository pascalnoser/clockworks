% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CircadianData.R
\name{filter_samples}
\alias{filter_samples}
\alias{filter_samples,CircadianData-method}
\title{Filter Samples by Matching Values in a Metadata Column}
\usage{
filter_samples(x, col, value, ...)

\S4method{filter_samples}{CircadianData}(x, col, value)
}
\arguments{
\item{x}{A \code{CircadianData} object.}

\item{col}{A single character string specifying the column name in
\code{metadata(x)} to filter by.}

\item{value}{A vector of values to match against in the specified column.
Samples where the column value is found in this vector will be kept.}

\item{...}{Additional arguments (currently unused).}
}
\value{
A new \code{CircadianData} object containing only the samples
that satisfy the filter condition.
}
\description{
Subsets a \code{CircadianData} object, keeping only samples where the value
in a specified metadata column matches one of the provided values.
}
\examples{
# Create minimal reproducible data
set.seed(789)
counts <- matrix(rpois(80, lambda = 50), nrow = 10, ncol = 8,
                 dimnames = list(paste0("Feature", 1:10), paste0("Sample", 1:8)))
meta <- data.frame(
  row.names = paste0("Sample", 1:8),
  time = rep(c(0, 6, 12, 18), each = 2),
  subject_id = paste0("S", rep(1:4, 2)),
  group = rep(c("Control", "Treated"), 4)
)
cd_obj <- CircadianData(counts, meta, experiment_info = list(period = 24))

# Filter samples belonging to the "Control" group
cd_control <- filter_samples(cd_obj, col = "group", value = "Control")
print(metadata(cd_control))

# Filter samples from subject S1 or S3
cd_s1_s3 <- filter_samples(cd_obj, col = "subject_id", value = c("S1", "S3"))
print(metadata(cd_s1_s3))

# Filter samples collected at time 0 or 6
cd_t0_t6 <- filter_samples(cd_obj, col = "time", value = c(0, 6))
print(metadata(cd_t0_t6))

}
