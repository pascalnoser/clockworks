% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze_circan.R
\name{analyze_circan}
\alias{analyze_circan}
\title{Detect rhythmicity with CircaN}
\usage{
analyze_circan(cd, method_args = list())
}
\arguments{
\item{cd}{A \code{CircadianData} object.}

\item{method_args}{Additional parameters passed to \code{CircaN::circan()}}
}
\value{
A data frame with the results of the CircaN analysis.
}
\description{
This function runs rhythmicity detection using CircaN.
}
\examples{
data(cw_data)
data(cw_metadata)
cw_metadata <- clockworks::check_metadata(
  cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group",
  colname_subject = "Subject_ID"
)
cd <- CircadianData(cw_data, cw_metadata)
cd <- clockworks:::add_experiment_info(cd, period = 24, data_type = "norm")
results <- clockworks:::analyze_circan(cd)
head(results$res_original)
}
