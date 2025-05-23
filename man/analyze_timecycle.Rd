% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze_timecycle.R
\name{analyze_timecycle}
\alias{analyze_timecycle}
\title{Detect rhythmicity with TimeCycle}
\usage{
analyze_timecycle(cd, method_args = list())
}
\arguments{
\item{cd}{A \code{CircadianData} object.}

\item{method_args}{Additional parameters passed to \verb{<TimeCycle::TimeCycle()>}}
}
\value{
A list with the original and formatted results of the TimeCycle
analysis.
}
\description{
This function runs rhythmicity detection with TimeCycle
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
# Set 'cores = 1' because CRAN limits available cores to 2
results <- clockworks:::analyze_timecycle(cd, method_args = list(cores = 1))
head(results)
}
