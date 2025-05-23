% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze_jtkcycle.R
\name{analyze_jtkcycle}
\alias{analyze_jtkcycle}
\title{Detect rhythmicity with JTK_CYCLE}
\usage{
analyze_jtkcycle(cd, method_args = list())
}
\arguments{
\item{cd}{A \code{CircadianData} object.}

\item{method_args}{Additional parameters passed to \verb{<method_function>}}
}
\value{
A data frame with the results of the JTK_CYCLE analysis.
}
\description{
This function runs rhythmicity detection JTK_CYCLE
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
results <- clockworks:::analyze_jtkcycle(cd)
head(results)
}
