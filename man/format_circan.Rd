% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_circan.R
\name{format_circan}
\alias{format_circan}
\title{Format CircaN Results}
\usage{
format_circan(ls_res_groups, ls_harm_groups, added_group)
}
\arguments{
\item{ls_res_groups}{A list with results from a CircaN rhythmicity
analysis, split into groups.}

\item{ls_harm_groups}{A list with results from a harmonic regression
analysis, split into groups.}

\item{added_group}{If TRUE the "group" column will be removed}
}
\value{
A list of data frames containing the original and formatted results
}
\description{
Format CircaN Results
}
