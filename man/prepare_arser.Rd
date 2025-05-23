% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_arser.R
\name{prepare_arser}
\alias{prepare_arser}
\title{ARSER prep}
\usage{
prepare_arser(cd, grp)
}
\arguments{
\item{cd}{A \code{CircadianData} object}

\item{grp}{A string specifying a value in the "group" column of the metadata
slot of \code{cd} which is used for filtering.}
}
\value{
A list with inputs for \code{execute_arser()}
}
\description{
This function is used to create the input required to run rhythmicity
detection with ARSER.
}
