% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_wave_params.R
\name{estimate_wave_params}
\alias{estimate_wave_params}
\title{Harmonic regression}
\usage{
estimate_wave_params(cd, grp)
}
\arguments{
\item{cd}{A \code{CircadianData} object}

\item{grp}{A string specifying a value in the "group" column of the metadata
slot of \code{cd} which is used for filtering.}
}
\value{
A data frame with the results of a harmonic regression.
}
\description{
This function is used to get an estimate for the amplitude and phase using
harmonic regression.
}
