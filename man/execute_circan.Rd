% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/execute_circan.R
\name{execute_circan}
\alias{execute_circan}
\title{Run CircaN rhythmicity detection}
\usage{
execute_circan(inputs, grp, method_args = list())
}
\arguments{
\item{inputs}{Inputs for rhythmicity detection function generated by
\code{prepare_circan()}}

\item{grp}{Group that is analysed}

\item{method_args}{Additional parameters passed to \code{CircaN::circan()}}
}
\value{
A data frame
}
\description{
Run CircaN rhythmicity detection
}
