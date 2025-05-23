% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_extension.R
\name{get_extension}
\alias{get_extension}
\title{Get the extension of a file}
\usage{
get_extension(path)
}
\arguments{
\item{path}{Character vector with one element containing the path to a file.}
}
\value{
A character vector containing the file extension. The extensions
\code{.gz}, \code{.bz2}, \code{.xz}, and \code{.zip} will be ignored, so the returned value for
e.g. \code{myfile.txt.gz} would be \code{txt}.
}
\description{
Get the extension of a file
}
\examples{
x <- "path/to/my/file.txt"
get_extension(x)
}
