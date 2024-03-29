% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S08_Legacy.R
\name{blank_plot}
\alias{blank_plot}
\title{Generate a Blank Plot}
\usage{
blank_plot(xDim = c(0, 1), yDim = c(0, 1), margins = FALSE, cex = 1)
}
\arguments{
\item{xDim}{The lower and upper boundaries for the
x-axis.}

\item{yDim}{The lower and upper boundaries for the
y-axis.}

\item{margins}{Logical; if \code{TRUE} displays
guidelines and details regarding the figure
margins to aid in axis and legend specifications.}

\item{cex}{Text size to use when marking axis line
positions.}
}
\value{
An empty plot.
}
\description{
This function generates a completely blank plot.
}
\examples{
# With margin info
blank_plot(margins = TRUE)
}
