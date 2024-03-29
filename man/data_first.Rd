% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S03_Utilities.R
\name{data_first}
\alias{data_first}
\title{Adapt Functions to Take Data Argument First}
\usage{
data_first(data_obj, fun_to_apply, ...)
}
\arguments{
\item{fun_to_apply}{An R function that has a
\code{data} argument (e.g., \link[stats:lm]{stats::lm}).}

\item{...}{Additional arguments for the given function.}

\item{data_ob}{An R object to pass to a given
function's \code{data} argument.}
}
\value{
The associated output for the \code{fun_to_apply}
function.
}
\description{
A function that ensures the first argument is
always for 'data' - allows greater compatibility
with the pipe operator.
}
\examples{
data( 'mtcars' )
lm_fit <- data_first( mtcars, lm, formula = mpg ~ cyl )
print( lm_fit )

}
