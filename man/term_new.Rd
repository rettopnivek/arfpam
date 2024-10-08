% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S03_Utilities.R
\name{term_new}
\alias{term_new}
\title{Template for Creating New Term}
\usage{
term_new(
  label = "",
  coding = NULL,
  transformation = NULL,
  range = NULL,
  scale = NULL,
  order = NULL
)
}
\arguments{
\item{label}{An optional label for the new term.}

\item{coding}{A list with the parameters for
recoding the base variable (see \link{term_coding}).}

\item{transformation}{A character string with an
R expression for transforming the base variable.}

\item{range}{A vector with the range for the term.}

\item{scale}{Either a logical value, or a named
vector of the form \code{c(m = ..., sd = ...)}
for standardizing the term.}

\item{order}{A character vector that can be used
to specify the order in which coding (\code{'c'}),
transformations (\code{'t'}), and scaling (\code{'s'})
are done.}
}
\value{
A list.
}
\description{
Function to help format input to the
\link{term_prep} function. Initializes
the sublist with the parameters for creating
a new term based on an existing variable in a
data frame.
}
