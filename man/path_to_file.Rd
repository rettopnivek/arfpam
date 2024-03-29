% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S03_Utilities.R
\name{path_to_file}
\alias{path_to_file}
\title{Returns File/Folder Paths}
\usage{
path_to_file(file_name = NULL, env_var = NULL, path = NULL, latest = TRUE)
}
\arguments{
\item{file_name}{A character string, a
partial match to the file of interest.}

\item{env_var}{A character string, the name for
the environment variable.}

\item{path}{A character string, a relative or
absolute path to a folder.}

\item{latest}{Logical; if \code{TRUE} returns only
the latest version of a file whose name contains
a date.}
}
\value{
A character string.
}
\description{
Returns an absolute file or folder path.
Folder paths can be extracted from a
pre-specified environmental variable.
}
