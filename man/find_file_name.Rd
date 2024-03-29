% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S03_Utilities.R
\name{find_file_name}
\alias{find_file_name}
\title{Check if a File Name can be Found}
\usage{
find_file_name(string, output = "name", full = FALSE, ...)
}
\arguments{
\item{string}{A character string, used
for partial string matching.}

\item{output}{The type of output to return.
Options include...
\itemize{
\item \code{'index'};
\item \code{'name'};
\item \code{'logical'};
\item \code{'vector'}.
}}

\item{full}{Logical; if \code{TRUE} returns the
full path.}

\item{...}{Additional arguments to the
\code{\link[base:list.files]{dir}} function.}
}
\value{
Either...
\itemize{
\item A single logical value, \code{TRUE} if
any matching file names are found
(\code{'logical'}).
\item A logical vector, \code{TRUE} for all
matching files in the vector of file
names (\code{'vector'}).
\item An integer vector giving the position of
any matches in the vector of file names
(\code{'index'}).
\item A character vector with any matching
file names, otherwise \code{NULL}
(\code{'name'}).
}
}
\description{
Checks if a file name can be found
in a folder via partial string matching.
Multiple types of output are supported.
}
\examples{
# Go to folder with html files for help pages
setwd(find.package("arfpam")[1])
setwd("html")

# Find help page for function 'every'
find_file_name("every")
find_file_name("every", output = "index")
find_file_name("every", output = "logical")
find_file_name("every", output = "vector")

}
