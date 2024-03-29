% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S03_Utilities.R
\name{date_and_time}
\alias{date_and_time}
\title{Formatted Date and Time for File Names}
\usage{
date_and_time(value = NULL, frmt = "\%Y_\%m_\%d-\%H_\%M")
}
\arguments{
\item{value}{A character string to convert
back into a date-time object.}

\item{frmt}{A character string specifying
the format for the date and time object
(see \code{\link[base:strptime]{as.POSIXct}}).}
}
\value{
Either 1) a character string with the
date and time, to include in a file name, or
2) a date-time object.
}
\description{
Convenience function to generate a
nicely formatted character string
with the date and time, typically of
the form: YYYY_MM_DD-HH_MM to include
as part of a file name. Can convert
the character string back into a
date and time if needed.
}
\examples{
string <- date_and_time()
string
# Convert back to date and time object
format( date_and_time( string ), '\%Y-\%m-\%d \%H:\%M' )

}
