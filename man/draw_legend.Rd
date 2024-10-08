% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S04_Plotting.R
\name{draw_legend}
\alias{draw_legend}
\title{Add a Legend to an Existing Plot}
\usage{
draw_legend(x, y, legend, bty = "n", xpd = NA, adj = c(0, 1.2), ...)
}
\arguments{
\item{x}{The lower and upper boundaries for the
x-axis.}

\item{y}{The lower and upper boundaries for the
y-axis.}

\item{legend}{A character vector, the text for the
legend (see \link[graphics:legend]{graphics::legend}).}

\item{bty}{A character string, either \code{"o"} or
\code{"n"} (the default), the type of box to
draw around the legend (see \link[graphics:legend]{graphics::legend}).}

\item{xpd}{A logical value or \code{NA}, determining
how the legend should be clipped relative to the
figure region (see \link[graphics:par]{graphics::par}). Defaults to \code{NA}.}

\item{adj}{A numeric vector of two values, the
relative x and y-axis position of the legend,
where values are ratios (e.g., values of
1 will place the legend exactly at the top right
corner of the figure boundary).}

\item{...}{Additional arguments to pass to \link[graphics:legend]{graphics::legend}.}
}
\value{
Adds a legend to an existing plot.
}
\description{
Function to add a legend to an
existing plot.
}
\examples{
# Example figure
x <- 0:1; y <- 0:1
plot_blank(x, y)

# Add legend in middle of figure
draw_legend(
  x, y, 'Example',
  adj = c( .5, .5 )
)

}
