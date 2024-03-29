% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S08_Legacy.R
\name{fill_plot}
\alias{fill_plot}
\title{Add Filled Vertical/Horizontal Bar}
\usage{
fill_plot(x = NULL, y = NULL, l = NULL, border = NA, ...)
}
\arguments{
\item{x}{A vector of two x-axis coordinates,
or a N x 2 matrix, giving the left and right
boundaries of the vertical bars to draw.}

\item{y}{A vector of two y-axis coordinates,
or a N x 2 matrix, giving the bottom and
top boundaries of the horizontal bars to draw.}

\item{l}{The limits for either 1) the height
of the bar(s) (if \code{x} is not \code{NULL}),
or 2) the width of the bar(s) (if \code{y} is
not \code{NULL}).}

\item{border}{The color of the border for the
filled bar(s). If \code{NA}, no border is drawn.}

\item{...}{Additional parameters to the function
\code{\link[graphics]{polygon}}.}
}
\description{
Adds a vertical or horizontal filled bar
at the specified x or y-axis coordinates.
}
\examples{
# Create blank plot
xl <- c(0, 4)
yl <- c(0, 1)
plot_blank(xl, yl)
# Draw vertical grey bar
fill_plot(x = c(1, 2), l = yl, col = "grey90")
# Also set height and border of bar
fill_plot(x = c(3, 4), l = c(.25, .75), col = "grey80", border = "black")
# Draw horizontal bar
fill_plot(y = c(.9, 1), col = "black")

# Multiple bars can be draw at once
xl <- c(0, 4)
yl <- c(0, 1)
plot_blank(xl, yl)
# Multiple vertical bars
fill_plot(x = rbind(c(1, 2), c(3, 4)), l = yl, col = "grey90")
# Multiple horizontal bars
fill_plot(y = rbind(c(0, .05), c(.95, 1)), l = xl, col = "black")
}
